#!/usr/bin/python
# -*- coding: utf-8 -*
import time
import string
import random
import os
import shutil
import subprocess
import re
import json
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from util.selenium_tools import wait_for_element_exists, wait_for_element_exists_and_contains_expected_text, wait_for_an_element_with_partial_link_text_exists, verify_element_label, wait_for_element_exists_and_attribute_contains_expected_text
from util.execution import console_log
import settings


def random_email_addresses_generator(size=20):
    res = []
    for x in range(size):
        res.append(random_email_address_generator())
    return res


def random_email_address_generator():
    return random_generator() + "@mailinator.com"


def random_generator(size=20, chars=string.ascii_lowercase + string.digits):
    return ''.join(random.choice(chars) for x in range(size))


# Yield successive n-sized
# chunks from l.
def divide_chunks(l, n):
    # looping till length l
    for i in range(0, len(l), n):
        yield l[i:i + n]


def populate_credential_and_password_for_voters_from_sent_emails(fake_sent_emails_manager, voters_email_addresses, election_title):
    """
    Reads the file that gathers all sent emails to find, for each voter provided in array voters_email_addresses, their credential and their latest sent password. Returns an array, where each element is a dictionary with fields "email_address", "credential", "election_page_url", "username", and "password".
    :return: array
    """

    result = []
    sent_emails = fake_sent_emails_manager.separate_sent_emails()
    for voter_email_address in voters_email_addresses:
        # Step 1: Gather all emails that have been sent to this voter's email address
        emails_to_selected_voter = [x for x in sent_emails if x["to"] == voter_email_address]
        if len(emails_to_selected_voter) == 0:
            raise Exception("No sent email found to voter " + voter_email_address)

        # Step 2: Find email sent to this voter that contains credentials, and extract useful information (credential, election page URL)
        credential_email_subject_to_look_for = "Your credential for election " + election_title
        emails_with_credential = [x for x in emails_to_selected_voter if x["subject"] == credential_email_subject_to_look_for]
        if len(emails_with_credential) == 0:
            raise Exception("No credential email found for voter " + voter_email_address)
        email_with_credential = emails_with_credential[0]

        voter_credential = ""
        match = re.search(r'^Credential: (.*)$', email_with_credential["full_content"], re.MULTILINE)
        if match:
            voter_credential = match.group(1)
        else:
            raise Exception("Credential not found in credential email for voter " + voter_email_address)

        election_page_url = "" # In this scenario, it looks like all voters receive the same election page URL. Maybe in different scenarios, voters will not all receive the same vote URL (for the same election).
        match = re.search(r'^Page of the election: (.*)$', email_with_credential["full_content"], re.MULTILINE)
        if match:
            election_page_url = match.group(1)
        else:
            raise Exception("Election page URL not found in credential email for voter " + voter_email_address)

        # Step 3: Find email sent to this voter that contains their password for this election, and extract useful information (username, password)
        password_email_subject_to_look_for = "Your password for election " + election_title
        emails_with_password = [x for x in emails_to_selected_voter if x["subject"] == password_email_subject_to_look_for]
        if len(emails_with_password) == 0:
            raise Exception("Password email not found for voter " + voter_email_address)
        email_with_password = emails_with_password[-1] # We select the last password email received, because user's password may have been regenerated and sent several times

        voter_password = ""
        match = re.search(r'^Password: (.*)$', email_with_password["full_content"], re.MULTILINE)
        if match:
            voter_password = match.group(1)
        else:
            raise Exception("Password not found in password email for voter " + voter_email_address)

        voter_username = ""
        match = re.search(r'^Username: (.*)$', email_with_password["full_content"], re.MULTILINE)
        if match:
            voter_username = match.group(1)
        else:
            raise Exception("Username not found in password email for voter " + voter_email_address)

        # Step 4: Insert all extracted information into returned array
        element = {}
        element["email_address"] = voter_email_address
        element["credential"] = voter_credential
        element["election_page_url"] = election_page_url
        element["username"] = voter_username.replace("=40", "@") # Hack for encoding, until we manage encoding better
        element["password"] = voter_password
        result.append(element)
    return result


def populate_random_votes_for_voters(voters):
    for voter in voters:
        # Voter can't cast their vote when all choices are unselected (an alert shows, saying "You must select at least 1 answer(s)"). So there must be at least one checked answer.
        answer1 = random.choice([True, False])
        answer2 = random.choice([True, False])
        if not answer1 and not answer2:
            select_answer1 = random.choice([True, False])
            if select_answer1:
                answer1 = True
            else:
                answer2 = True

        voter.update({
            "votes": {
                "question1": {
                    "answer1": answer1,
                    "answer2": answer2
                }
            }
        })
    return voters


def repopulate_vote_confirmations_for_voters_from_sent_emails(fake_sent_emails_manager, voters_with_credentials, election_title):
    sent_emails = fake_sent_emails_manager.separate_sent_emails()
    for voter in voters_with_credentials:
        voter_email_address = voter["email_address"]
        # Step 1: Gather all emails that have been sent to this voter's email address
        emails_to_selected_voter = [x for x in sent_emails if x["to"] == voter_email_address]
        if len(emails_to_selected_voter) == 0:
            raise Exception("No sent email found to voter " + voter_email_address)

        # Step 2: Find email sent to this voter that contains vote confirmation, and extract useful information (smart ballot tracker)
        """
        The received email looks like this:

        Content-type: text/plain; charset="UTF-8"
        Content-transfer-encoding: quoted-printable
        From: Belenios public server <noreply@example.org>
        To: "A6QKLFSL0TTJ05XE2LHD@mailinator.com"
         <A6QKLFSL0TTJ05XE2LHD@mailinator.com>
        Subject: Your vote for election My test election for Scenario 1
        MIME-Version: 1.0
        X-Mailer: OcamlNet (ocamlnet.sourceforge.net)
        Date: Fri, 09 Nov 2018 21:40:39 +0100

        Dear A6QKLFSL0TTJ05XE2LHD=40mailinator.com,

        Your vote for election

          My test election for Scenario 1

        has been recorded. Your smart ballot tracker is

          jaSjEsICnqaVYcFIkfcdajCZbpwaR0QmHZouYUwabuc

        {This vote replaces any previous vote.}
        You can check its presence in the ballot box, accessible at
          http://localhost:8001/elections/imkV1i7hUR4dV3/ballots

        Results will be published on the election page
          http://localhost:8001/elections/imkV1i7hUR4dV3/

        --=20

        (Where {...} means this string appears only in some cases, namely only if this notification corresponds to a re-vote)
        """
        vote_confirmation_email_subject_to_look_for = "Your vote for election " + election_title
        emails_with_vote_confirmation = [x for x in emails_to_selected_voter if x["subject"] == vote_confirmation_email_subject_to_look_for]
        if len(emails_with_vote_confirmation) == 0:
            raise Exception("No vote confirmation email found for voter " + voter_email_address)
        email_with_vote_confirmation = emails_with_vote_confirmation[-1] # If voter received several vote confirmation emails (which happens when they revote), select the last one

        voter_smart_ballot_confirmation = ""
        match = re.search(r'Your smart ballot tracker is\s*(\S+)\s', email_with_vote_confirmation["full_content"], re.MULTILINE | re.DOTALL)
        if match:
            voter_smart_ballot_confirmation = match.group(1)
            voter_smart_ballot_confirmation = voter_smart_ballot_confirmation.strip()
        else:
            raise Exception("Smart ballot tracker not found in vote confirmation email for voter " + voter_email_address)
        voter["smart_ballot_tracker_in_vote_confirmation_email"] = voter_smart_ballot_confirmation
    return voters_with_credentials


def remove_database_folder():
    shutil.rmtree(os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, settings.DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY), ignore_errors=True)


def remove_election_from_database(election_id):
    shutil.rmtree(os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, settings.DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY, election_id), ignore_errors=True)


def remove_credentials_files(credential_file_id):
    if credential_file_id:
        for extension in [".privcreds", ".pubcreds"]:
            os.remove(credential_file_id + extension)


def wait_a_bit():
    if settings.WAIT_TIME_BETWEEN_EACH_STEP > 0:
        time.sleep(settings.WAIT_TIME_BETWEEN_EACH_STEP)


def build_css_selector_to_find_buttons_in_page_content_by_value(expected_value):
    return "#main input[value='" + expected_value + "']" # A more precise use case would be "#main form input[type=submit][value='...']"


def find_button_in_page_content_by_value(browser, expected_value):
    css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(expected_value)
    return browser.find_element_by_css_selector(css_selector)


def find_buttons_in_page_content_by_value(browser, expected_value):
    css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(expected_value)
    return browser.find_elements_by_css_selector(css_selector)


def initialize_server():
    server_path = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, settings.SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY)
    fake_sendmail_absolute_path = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, settings.FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY)
    custom_environment_variables = dict(os.environ, BELENIOS_SENDMAIL=fake_sendmail_absolute_path)
    server = subprocess.Popen([server_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, env=custom_environment_variables)
    try:
        out, err = server.communicate(timeout=1)
        raise Exception("Error while trying to run the Belenios server: " + err)
    except subprocess.TimeoutExpired: # Server process has not exited yet, so we suppose it is working correctly. For example: When port is already in use, server process exits quickly, with error details in its stderr
        console_log("Server process has not exited yet, so we suppose it is working correctly")
    return server


def initialize_browser(for_scenario_2=False):
    browser = None

    profile = webdriver.FirefoxProfile()
    profile.set_preference("intl.accept_languages", "en-us")

    if for_scenario_2:
        # Test Scenario 2 requires users to download things from their browser.
        # Define a custom profile for Firefox, to automatically download files that a page asks user to download, without asking. This is because Selenium can't control downloads.
        profile.set_preference('browser.download.folderList', 2) # Can be set to either 0, 1, or 2. When set to 0, Firefox will save all files downloaded via the browser on the user's desktop. When set to 1, these downloads are stored in the Downloads folder. When set to 2, the location specified for the most recent download is utilized again.
        profile.set_preference('browser.download.manager.showWhenStarting', False)
        profile.set_preference('browser.download.dir', settings.BROWSER_DOWNLOAD_FOLDER)
        mime_types_that_should_be_downloaded = ['text/plain', 'application/json']
        profile.set_preference('browser.helperApps.neverAsk.saveToDisk', ';'.join(mime_types_that_should_be_downloaded))

    if settings.USE_HEADLESS_BROWSER:
        from selenium.webdriver.firefox.options import Options
        options = Options()
        options.add_argument("--headless")
        options.log.level = "trace"
        browser = webdriver.Firefox(profile, options=options)
    else:
        browser = webdriver.Firefox(profile)
        # browser.maximize_window() # make the browser window use all available screen space. FIXME: When enabled, some clicks are not triggered anymore
    browser.implicitly_wait(settings.WAIT_TIME_BETWEEN_EACH_STEP) # In seconds
    return browser


def election_page_url_to_election_id(election_page_url):
    """
    From an election page URL like `http://localhost:8001/elections/JwCoBvR7thYcBG/`, we extract its UUID like `JwCoBvR7thYcBG`.
    """
    election_uuid = None
    match = re.search(r'/elections/(.+)/$', election_page_url)
    if match:
        election_uuid = match.group(1)
    else:
        raise Exception("Could not extract UUID from this election page URL: ", election_page_url)
    return election_uuid


def election_id_to_election_home_page_url(election_id):
    return "/".join([settings.SERVER_URL, "elections", election_id, ""])


def admin_election_draft_page_url_to_election_id(election_page_url):
    """
    From an election page URL like `http://localhost:8001/draft/credentials?token=k3GDN78v16etPW&uuid=3YbExvoPyAyujZ`, we extract its UUID like `3YbExvoPyAyujZ`.
    """
    election_uuid = None
    match = re.search(r'uuid=(.+)$', election_page_url)
    if match:
        election_uuid = match.group(1)
    else:
        raise Exception("Could not extract UUID from this election page URL: ", election_page_url)
    return election_uuid


def verify_election_consistency(election_id, snapshot_folder=None):
    """
    :param snapshot_folder: Optional parameter. If provided, it will verify consistency of differences (evolution) between this snapshot folder and current election database folder
    """

    election_folder = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, settings.DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY, election_id)
    verification_tool_path = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, "_run/tool-debug/bin/belenios-tool")
    command = [verification_tool_path, "verify"]
    if snapshot_folder:
        command = [verification_tool_path, "verify-diff", "--dir1=" + snapshot_folder, "--dir2=" + election_folder]
    running_process = subprocess.Popen(command, cwd=election_folder, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    process_timeout = 15 # seconds
    try:
        outs, errs = running_process.communicate(timeout=process_timeout) # It looks like all output of this program is in stderr
        match = re.search(r'^I: all (checks|tests) passed!?$', errs, re.MULTILINE)
        if match:
            console_log("Verification of election consistency has been correctly processed")
            assert match
        else:
            raise Exception("Error: Verification of election consistency is wrong. STDOUT was: " + outs + " STDERR was:" + errs)
    except subprocess.TimeoutExpired:
        running_process.kill()
        outs, errs = running_process.communicate()
        raise Exception("Error: Verification took longer than " + process_timeout + " seconds. STDOUT was: " + outs + " STDERR was:" + errs)


def belenios_tool_generate_credentials(election_id, number_of_voters=None, nh_question=False):
    """
    Use local CLI belenios-tool to generate a number of credentials corresponding to the number of voters. Example:
    ```
    ./_run/tool-debug/bin/belenios-tool credgen --uuid dmGuNVL1meanZt --group ./files/groups/default.json --count 5
    5 private credentials with ids saved to ./1579802689.privcreds
    5 public credentials saved to ./1579802689.pubcreds
    5 hashed public credentials with ids saved to ./1579802689.hashcreds
    ```
    """

    if not number_of_voters:
        number_of_voters = settings.NUMBER_OF_INVITED_VOTERS
    generated_files_destination_folder = settings.GENERATED_FILES_DESTINATION_FOLDER
    belenios_tool_path = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, "_run/tool-debug/bin/belenios-tool")
    if nh_question:
        group_base_path = "files/groups/rfc3526-2048.json"
    else:
        group_base_path = "files/groups/default.json"
    crypto_group_path = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, group_base_path)
    command = [belenios_tool_path, "credgen", "--uuid", election_id, "--group", crypto_group_path, "--count", str(number_of_voters)]
    running_process = subprocess.Popen(command, cwd=generated_files_destination_folder, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    process_timeout = 15 * number_of_voters # seconds
    credential_file_id = None
    try:
        outs, errs = running_process.communicate(timeout=process_timeout) # It looks like all output of this program is in stderr
        match = re.search(r'private credentials with ids saved to \./(.+)\.privcreds', outs, re.MULTILINE)
        if match:
            assert match
            console_log("Credentials have been generated successfully")
            credential_file_id = match.group(1)
        else:
            raise Exception("Error: Credentials generation went wrong. STDOUT was: " + outs + " STDERR was:" + errs)
    except subprocess.TimeoutExpired:
        running_process.kill()
        outs, errs = running_process.communicate()
        raise Exception("Error: Credentials generation took longer than " + process_timeout + " seconds. STDOUT was: " + outs + " STDERR was:" + errs)
    return os.path.join(generated_files_destination_folder, credential_file_id)


def belenios_tool_generate_ballots(voters_data, global_credential_file_id, vote_page_url):
    generated_files_destination_folder = settings.GENERATED_FILES_DESTINATION_FOLDER
    belenios_tool_path = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, "_run/tool-debug/bin/belenios-tool")

    i = 0
    for k, v in voters_data.items():
        i += 1
        # Extract voter private credential from global private credentials file (it corresponds to row `i` in the file) and write it to its own file
        voter_credential_file = os.path.join(generated_files_destination_folder, "voter_row_" + str(i) + "_privcred.txt")
        command = "tail -n +" + str(i) + " " + global_credential_file_id + ".privcreds | head -n 1 | cut -d' ' -f2 > " + voter_credential_file
        running_process = subprocess.Popen(command, cwd=generated_files_destination_folder, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, shell=True)
        process_timeout = 15 # seconds
        try:
            outs, errs = running_process.communicate(timeout=process_timeout)
        except subprocess.TimeoutExpired:
            running_process.kill()
            outs, errs = running_process.communicate()
            raise Exception("Error: Extraction of voter private credential from global private credentials file took longer than " + str(process_timeout) + " seconds. STDOUT was: " + outs + " STDERR was:" + errs)

        # Write array of voter's answers to questions in a file: This is his non-encrypted ballot, written as a JSON array where each element is the answer to the `i`th question. This answer is itself an array of zeros or ones depending on whether voter checked or not the checkbox corresponding to this answer.
        voter_uncrypted_ballot_file = os.path.join(generated_files_destination_folder, "voter_row_" + str(i) + "_uncrypted_ballot.json")
        voter_uncrypted_ballot_content = json.dumps(convert_voter_votes_to_json_uncrypted_ballot(v))
        console_log("voter_uncrypted_ballot_file:", voter_uncrypted_ballot_file)
        try:
            with open(voter_uncrypted_ballot_file, 'w') as myfile:
                myfile.write(voter_uncrypted_ballot_content)
        except Exception as e:
            raise Exception("Error: Could not write voter's answers (his uncrypted ballot) to a file.") from e

        # Execute belenios-tool to generate a vote ballot for voter
        voter_crypted_ballot_file = "voter_row_" + str(i) + "_crypted_ballot.json"
        command = [belenios_tool_path, "vote", "--url", vote_page_url, "--privcred", voter_credential_file, "--ballot", voter_uncrypted_ballot_file, ">", voter_crypted_ballot_file]
        running_process = subprocess.Popen(" ".join(command), cwd=generated_files_destination_folder, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, shell=True)
        process_timeout = 120 # seconds
        try:
            outs, errs = running_process.communicate(timeout=process_timeout)
        except subprocess.TimeoutExpired:
            running_process.kill()
            outs, errs = running_process.communicate()
            raise Exception("Error: Generation of voter's encrypted ballot file took longer than " + str(process_timeout) + " seconds. STDOUT was: " + outs + " STDERR was:" + errs)


def convert_voter_votes_to_json_uncrypted_ballot(voter):
    answer1 = 1 if voter["votes"]["question1"]["answer1"] is True else 0
    answer2 = 1 if voter["votes"]["question1"]["answer2"] is True else 0
    return [[answer1, answer2]]


def create_election_data_snapshot(election_id):
    election_folder = os.path.join(settings.GIT_REPOSITORY_ABSOLUTE_PATH, settings.DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY, election_id)
    process = subprocess.Popen(["mktemp", "-d"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    out, err = process.communicate(timeout=2)

    temporary_folder_absolute_path = None
    match = re.search(r'^\s*(\S+)\s*$', out)
    if match:
        temporary_folder_absolute_path = match.group(1)
    else:
        raise Exception("Could not extract absolute path from output of mktemp:", out)

    # Remark: If this command is run before any vote is cast, files `public_creds.txt` and `ballots.jsons` do not exist yet
    subprocess.run(["cp", "election.json", "public_creds.txt", "trustees.json", "ballots.jsons", temporary_folder_absolute_path], cwd=election_folder) # TODO: Execute a command that works on other OS, like `shutil.copy()`

    return temporary_folder_absolute_path


def delete_election_data_snapshot(snapshot_folder):
    subprocess.run(["rm", "-rf", snapshot_folder]) # TODO: Execute a command that works on other OS, like `shutil.rmtree()`


def accept_data_policy(browser):
    # If a personal data policy modal appears (it does not appear after it has been accepted), she clicks on the "Accept" button
    accept_button_label = "Accept"
    button_elements = find_buttons_in_page_content_by_value(browser, accept_button_label)
    if len(button_elements) > 0:
        assert len(button_elements) is 1
        button_elements[0].click()

def log_in_as_administrator(browser, from_a_login_page=False):
    if from_a_login_page:
        local_login_link_label = settings.LOGIN_MODE
        local_login_link_element = wait_for_an_element_with_partial_link_text_exists(browser, local_login_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
        local_login_link_element.click()
    else:
        # Alice has been given administrator rights on an online voting app called Belenios. She goes
        # to check out its homepage

        browser.get(settings.SERVER_URL)

        wait_a_bit()

        # She notices the page title mentions an election
        # TODO: Should we wait for the page to load here? It looks like we don't need to.
        assert 'Belenios' in browser.title, "Browser title was: " + browser.title

        # She clicks on "local" to go to the login page
        login_link_css_selector = "#login_" + settings.LOGIN_MODE
        login_element = wait_for_element_exists(browser, login_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        login_element.click()

    wait_a_bit()

    # She enters her identifier and password and submits the form to log in
    login_form_username_value = settings.ADMINISTRATOR_USERNAME
    login_form_password_value = settings.ADMINISTRATOR_PASSWORD

    login_form_username_css_selector = '#main form input[name=username]'
    login_form_password_css_selector = '#main form input[name=password]'

    login_form_username_element = wait_for_element_exists(browser, login_form_username_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
    login_form_password_element = wait_for_element_exists(browser, login_form_password_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)

    login_form_username_element.send_keys(login_form_username_value)
    login_form_password_element.send_keys(login_form_password_value)

    wait_a_bit()

    login_form_password_element.submit()

    # She verifies that she arrived on the administration page (instead of any login error page)

    # Here we use Selenium's Explicit Wait to wait for the h1 element of the page to contain expected text, meaning browser will have changed from login page to administration page. If we had used an Implicit Wait (with a defined duration) instead of an Explicit one, we risk to have some errors sometimes (we experienced them before doing this refactoring):
    # - Sometimes we get an error like `selenium.common.exceptions.StaleElementReferenceException: Message: The element reference of <h1> is stale; either the element is no longer attached to the DOM, it is not in the current frame context, or the document has been refreshed` or `selenium.common.exceptions.NoSuchElementException: Message: Unable to locate element: #header h1`. This is because page content changed in between two of our instructions.
    # - Value read from the page is still the value contained in previous page, because page content has not changed yet.

    page_title_css_selector = "#header h1"
    page_title_expected_content = "Administration"
    try:
        wait_for_element_exists_and_contains_expected_text(browser, page_title_css_selector, page_title_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)
    except:
        accept_data_policy(browser)
        wait_for_element_exists_and_contains_expected_text(browser, page_title_css_selector, page_title_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)


def election_home_find_start_button(browser):
    return wait_for_element_exists_and_attribute_contains_expected_text(browser, "#main button", "onclick", "location.href='../../vote.html#uuid=", settings.EXPLICIT_WAIT_TIMEOUT)


def log_out(browser, election_id=None):
    # In the header of the page, she clicks on the "Log out" link
    logout_link_css_selector = "#logout"
    logout_element = wait_for_element_exists(browser, logout_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
    logout_element.click()

    # She arrives on the election home page. She checks that the "Start" button is present

    # v1:
    # if election_id:
    #     verify_all_elements_have_attribute_value(browser, "#main button", "onclick", "location.href='../../vote.html#uuid=" + election_id + "';")
    # else:
    #     wait_for_element_exists_and_contains_expected_text(browser, "#main button", "Start", settings.EXPLICIT_WAIT_TIMEOUT) # This solution is less robust to variations in browser language settings

    # v2:
    election_home_find_start_button(browser)


def administrator_starts_creation_of_election(browser, manual_credential_management=False, election_title=None, election_description=None, initiator_contact=None):
    """
    Initial browser (required) state: administrator has just logged in
    Final browser state: on the "Preparation of election" page

    Alice, as an administrator, starts creation of the election:
    - She clicks on the "Prepare a new election" link
    - She picks the Credential management method she wants (function paramenter `manual_credential_management`)
    (- She keeps default value for Authentication method: it is Password, not CAS)
    - She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
    - In the "Name and description of the election" section, she changes values of fields name and description of the election
    - She clicks on the "Save changes button" (the one that is next to the election description field)
    - In "Contact" section, she changes the value of "contact" field
    - She clicks on the "Save changes" button (the one that is in the "Contact" section)
    """

    if election_title is None:
        election_title = settings.ELECTION_TITLE

    if election_description is None:
        election_description = settings.ELECTION_DESCRIPTION

    if initiator_contact is None:
        initiator_contact = settings.INITIATOR_CONTACT

    # She clicks on the "Prepare a new election" link
    create_election_link_expected_content = "Prepare a new election"
    links_css_selector = "#prepare_new_election"
    create_election_link_element = wait_for_element_exists_and_contains_expected_text(browser, links_css_selector, create_election_link_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)
    create_election_link_element.click()

    wait_a_bit()

    if manual_credential_management:
        # She selects the "Manual" radio button, under section "Credential management"
        manual_mode_radio_button_css_selector = "#main input[type=radio][value=manual]"
        manual_mode_radio_button_element = wait_for_element_exists(browser, manual_mode_radio_button_css_selector)
        manual_mode_radio_button_element.click()

    wait_a_bit()

    # She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
    proceed_button_css_selector = "#main form input[type=submit]"
    proceed_button_element = wait_for_element_exists(browser, proceed_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
    proceed_button_element.click()

    wait_a_bit()

    # In the "Name and description of the election" section, she changes values of fields name and description of the election
    election_name_field_css_selector = "#name_and_description_form input[name=__co_eliom_name]"
    election_name_field_element = wait_for_element_exists(browser, election_name_field_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
    election_name_field_value = election_title
    election_name_field_element.clear()
    election_name_field_element.send_keys(election_name_field_value)

    wait_a_bit()

    election_description_field_css_selector = "#name_and_description_form textarea[name=__co_eliom_description]"
    election_description_field_element = browser.find_element_by_css_selector(election_description_field_css_selector)
    election_description_field_value = election_description
    election_description_field_element.clear()
    election_description_field_element.send_keys(election_description_field_value)

    wait_a_bit()

    # She clicks on the "Save changes" button (the one that is next to the election description field)
    save_changes_button_css_selector = "#name_and_description_form input[type=submit]"
    save_changes_button_element = browser.find_element_by_css_selector(save_changes_button_css_selector)
    save_changes_button_element.click()

    wait_a_bit()

    # In "Contact" section, she changes the value of "contact" field
    election_contact_field_css_selector = "#form_contact input[name=__co_eliom_contact]"
    election_contact_field_element = browser.find_element_by_css_selector(election_contact_field_css_selector)
    election_contact_field_value = initiator_contact
    election_contact_field_element.clear()
    election_contact_field_element.send_keys(election_contact_field_value)

    wait_a_bit()

    # She clicks on the "Save changes" button (the one that is in the "Contact" section)
    contact_section_save_changes_button_css_selector = "#form_contact input[type=submit]"
    contact_section_save_changes_button_element = browser.find_element_by_css_selector(contact_section_save_changes_button_css_selector)
    contact_section_save_changes_button_element.click()

    wait_a_bit()

    # In "Public name of the administrator" section, she changes the value of "name" field
    admin_name_field_css_selector = "#form_admin_name input[name=__co_eliom_name]"
    admin_name_field_element = browser.find_element_by_css_selector(admin_name_field_css_selector)
    admin_name_field_value = "Election initiator"
    admin_name_field_element.clear()
    admin_name_field_element.send_keys(admin_name_field_value)

    wait_a_bit()

    # She clicks on the "Save changes" button (the one that is in the "Contact" section)
    admin_name_save_changes_button_css_selector = "#form_admin_name input[type=submit]"
    admin_name_save_changes_button_element = browser.find_element_by_css_selector(admin_name_save_changes_button_css_selector)
    admin_name_save_changes_button_element.click()

    wait_a_bit()


def administrator_edits_election_questions(browser, nh_question=False):
    """
    Initial browser (required) state: on the "Preparation of election" page, with questions not edited yet
    Final browser state: on the "Preparation of election" page (with questions edited)

    Alice, as an administrator who has recently started creating an election (election status is draft), configures its questions:
    - She clicks on the "Edit questions" link, to write her own questions
    - She arrives on the Questions page. She checks that the page title is correct
    - She removes answer 3
    - She clicks on the "Save changes" button (this redirects to the "Preparation of election" page)
    """

    # She clicks on the "Edit questions" link, to write her own questions
    edit_questions_link_css_selector = "#edit_questions"
    edit_questions_link_element = wait_for_element_exists(browser, edit_questions_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
    edit_questions_link_element.click()

    wait_a_bit()

    # She arrives on the Questions page. She checks that the page title is correct
    page_title_css_selector = "#header h1"
    page_title_expected_content = "Questions for"
    wait_for_element_exists_and_contains_expected_text(browser, page_title_css_selector, page_title_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)

    if nh_question:
        # She ticks "Tick the box to activate this mode."
        browser.find_element_by_css_selector("#hybrid_mode").click()
        # She ticks "Alternative"
        nhtally_css_selector = ".nonhomomorphic_tally"
        nhtally_checkbox_element = browser.find_element_by_css_selector(nhtally_css_selector)
        nhtally_checkbox_element.click()

    # She removes answer 3
    question_to_remove = 3
    remove_button_css_selector = ".question_answer_item:nth-child(" + str(question_to_remove) + ") .btn_remove"
    remove_button_element = browser.find_element_by_css_selector(remove_button_css_selector)
    remove_button_element.click()

    wait_a_bit()

    # She clicks on the "Save changes" button (this redirects to the "Preparation of election" page)
    save_changes_button_expected_label = "Save changes"
    button_elements = browser.find_elements_by_css_selector("button")
    assert len(button_elements)
    save_changes_button_element = button_elements[-1]
    verify_element_label(save_changes_button_element, save_changes_button_expected_label)
    save_changes_button_element.click()

    wait_a_bit()


def administrator_sets_election_voters(browser, voters_email_addresses):
    """
    Initial browser (required) state: on the "Preparation of election" page, with voters not set yet
    Final browser state: on the "Preparation of election" page (with voters set)

    :param voters_email_addresses: an array of voters' email addresses, for example generated using `random_email_addresses_generator()`

    Alice, as an administrator who has recently started creating an election (election status is draft), sets its voters:
    - She clicks on the "Edit voters" link, to then type the list of voters
    - She types N e-mail addresses (the list of invited voters)
    - She clicks on the "Add" button to submit changes
    - She clicks on "Go back to election draft" link
    """

    # She clicks on the "Edit voters" link, to then type the list of voters
    edit_voters_link_css_selector = "#edit_voters"
    edit_voters_link_element = wait_for_element_exists(browser, edit_voters_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
    edit_voters_link_element.click()

    wait_a_bit()

    # Split voters_email_addresses into batches of maximum 1000 elements (this is a limit imposed by Belenios UI)
    splitted_voters_email_addresses = list(divide_chunks(voters_email_addresses, 1000))

    for batch_of_email_addresses in splitted_voters_email_addresses:
        # She types N e-mail addresses (the list of invited voters)
        voters_list_field_css_selector = "#main form textarea"
        voters_list_field_element = wait_for_element_exists(browser, voters_list_field_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        voters_list_field_element.clear()
        last_email_address_typed = None
        is_first = True
        for email_address in batch_of_email_addresses:
            if is_first:
                is_first = False
            else:
                voters_list_field_element.send_keys(Keys.ENTER)
            voters_list_field_element.send_keys(email_address)
            last_email_address_typed = email_address

        wait_a_bit()

        # She clicks on the "Add" button to submit changes
        voters_list_field_element.submit()

        wait_a_bit()

        # She waits until the returned page displays the last email address typed
        if last_email_address_typed:
            expected_email_address_css_selector = "tr:last-child td:first-child"
            wait_for_element_exists_and_contains_expected_text(browser, expected_email_address_css_selector, last_email_address_typed)

    # She clicks on "Go back to election draft" link
    return_link_label = "Go back to election draft"
    return_link_element = wait_for_an_element_with_partial_link_text_exists(browser, return_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
    return_link_element.click()

    wait_a_bit()


def administrator_validates_creation_of_election(browser):
    """
    :return: election page URL

    Initial browser (required) state: on the "Preparation of election" page, with election not yet completely created
    Final browser state: on the "Preparation of election" page (with election completely created)

    Alice, as an administrator who has recently started creating an election (election status is draft), finalizes the creation of the election:
    - In "Validate creation" section, she clicks on the "Create election" link
    - (She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!")
    - In the "Validate creation" section, she clicks on the "Create election" button
    - (She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election")
    - She remembers the URL of the voting page, that is where the "Election home" link points to
    - She checks that a "Close election" button is present (but she does not click on it)
    """

    # In "Validate creation" section, she clicks on the "Create election" link
    create_election_link_label = "Create election"
    create_election_link_element = wait_for_an_element_with_partial_link_text_exists(browser, create_election_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
    create_election_link_element.click()

    wait_a_bit()

    # She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!"

    # She checks the presence of text "election ready"
    expected_confirmation_label = "election ready"
    expected_confirmation_css_selector = "#main"
    wait_for_element_exists_and_contains_expected_text(browser, expected_confirmation_css_selector, expected_confirmation_label)

    # In the "Validate creation" section, she clicks on the "Create election" button
    create_election_button_label = "Create election"
    create_election_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(create_election_button_label)
    create_election_button_element = wait_for_element_exists(browser, create_election_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
    create_election_button_element.click()

    wait_a_bit()

    # She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election"

    # She remembers the URL of the voting page, that is where the "Election home" link points to
    election_page_link_label = "Election home"
    election_page_link_element = wait_for_an_element_with_partial_link_text_exists(browser, election_page_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
    election_page_url = election_page_link_element.get_attribute('href')

    # She checks that a "Close election" button is present (but she does not click on it)
    close_election_button_label = "Close election"
    close_election_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(close_election_button_label)
    wait_for_element_exists(browser, close_election_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)

    return election_page_url
