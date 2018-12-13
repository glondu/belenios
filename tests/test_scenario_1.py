#!/usr/bin/python
# -*- coding: utf-8 -*
import unittest
import time
import string
import random
import os
import shutil
import subprocess
import re
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.alert import Alert
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.selenium_tools import wait_for_element_exists, wait_for_elements_exist, wait_for_element_exists_and_contains_expected_text, wait_for_element_exists_and_has_non_empty_content, wait_for_an_element_with_partial_link_text_exists


SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "demo/run-server.sh"
SERVER_URL = "http://localhost:8001"
DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY = "_run/spool"
FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "tests/tools/sendmail_fake.sh"
SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = "/tmp/sendmail_fake"
USE_HEADLESS_BROWSER = True # Set this to True if you run this test in Continuous Integration (it has no graphical display)
WAIT_TIME_BETWEEN_EACH_STEP = 0 # In seconds (float). Time we wait between each action that we tell Selenium driver to do in the browser. Set to 0 if you don't need to have the time to visually follow progress of actions in the browser
EXPLICIT_WAIT_TIMEOUT = 10 # In seconds. Maximum duration Selenium driver will wait for appearance of a specific DOM element expected in the page (for example when transitioning from a page to another). This referes to Selenium's "Explicit Wait" concept

NUMBER_OF_INVITED_VOTERS = 20 # This is N in description of Scenario 1. N is between 6 (quick test) and 1000 (load testing)
NUMBER_OF_VOTING_VOTERS = 10 # This is K in description of Scenario 1. K is between 6 (quick test) and 1000 (load testing). K <= N. (Some invited voters don't vote, this is abstention, and its value is N - K)
NUMBER_OF_REVOTING_VOTERS = 5 # This is L in description of Scenario 1. L <= K
NUMBER_OF_REGENERATED_PASSWORD_VOTERS = 4 # This is M in description of Scenario 1. M <= K
ELECTION_TITLE = "My test election for Scenario 1"
ADMINISTRATOR_USERNAME = "user1" # This value comes from file `demo/password_db.csv`, first row, first column
ADMINISTRATOR_PASSWORD = "phiexoey" # This value comes from file `demo/password_db.csv`, first row, 4th column

THIS_FILE_ABSOLUTE_PATH = os.path.abspath(__file__)
GIT_REPOSITORY_ABSOLUTE_PATH = os.path.dirname(os.path.dirname(THIS_FILE_ABSOLUTE_PATH))


def console_log(*args, **kwargs):
    print(*args, **kwargs, flush=True)


def random_email_addresses_generator(size=20):
    res = []
    for x in range(size):
        res.append(random_email_address_generator())
    return res


def random_email_address_generator():
    return random_generator() + "@mailinator.com"


def random_generator(size=20, chars=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(chars) for x in range(size))


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
    shutil.rmtree(os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY), ignore_errors=True)


def wait_a_bit():
    if WAIT_TIME_BETWEEN_EACH_STEP > 0:
        time.sleep(WAIT_TIME_BETWEEN_EACH_STEP)


def verify_element_label(element, expected_label):
    element_real_label = element.get_attribute('innerText')
    assert expected_label in element_real_label, 'Expected label "' + expected_label + '" not found in element label "' + element_real_label + "'"


def build_css_selector_to_find_buttons_in_page_content_by_value(expected_value):
    return "#main input[value='" + expected_value + "']" # A more precise use case would be "#main form input[type=submit][value='...']"


def find_button_in_page_content_by_value(browser, expected_value):
    css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(expected_value)
    return browser.find_element_by_css_selector(css_selector)


def find_buttons_in_page_content_by_value(browser, expected_value):
    css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(expected_value)
    return browser.find_elements_by_css_selector(css_selector)


def initialize_server():
    server_path = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY)
    fake_sendmail_absolute_path = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY)
    custom_environment_variables = dict(os.environ, BELENIOS_SENDMAIL=fake_sendmail_absolute_path)
    server = subprocess.Popen([server_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True, env=custom_environment_variables)
    try:
        out, err = server.communicate(timeout=1)
        raise Exception("Error while trying to run the Belenios server: " + err)
    except subprocess.TimeoutExpired: # Server process has not exited yet, so we suppose it is working correctly. For example: When port is already in use, server process exits quickly, with error details in its stderr
        console_log("Server process has not exited yet, so we suppose it is working correctly")
    return server


def initialize_browser():
    browser = None
    if USE_HEADLESS_BROWSER:
        from selenium.webdriver.firefox.options import Options
        options = Options()
        options.add_argument("--headless")
        options.log.level = "trace"
        browser = webdriver.Firefox(options=options)
    else:
        browser = webdriver.Firefox()
    browser.implicitly_wait(WAIT_TIME_BETWEEN_EACH_STEP) # In seconds
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


def verify_election_consistency(election_id, snapshot_folder=None):
    """
    :param snapshot_folder: Optional parameter. If provided, it will verify consistency of differences (evolution) between this snapshot folder and current election database folder
    """

    election_folder = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY, election_id)
    verification_tool_path = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, "_build/belenios-tool")
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


def create_election_data_snapshot(election_id):
    election_folder = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY, election_id)
    process = subprocess.Popen(["mktemp", "-d"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    out, err = process.communicate(timeout=2)

    temporary_folder_absolute_path = None
    match = re.search(r'^\s*(\S+)\s*$', out)
    if match:
        temporary_folder_absolute_path = match.group(1)
    else:
        raise Exception("Could not extract absolute path from output of mktemp:", out)

    # Remark: If this command is run before any vote is cast, files `public_creds.txt` and `ballots.jsons` do not exist yet
    subprocess.run(["cp", "election.json", "public_creds.txt", "public_keys.jsons", "ballots.jsons", temporary_folder_absolute_path], cwd=election_folder) # TODO: Execute a command that works on other OS, like `shutil.copy()`

    return temporary_folder_absolute_path


def delete_election_data_snapshot(snapshot_folder):
    subprocess.run(["rm", "-rf", snapshot_folder]) # TODO: Execute a command that works on other OS, like `shutil.rmtree()`


class BeleniosTestElectionScenario1(unittest.TestCase):
    """
    Properties:
    - server
    - browser
    - voters_email_addresses
    - voters_email_addresses_who_have_lost_their_password
    - voters_data
    - election_page_url
    - election_id
    """

    def setUp(self):
        self.fake_sent_emails_manager = FakeSentEmailsManager(SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
        self.fake_sent_emails_manager.install_fake_sendmail_log_file()

        remove_database_folder()

        self.server = initialize_server()

        self.browser = initialize_browser()

        self.voters_email_addresses = []
        self.voters_email_addresses_who_have_lost_their_password = []
        self.voters_data = dict()
        self.election_page_url = None
        self.election_id = None


    def tearDown(self):
        self.browser.quit()

        self.server.kill()

        remove_database_folder()

        self.fake_sent_emails_manager.uninstall_fake_sendmail_log_file()


    def update_voters_data(self, some_voters_data):
        """
        :param some_voters: a list of voter data
        """
        for voter in some_voters_data:
            self.voters_data[voter["email_address"]] = voter


    def log_in_as_administrator(self, from_a_login_page=False):
        browser = self.browser

        if from_a_login_page:
            local_login_link_label = "local"
            local_login_link_element = wait_for_an_element_with_partial_link_text_exists(browser, local_login_link_label, EXPLICIT_WAIT_TIMEOUT)
            local_login_link_element.click()
        else:
            # Edith has been given administrator rights on an online voting app called Belenios. She goes
            # to check out its homepage

            browser.get(SERVER_URL)

            wait_a_bit()

            # She notices the page title mentions an election
            # TODO: Should we wait for the page to load here? It looks like we don't need to.
            assert 'Election Server' in browser.title, "Browser title was: " + browser.title

            # If a personal data policy modal appears (it does not appear after it has been accepted), she clicks on the "Accept" button
            accept_button_label = "Accept"
            button_elements = find_buttons_in_page_content_by_value(browser, accept_button_label)
            if len(button_elements) > 0:
                assert len(button_elements) is 1
                button_elements[0].click()

            # She clicks on "local" to go to the login page
            login_link_css_selector = "#login_local"
            login_element = wait_for_element_exists(browser, login_link_css_selector, EXPLICIT_WAIT_TIMEOUT)
            login_element.click()

        wait_a_bit()

        # She enters her identifier and password and submits the form to log in
        login_form_username_value = ADMINISTRATOR_USERNAME
        login_form_password_value = ADMINISTRATOR_PASSWORD

        login_form_username_css_selector = '#main form input[name=username]'
        login_form_password_css_selector = '#main form input[name=password]'

        login_form_username_element = wait_for_element_exists(browser, login_form_username_css_selector, EXPLICIT_WAIT_TIMEOUT)
        login_form_password_element = wait_for_element_exists(browser, login_form_password_css_selector, EXPLICIT_WAIT_TIMEOUT)

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
        wait_for_element_exists_and_contains_expected_text(browser, page_title_css_selector, page_title_expected_content, EXPLICIT_WAIT_TIMEOUT)


    def log_out(self):
        browser = self.browser
        # In the header of the page, she clicks on the "Log out" link
        logout_link_css_selector = "#logout"
        logout_element = wait_for_element_exists(browser, logout_link_css_selector, EXPLICIT_WAIT_TIMEOUT)
        logout_element.click()

        # She arrives on the election home page. She checks that the "Start" button is present
        wait_for_element_exists_and_contains_expected_text(browser, "#main button", "Start", EXPLICIT_WAIT_TIMEOUT)


    def administrator_creates_election(self):
        # # Setting up a new election (action of the administrator)

        # Edith has been given administrator rights on an online voting app called Belenios. She goes
        # to check out its homepage and logs in
        self.log_in_as_administrator()

        # She clicks on the "Prepare a new election" link
        browser = self.browser
        create_election_link_expected_content = "Prepare a new election"
        links_css_selector = "#main a"
        create_election_link_element = wait_for_element_exists_and_contains_expected_text(browser, links_css_selector, create_election_link_expected_content, EXPLICIT_WAIT_TIMEOUT)
        create_election_link_element.click()

        wait_a_bit()

        # She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
        proceed_button_css_selector = "#main form input[type=submit]"
        proceed_button_element = wait_for_element_exists(browser, proceed_button_css_selector, EXPLICIT_WAIT_TIMEOUT)
        proceed_button_element.click()

        wait_a_bit()

        # She changes values of fields name and description of the election
        election_name_field_css_selector = "#main form input[name=__co_eliom_name]"
        election_name_field_element = wait_for_element_exists(browser, election_name_field_css_selector, EXPLICIT_WAIT_TIMEOUT)
        election_name_field_value = ELECTION_TITLE
        election_name_field_element.clear()
        election_name_field_element.send_keys(election_name_field_value)

        wait_a_bit()

        election_description_field_css_selector = "#main form textarea[name=__co_eliom_description]"
        election_description_field_element = browser.find_element_by_css_selector(election_description_field_css_selector)
        election_description_field_value = "This is the description of my test election for Scenario 1"
        election_description_field_element.clear()
        election_description_field_element.send_keys(election_description_field_value)

        wait_a_bit()

        # She clicks on the "Save changes button" (the one that is next to the election description field)
        save_changes_button_css_selector = "#main > div:nth-child(1) form input[type=submit]" # Warning: form:nth-child(1) selects another form
        save_changes_button_element = browser.find_element_by_css_selector(save_changes_button_css_selector)
        save_changes_button_element.click()

        wait_a_bit()

        # She clicks on the "Edit questions" link, to write her own questions
        edit_questions_link_css_selector = "#edit_questions"
        edit_questions_link_element = wait_for_element_exists(browser, edit_questions_link_css_selector, EXPLICIT_WAIT_TIMEOUT)
        edit_questions_link_element.click()

        wait_a_bit()

        # She arrives on the Questions page. She checks that the page title is correct
        page_title_css_selector = "#header h1"
        page_title_expected_content = "Questions for"
        wait_for_element_exists_and_contains_expected_text(browser, page_title_css_selector, page_title_expected_content, EXPLICIT_WAIT_TIMEOUT)

        # She removes answer 3
        question_to_remove = 3
        remove_button_css_selector = ".question_answers > div:nth-child(" + str(question_to_remove) + ") button:nth-child(2)"
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

        # She clicks on the "Edit voters" link, to then type the list of voters
        edit_voters_link_css_selector = "#edit_voters"
        edit_voters_link_element = wait_for_element_exists(browser, edit_voters_link_css_selector, EXPLICIT_WAIT_TIMEOUT)
        edit_voters_link_element.click()

        wait_a_bit()

        # She types N e-mail addresses (the list of invited voters)
        self.voters_email_addresses = random_email_addresses_generator(NUMBER_OF_INVITED_VOTERS)
        voters_list_field_css_selector = "#main form textarea"
        voters_list_field_element = wait_for_element_exists(browser, voters_list_field_css_selector, EXPLICIT_WAIT_TIMEOUT)
        voters_list_field_element.clear()
        is_first = True
        for email_address in self.voters_email_addresses:
            if is_first:
                is_first = False
            else:
                voters_list_field_element.send_keys(Keys.ENTER)
            voters_list_field_element.send_keys(email_address)

        wait_a_bit()

        # She clicks on the "Add" button to submit changes
        add_button_css_selector = "#main form input[type=submit]"
        add_button_element = browser.find_element_by_css_selector(add_button_css_selector)
        add_button_element.click()

        wait_a_bit()

        # She clicks on "Return to draft page" link
        return_link_label = "Return to draft page"
        return_link_element = wait_for_an_element_with_partial_link_text_exists(browser, return_link_label, EXPLICIT_WAIT_TIMEOUT)
        return_link_element.click()

        wait_a_bit()

        # She clicks on button "Generate on server"
        generate_on_server_button_label = "Generate on server"
        generate_on_server_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(generate_on_server_button_label)
        generate_on_server_button_element = wait_for_element_exists(browser, generate_on_server_button_css_selector, EXPLICIT_WAIT_TIMEOUT)
        generate_on_server_button_element.click()

        wait_a_bit()

        # (Server sends emails to voters.) She checks that server does not show any error that would happen when trying to send these emails (this can happen if sendmail is not configured)
        confirmation_sentence_expected_text = "Credentials have been generated and mailed!"
        confirmation_sentence_css_selector = "#main p"
        wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, EXPLICIT_WAIT_TIMEOUT)

        # Now we do a sanity check that server has really tried to send emails. For this, we look for email addresses in the temporary file where our fake sendmail executable redirects its inputs to.

        """
        An email sent by Belenios (using sendmail or using the fake sendmail) to a voter looks like this:

Content-type: text/plain; charset="UTF-8"
Content-transfer-encoding: quoted-printable
From: Belenios public server <noreply@example.org>
To: "820E7G83JBY0F4Z3DY2Y@mailinator.com"
 <820E7G83JBY0F4Z3DY2Y@mailinator.com>
Subject: Your credential for election My test election for Scenario 1
MIME-Version: 1.0
X-Mailer: OcamlNet (ocamlnet.sourceforge.net)
Date: Wed, 31 Oct 2018 15:22:27 +0100

You are listed as a voter for the election

  My test election for Scenario 1

You will find below your credential.  To cast a vote, you will also
need a password, sent in a separate email.  Be careful, passwords and
credentials look similar but play different roles.  You will be asked
to enter your credential before entering the voting booth.  Login and
passwords are required once your ballot is ready to be cast.

Credential: yQVDQaKSAQVjdZq
Page of the election: http://localhost:8001/elections/AFFNDEPnpy21bw/

Note that you are allowed to vote several times.  Only the last vote
counts.

----------

Vous =C3=AAtes enregistr=C3=A9(e) en tant qu=27=C3=A9lecteur(trice) pour=20=
l=27=C3=A9lection

  My test election for Scenario 1

Veuillez trouver ci-dessous votre code de vote. Pour soumettre un
bulletin, vous aurez =C3=A9galement besoin d=27un mot de passe, envoy=C3=A9=
 dans
un e-mail s=C3=A9par=C3=A9. Soyez attentif(ve), le mot de passe et le cod=
e de
vote se ressemblent mais jouent des r=C3=B4les diff=C3=A9rents. Le syst=C3=
=A8me vous
demandera votre code de vote d=C3=A8s l=27entr=C3=A9e dans l=27isoloir=20=
virtuel. Le
nom d=27utilisateur et le mot de passe sont n=C3=A9cessaires lorsque votr=
e
bulletin est pr=C3=AAt =C3=A0 =C3=AAtre soumis.

Code de vote=C2=A0: yQVDQaKSAQVjdZq
Page de l=27=C3=A9lection=C2=A0: http://localhost:8001/elections/AFFNDEPn=
py21bw/

Notez que vous pouvez voter plusieurs fois. Seul le dernier vote est
pris en compte.

--=20
        """

        email_address_to_look_for = self.voters_email_addresses[0]
        text_to_look_for = 'To: "' + email_address_to_look_for + '"'
        email_address_found = self.fake_sent_emails_manager.find_in_sent_emails(text_to_look_for)
        assert email_address_found, "Text '" + email_address_to_look_for + "'' not found in fake sendmail log file"


        # She clicks on the "Proceed" link
        proceed_link_expected_label = "Proceed"
        proceed_link_css_selector = "#main a"
        proceed_link_element = wait_for_element_exists_and_contains_expected_text(browser, proceed_link_css_selector, proceed_link_expected_label, EXPLICIT_WAIT_TIMEOUT)
        proceed_link_element.click()

        wait_a_bit()

        # In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
        generate_and_mail_missing_passwords_button_label = "Generate and mail missing passwords"
        generate_and_mail_missing_passwords_button_element = wait_for_element_exists(browser, build_css_selector_to_find_buttons_in_page_content_by_value(generate_and_mail_missing_passwords_button_label), EXPLICIT_WAIT_TIMEOUT)
        generate_and_mail_missing_passwords_button_element.click()

        wait_a_bit()

        # She checks that the page contains expected confirmation text, instead of an error (TODO: explain in which case an error can happen, and check that it does not show)
        confirmation_sentence_expected_text = "Passwords have been generated and mailed!"
        confirmation_sentence_css_selector = "#main p"
        wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, EXPLICIT_WAIT_TIMEOUT)

        # She clicks on the "Proceed" link
        proceed_link_expected_label = "Proceed"
        proceed_link_css_selector = "#main a"
        proceed_link_element = wait_for_element_exists_and_contains_expected_text(browser, proceed_link_css_selector, proceed_link_expected_label, EXPLICIT_WAIT_TIMEOUT)
        proceed_link_element.click()

        wait_a_bit()

        # In "Validate creation" section, she clicks on the "Create election" link
        create_election_link_label = "Create election"
        create_election_link_element = wait_for_an_element_with_partial_link_text_exists(browser, create_election_link_label, EXPLICIT_WAIT_TIMEOUT)
        create_election_link_element.click()

        wait_a_bit()

        # She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!"

        # In the "Validate creation" section, she clicks on the "Create election" button
        create_election_button_label = "Create election"
        create_election_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(create_election_button_label)
        create_election_button_element = wait_for_element_exists(browser, create_election_button_css_selector, EXPLICIT_WAIT_TIMEOUT)
        create_election_button_element.click()

        wait_a_bit()

        # She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election"

        # She remembers the URL of the voting page, that is where the "Election home" link points to
        election_page_link_label = "Election home"
        election_page_link_element = wait_for_an_element_with_partial_link_text_exists(browser, election_page_link_label, EXPLICIT_WAIT_TIMEOUT)
        self.election_page_url = election_page_link_element.get_attribute('href')
        console_log("election_page_url:", self.election_page_url)
        self.election_id = election_page_url_to_election_id(self.election_page_url)
        console_log("election_id:", self.election_id)

        # She checks that a "Close election" button is present (but she does not click on it)
        close_election_button_label = "Close election"
        close_election_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(close_election_button_label)
        wait_for_element_exists(browser, close_election_button_css_selector, EXPLICIT_WAIT_TIMEOUT)

        self.log_out()


    def administrator_regenerates_passwords_for_some_voters(self):
        # Edith has been contacted by some voters who say they lost their password. She wants to re-generate their passwords and have the platform send them by email. For this, she logs in as administrator.
        self.log_in_as_administrator()


        # She remembers the list of voters who contacted her and said they lost their password. For this, we pick NUMBER_OF_REGENERATED_PASSWORD_VOTERS voters from all the voters. TODO: We could pick them randomly or force an overlap with voters that are in NUMBER_OF_REVOTING_VOTERS
        browser = self.browser
        self.voters_email_addresses_who_have_lost_their_password = self.voters_email_addresses[0:NUMBER_OF_REGENERATED_PASSWORD_VOTERS]

        # She selects the election that she wants to edit
        election_to_edit_css_selector = "#main li a[href^='election/admin?uuid=']"
        election_to_edit_elements = wait_for_elements_exist(browser, election_to_edit_css_selector, EXPLICIT_WAIT_TIMEOUT)
        assert len(election_to_edit_elements) > 0
        election_to_edit_elements[0].click()

        wait_a_bit()

        # She arrives to the election administration page. For each voter of the NUMBER_OF_REGENERATED_PASSWORD_VOTERS selected voters:
        for email_address in self.voters_email_addresses_who_have_lost_their_password:
            # She clicks on the "Regenerate and mail a password" link
            regenerate_and_mail_a_password_link_css_selector = "#main a[href^='regenpwd?uuid=']"
            regenerate_and_mail_a_password_link_element = wait_for_element_exists(browser, regenerate_and_mail_a_password_link_css_selector, EXPLICIT_WAIT_TIMEOUT)
            regenerate_and_mail_a_password_link_element.click()

            wait_a_bit()

            # She types the e-mail address of the voter in the "Username" field
            username_field_css_selector = "#main input[type=text]"
            username_field_element = wait_for_element_exists(browser, username_field_css_selector, EXPLICIT_WAIT_TIMEOUT)
            username_field_element.send_keys(email_address)

            wait_a_bit()

            # She clicks on the "Submit" button
            submit_button_label = "Submit"
            submit_button_element = find_button_in_page_content_by_value(browser, submit_button_label)
            submit_button_element.click()

            wait_a_bit()

            # She checks that the page shows a confirmation message similar to "A new password has been mailed to RMR4MY4XV5GUDNOR6XNH@mailinator.com"
            confirmation_sentence_expected_text = "A new password has been mailed to"
            confirmation_sentence_css_selector = "#main p"
            wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, EXPLICIT_WAIT_TIMEOUT)

            # She clicks on the "Proceed" link
            proceed_link_expected_label = "Proceed"
            proceed_link_css_selector = "#main a"
            proceed_link_element = wait_for_element_exists_and_contains_expected_text(browser, proceed_link_css_selector, proceed_link_expected_label, EXPLICIT_WAIT_TIMEOUT)
            proceed_link_element.click()

            wait_a_bit()

            # She arrives back to the election administration page

        """
        Now we do a sanity check that server has really tried to send these emails. For this, we look for email addresses in the temporary file where our fake sendmail executable redirects its inputs to. There should be 3 occurences of "To : xxx@xxx" for users who have lost their password, with respective subjects:
        - "Your credential for election My test election for Scenario 1"
        - "Your password for election My test election for Scenario 1"
        - "Your password for election My test election for Scenario 1"

        And there should be only 2 occurences for other users, with respective subjects:
        - "Your credential for election My test election for Scenario 1"
        - "Your password for election My test election for Scenario 1"
        """

        for email_address in self.voters_email_addresses_who_have_lost_their_password:
            text_to_look_for = 'To: "' + email_address + '"'
            assert self.fake_sent_emails_manager.count_occurences_in_sent_emails(text_to_look_for) is 3

        voters_email_addresses_who_have_not_lost_their_password = set(self.voters_email_addresses) - set(self.voters_email_addresses_who_have_lost_their_password)

        for email_address in voters_email_addresses_who_have_not_lost_their_password:
            text_to_look_for = 'To: "' + email_address + '"'
            assert self.fake_sent_emails_manager.count_occurences_in_sent_emails(text_to_look_for) is 2

        self.log_out()



    def some_voters_cast_their_vote(self, voters):
        """
        :param voters: list of dict. Each element contains information about a voter (their e-mail address, the planned answers to each question they will cast)
        """
        browser = self.browser
        voters_count = len(voters)
        for index, voter in enumerate(voters):
            console_log("#### Current voter casting their vote in current batch: " + str(index + 1) + "/" + str(voters_count))
            # Bob has received 2 emails containing an invitation to vote and all necessary credentials (election page URL, username, password). He goes to the election page URL.
            browser.get(voter["election_page_url"])

            wait_a_bit()

            # He clicks on the "Start" button
            start_button_expected_label = "Start"
            start_button_css_selector = "#main button"
            start_button_element = wait_for_element_exists_and_contains_expected_text(browser, start_button_css_selector, start_button_expected_label, EXPLICIT_WAIT_TIMEOUT)
            start_button_element.click()

            wait_a_bit()

            # A loading screen appears, then another screen appears. He clicks on the "Here" button
            here_button_expected_label = "here"
            here_button_css_selector = "#input_code button"
            here_button_element = wait_for_element_exists_and_contains_expected_text(browser, here_button_css_selector, here_button_expected_label, EXPLICIT_WAIT_TIMEOUT)
            here_button_element.click()

            wait_a_bit()

            # A modal opens (it is an HTML modal created using Window.prompt()), with an input field. He types his credential.
            credential_prompt = Alert(browser)
            credential_prompt.send_keys(voter["credential"])
            credential_prompt.accept()

            wait_a_bit()

            # A new screen appears, which has a title "Step 2/6: Answer to questions", and a content:
            # "Question 1?"
            # "Question #1 of 1 — select between 1 and 2 answer(s)"
            # [ ] "Answer 1"
            # [ ] "Answer 2"
            # [Next]
            # (where "[ ]" is a checkbox, and [Next] is a button)

            # He fills his votes to each answer of the question
            answers_css_selector = "#question_div input[type=checkbox]"
            answers_elements = wait_for_elements_exist(browser, answers_css_selector, EXPLICIT_WAIT_TIMEOUT) # or we could use find_element_by_xpath("//div[@id='question_div']/input[@type='checkbox'][2]")

            assert len(answers_elements) is 2
            question1_answer1_element = answers_elements[0]
            question1_answer2_element = answers_elements[1]
            voter_vote_to_question_1_answer_1 = voter["votes"]["question1"]["answer1"]
            voter_vote_to_question_1_answer_2 = voter["votes"]["question1"]["answer2"]
            voter_vote_to_question_1_answer_1_is_checked = question1_answer1_element.get_attribute('checked')
            voter_vote_to_question_1_answer_2_is_checked = question1_answer2_element.get_attribute('checked')
            assert voter_vote_to_question_1_answer_1_is_checked is None
            assert voter_vote_to_question_1_answer_2_is_checked is None
            if voter_vote_to_question_1_answer_1:
                question1_answer1_element.click()
            if voter_vote_to_question_1_answer_2:
                question1_answer2_element.click()

            wait_a_bit()

            # He clicks on the "Next" button
            next_button_expected_label = "Next"
            next_button_css_selector = "#question_div button"
            next_button_element = wait_for_element_exists_and_contains_expected_text(browser, next_button_css_selector, next_button_expected_label, EXPLICIT_WAIT_TIMEOUT)
            next_button_element.click()

            wait_a_bit()

            """
            A new screen appears, showing:

            Step 3/6: Review and encrypt
            Question 1?
            - Answer 1

            Your ballot has been successfully encrypted, but has not been cast yet!

            Your smart ballot tracker is sLRilXoAYcodIrjWrOqPrVXLNlRyCJAqFeeHZ4WCajU

            We invite you to save it in order to check later that it is taken into account.

            [Continue]
            [Restart]
            """

            # He remembers the smart ballot tracker that is displayed.
            smart_ballot_tracker_css_selector = "#ballot_tracker"
            smart_ballot_tracker_element = wait_for_element_exists_and_has_non_empty_content(browser, smart_ballot_tracker_css_selector, EXPLICIT_WAIT_TIMEOUT)
            smart_ballot_tracker_value = smart_ballot_tracker_element.get_attribute('innerText')
            assert len(smart_ballot_tracker_value) > 5

            voter["smart_ballot_tracker"] = smart_ballot_tracker_value

            # He clicks on the "Continue" button
            next_button_expected_label = "Continue"
            next_button_css_selector = "#div_submit input[type=submit][value='" + next_button_expected_label + "']"
            next_button_element = browser.find_element_by_css_selector(next_button_css_selector)
            next_button_element.click()

            wait_a_bit()

            # He types his voter username and password, and submits the form
            username_field_css_selector = "#main input[name=username]"
            username_field_element = wait_for_element_exists(browser, username_field_css_selector, EXPLICIT_WAIT_TIMEOUT)
            username_field_element.send_keys(voter["username"])

            password_field_css_selector = "#main input[name=password]"
            password_field_element = browser.find_element_by_css_selector(password_field_css_selector)
            password_field_element.send_keys(voter["password"])

            wait_a_bit()

            password_field_element.submit()

            wait_a_bit()

            # He checks that the smart ballot tracker value that appears on screen is the same as the one he noted
            smart_ballot_tracker_verification_css_selector = "#ballot_tracker"
            smart_ballot_tracker_verification_element = wait_for_element_exists_and_has_non_empty_content(browser, smart_ballot_tracker_verification_css_selector, EXPLICIT_WAIT_TIMEOUT)
            smart_ballot_tracker_verification_value = smart_ballot_tracker_verification_element.get_attribute('innerText')
            assert len(smart_ballot_tracker_verification_value) > 5

            assert smart_ballot_tracker_verification_value == voter["smart_ballot_tracker"]

            # He clicks on the "I cast my vote" button
            submit_button_css_selector = "#main input[type=submit]"
            submit_button_element = browser.find_element_by_css_selector(submit_button_css_selector)
            submit_button_element.click()

            wait_a_bit()

            """
            Next screen looks like this:
            Your ballot for My test election for Scenario 1 has been accepted. Your smart ballot tracker is ISXe/rCNCVa9XcVeFgKglbpgo5SoZs4svT6dPbR5b6M. You can check its presence in the {ballot box} anytime during the election. A confirmation e-mail has been sent to you.

            {Go back to election}

            Where {xxx} is a links_elements
            """

            # He clicks on the "ballot box" link
            ballot_box_link_label = "ballot box"
            ballot_box_link_element = wait_for_an_element_with_partial_link_text_exists(browser, ballot_box_link_label, EXPLICIT_WAIT_TIMEOUT)
            ballot_box_link_element.click()

            wait_a_bit()

            # He checks that his smart ballot tracker appears in the list
            all_smart_ballot_trackers_css_selector = "#main ul li a"
            all_smart_ballot_trackers_elements = wait_for_elements_exist(browser, all_smart_ballot_trackers_css_selector, EXPLICIT_WAIT_TIMEOUT)
            assert len(all_smart_ballot_trackers_elements)
            matches = [element for element in all_smart_ballot_trackers_elements if element.get_attribute('innerText') == voter["smart_ballot_tracker"]]
            assert len(matches) is 1

            # In a following pass, he checks his mailbox to find a new email with confirmation of his vote, and verifies the value of the smart ballot tracker written in this email is the same as the one he noted. This verification is done in a separated pass because of an optimization, so that we only re-read and re-populate the sendmail_fake text file once for all users.

            # He closes the window (there is no log-out link, because user is not logged in: credentials are not remembered)
            # TODO: Is it really mandatory for the test to close the window? Re-opening a browser takes much more time, compared to just navigating to another URL.
            browser.quit()
            self.browser = initialize_browser()
            browser = self.browser

        # Start another pass, where we re-read and re-populate the sendmail_fake text file once for all users.
        voters = repopulate_vote_confirmations_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters, ELECTION_TITLE)
        for voter in voters:
            # He checks his mailbox to find a new email with confirmation of his vote, and verifies the value of the smart ballot tracker written in this email is the same as the one he noted.
            assert voter["smart_ballot_tracker"] == voter["smart_ballot_tracker_in_vote_confirmation_email"], "Ballot tracker read in vote confirmation email (" + voter["smart_ballot_tracker"] + ") is not the same as the one read on the vote confirmation page (" + voter["smart_ballot_tracker_in_vote_confirmation_email"] + ")"


    def all_voters_vote(self):
        voters_who_will_vote_now = self.voters_email_addresses[0:NUMBER_OF_VOTING_VOTERS] # TODO: "à faire avec K électeurs différents, avec au moins 3 sessions d'électeurs entrelacées"
        voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters_who_will_vote_now, ELECTION_TITLE)
        voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
        self.update_voters_data(voters_who_will_vote_now_data)
        self.some_voters_cast_their_vote(voters_who_will_vote_now_data)


    def all_voters_vote_in_sequences(self, verify_every_x_votes=5):
        """
        This function is an alias of some_voters_vote_in_sequences() using some default parameters, for readability
        """
        self.some_voters_vote_in_sequences(start_index=0, end_index=NUMBER_OF_VOTING_VOTERS, verify_every_x_votes=verify_every_x_votes)

    def some_voters_vote_in_sequences(self, start_index=0, end_index=NUMBER_OF_VOTING_VOTERS, verify_every_x_votes=5):
        if start_index < 0:
            raise Exception("start_index cannot be below 0")
        current_start_index = start_index
        if end_index > NUMBER_OF_VOTING_VOTERS:
            raise Exception("end_index cannot exceeed NUMBER_OF_VOTING_VOTERS")
        voters_who_will_vote_now = self.voters_email_addresses[start_index:end_index] # TODO: "à faire avec K électeurs différents, avec au moins 3 sessions d'électeurs entrelacées"
        voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters_who_will_vote_now, ELECTION_TITLE)
        voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
        self.update_voters_data(voters_who_will_vote_now_data)
        snapshot_folder = None

        while current_start_index < end_index:
            increment = verify_every_x_votes # could be randomized
            current_end_index = current_start_index + increment
            if current_end_index > end_index:
                current_end_index = end_index

            if current_start_index > 0:
                console_log("#### Starting substep: create_election_data_snapshot")
                snapshot_folder = create_election_data_snapshot(self.election_id)
                console_log("#### Substep complete: create_election_data_snapshot")

            try:
                console_log("#### A batch of " + str(current_end_index - current_start_index) + " voters, indexed " + str(current_start_index) + " to " + str(current_end_index - 1) + " are now going to vote")
                self.some_voters_cast_their_vote(voters_who_will_vote_now_data[current_start_index:current_end_index])
                console_log("#### A batch of " + str(current_end_index - current_start_index) + " voters, indexed " + str(current_start_index) + " to " + str(current_end_index - 1) + " have now voted")

                if current_start_index > 0:
                    console_log("#### Starting substep: verify_election_consistency using `belenios_tool verify-diff` (for a batch of votes)")
                    verify_election_consistency(self.election_id, snapshot_folder)
                    console_log("#### Substep complete: verify_election_consistency using `belenios_tool verify-diff` (for a batch of votes)")
            finally:
                if current_start_index > 0:
                    console_log("#### Starting substep: delete_election_data_snapshot")
                    delete_election_data_snapshot(snapshot_folder)
                    console_log("#### Substep complete: delete_election_data_snapshot")

            current_start_index += increment


    def some_voters_revote(self):
        voters_who_will_vote_now = self.voters_email_addresses[0:NUMBER_OF_REVOTING_VOTERS] # TODO: Should we pick these voters in a different way than as a sequential subset of initial set?
        voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters_who_will_vote_now, ELECTION_TITLE)
        voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
        self.update_voters_data(voters_who_will_vote_now_data)
        self.some_voters_cast_their_vote(voters_who_will_vote_now_data)


    def administrator_does_tallying_of_election(self):
        browser = self.browser

        # Alice goes to the election page
        election_url = self.election_page_url # Could also be obtained with self.voters_data[self.voters_email_addresses[0]]["election_page_url"]
        browser.get(election_url)

        wait_a_bit()

        # She clicks on the "Administer this election" link
        administration_link_label = "Administer this election"
        administration_link_element = wait_for_an_element_with_partial_link_text_exists(browser, administration_link_label, EXPLICIT_WAIT_TIMEOUT)
        administration_link_element.click()

        # She logs in as administrator
        self.log_in_as_administrator(from_a_login_page=True)

        wait_a_bit()

        # She clicks on the "Close election" button
        close_election_button_label = "Close election"
        close_election_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(close_election_button_label)
        close_election_button_element = wait_for_element_exists(browser, close_election_button_css_selector, EXPLICIT_WAIT_TIMEOUT)
        close_election_button_element.click()

        wait_a_bit()

        # She clicks on the "Proceed to vote counting" button
        proceed_button_label = "Proceed to vote counting"
        proceed_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(proceed_button_label)
        proceed_button_element = wait_for_element_exists(browser, proceed_button_css_selector, EXPLICIT_WAIT_TIMEOUT)
        proceed_button_element.click()

        wait_a_bit()

        # FIXME: If no voter has cast their vote, it shows a "Internal Server Error" "Error 500" page

        """
        She checks consistency of the vote result:
        - 1) She checks that the number of accepted ballots is the same as the number of voters who voted
        - 2) For each available answer in the question, she checks that the total number of votes in favor of Answer X displayed in result page is the same as the sum of votes for Answer X in all votes of voters who voted that have been randomly generated in advance
        - 3) She checks that each ballot content corresponds to content that of this vote that has been randomly generated in advance


        This screen looks like this:

        This is the development version!
        By using this site, you accept our <personal data policy>. <Accept>
        <en> <fr> <de> <ro> <it>

        This election has been tallied.

            Question 1?
            Answer 1    6
            Answer 2    8

        Number of accepted ballots: 10
        You can also download the <result with cryptographic proofs>.

        <See accepted ballots>

        Where <...> is a link
        """

        # - 1) She checks that the number of accepted ballots is the same as the number of voters who voted

        main_css_selector = "#main"
        main_expected_content = "Number of accepted ballots:"
        main_element = wait_for_element_exists_and_contains_expected_text(browser, main_css_selector, main_expected_content, EXPLICIT_WAIT_TIMEOUT)
        main_text_content = main_element.get_attribute('innerText')

        number_of_accepted_ballots = None
        match = re.search(r'Number of accepted ballots:\s*(\d+)\s', main_text_content, re.MULTILINE | re.DOTALL)
        if match:
            number_of_accepted_ballots = match.group(1)
            number_of_accepted_ballots = number_of_accepted_ballots.strip()
        else:
            raise Exception("Number of accepted ballots not found in election tally page: " + main_text_content)
        assert str(number_of_accepted_ballots) == str(NUMBER_OF_VOTING_VOTERS), "Number of accepted ballots (" + str(number_of_accepted_ballots) + ") is not the same as number of voters (" + str(NUMBER_OF_VOTING_VOTERS) + ")"


        # - 2) For each available answer in the question, she checks that the total number of votes in favor of Answer X displayed in result page is the same as the sum of votes for Answer X in all votes of voters who voted that have been randomly generated in advance
        # TODO

        # - 3) She checks that each ballot content corresponds to content that of this vote that has been randomly generated in advance
        # TODO


    def test_scenario_1_simple_vote(self):
        console_log("### Starting step: administrator_creates_election")
        self.administrator_creates_election()
        console_log("### Step complete: administrator_creates_election")

        console_log("### Starting step: administrator_regenerates_passwords_for_some_voters")
        self.administrator_regenerates_passwords_for_some_voters()
        console_log("### Step complete: administrator_regenerates_passwords_for_some_voters")

        console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (0)")
        verify_election_consistency(self.election_id)
        console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (0)")

        console_log("### Starting step: all_voters_vote_in_sequences")
        self.all_voters_vote_in_sequences()
        console_log("### Step complete: all_voters_vote_in_sequences")

        console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (1)")
        verify_election_consistency(self.election_id)
        console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (1)")

        console_log("### Starting step: create_election_data_snapshot (0)")
        snapshot_folder = create_election_data_snapshot(self.election_id)
        console_log("### Step complete: create_election_data_snapshot (0)")

        try:
            console_log("### Starting step: some_voters_revote")
            self.some_voters_revote()
            console_log("### Step complete: some_voters_revote")

            console_log("### Starting step: verify_election_consistency using `belenios_tool verify-diff` (0)")
            verify_election_consistency(self.election_id, snapshot_folder)
        finally:
            delete_election_data_snapshot(snapshot_folder)
        console_log("### Step complete: verify_election_consistency using `belenios_tool verify-diff` (0)")

        console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (2)")
        verify_election_consistency(self.election_id)
        console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (2)")

        console_log("### Starting step: administrator_does_tallying_of_election")
        self.administrator_does_tallying_of_election()
        console_log("### Step complete: administrator_does_tallying_of_election")

        console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (3)")
        verify_election_consistency(self.election_id)
        console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (3)")


if __name__ == "__main__":
    unittest.main()
