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
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.alert import Alert


SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "demo/run-server.sh"
SERVER_URL = "http://localhost:8001"
DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY = "_run/spool"
FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "tests/tools/sendmail_fake.sh"
SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = "/tmp/sendmail_fake"
USE_HEADLESS_BROWSER = True # Set this to True if you run this test in Continuous Integration (it has no graphical display)
WAIT_TIME_BETWEEN_EACH_STEP = 0.05 # In seconds (float)

NUMBER_OF_INVITED_VOTERS = 20 # This is N in description of Scenario 1. N is between 6 (quick test) and 1000 (load testing)
NUMBER_OF_VOTING_VOTERS = 10 # This is K in description of Scenario 1. K is between 6 (quick test) and 1000 (load testing). K <= N
NUMBER_OF_REVOTING_VOTERS = 5 # This is L in description of Scenario 1. L <= K
NUMBER_OF_REGENERATED_PASSWORD_VOTERS = 4 # This is M in description of Scenario 1. M <= K
ELECTION_TITLE = "My test election for Scenario 1"

THIS_FILE_ABSOLUTE_PATH = os.path.abspath(__file__)
GIT_REPOSITORY_ABSOLUTE_PATH = os.path.dirname(os.path.dirname(THIS_FILE_ABSOLUTE_PATH))


def random_email_addresses_generator(size=20):
    res = []
    for x in range(size):
        res.append(random_email_address_generator())
    return res


def random_email_address_generator():
    return random_generator() + "@mailinator.com"


def random_generator(size=20, chars=string.ascii_uppercase + string.digits):
    return ''.join(random.choice(chars) for x in range(size))


def find_in_sent_emails(text):
    with open(SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH) as fl:
        return text in fl.read()


def count_occurences_in_sent_emails(text):
    with open(SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH) as file:
        count = file.read().count(text)
    return count


"""
Converts the file that gathers all sent emails to an array with one element per sent email. Each element is a dictionary with fields "to", "subject", and "full_content".
:return: array
"""
def separate_sent_emails():
    # Email content is encoded using "quoted-printable" encoding. Please refer to https://en.wikipedia.org/wiki/Quoted-printable for more information. For example, this enconding transforms "@" into "=40". TODO: We could improve this function by having it directly decode the part of the email that is encoded, using `quopri` library for example.
    marker_for_end_of_email = "--=20"
    result = []
    with open(SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH) as file:
        contents = file.read()
        separated_emails = contents.split(marker_for_end_of_email)

        if len(separated_emails[-1]) < 5: # The last sent email ends with marker_for_end_of_email, so we can ignore what comes after
            separated_emails.pop()

        for email_full_content in separated_emails:
            email_to = ""
            match = re.search(r'^To: "(.*)"', email_full_content, re.MULTILINE)
            if match:
                email_to = match.group(1)

            email_subject = ""
            match = re.search(r'^Subject: (.*)$', email_full_content, re.MULTILINE)
            if match:
                email_subject = match.group(1)

            element = {
                "to": email_to,
                "subject": email_subject,
                "full_content": email_full_content
            }
            result.append(element)
    return result


"""
Reads the file that gathers all sent emails to find, for each voter provided in array voters_email_addresses, their credential and their latest sent password. Returns an array, where each element is a dictionary with fields "email_address", "credential", "election_page_url", "username", and "password".
:return: array
"""
def populate_credential_and_password_for_voters_from_sent_emails(voters_email_addresses, election_title):
    result = []
    sent_emails = separate_sent_emails()
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


def repopulate_vote_confirmations_for_voters_from_sent_emails(voters_with_credentials, election_title):
    sent_emails = separate_sent_emails()
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
    time.sleep(WAIT_TIME_BETWEEN_EACH_STEP)


def install_fake_sendmail_log_file():
    subprocess.run(["rm", "-f", SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH])
    subprocess.run(["touch", SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH])


def uninstall_fake_sendmail_log_file():
    subprocess.run(["rm", "-f", SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH])

def verify_element_label(element, expected_label):
    element_real_label = element.get_attribute('innerText')
    assert expected_label in element_real_label, 'Expected label "' + expected_label + '" not found in element label "' + element_real_label + "'"


def find_element_by_css_selector_and_label(browser, css_selector, expected_label=None):
    element = browser.find_element_by_css_selector(css_selector)
    if expected_label:
        verify_element_label(element, expected_label)
    return element


def find_button_in_page_content_by_value(browser, expected_value):
    css_selector = "#main input[value='" + expected_value + "']" # a more precise use case would be "#main form input[type=submit][value='...']"
    return browser.find_element_by_css_selector(css_selector)


def find_buttons_in_page_content_by_value(browser, expected_value):
    css_selector = "#main input[value='" + expected_value + "']" # a more precise use case would be "#main form input[type=submit][value='...']"
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
        print("Server process has not exited yet, so we suppose it is working correctly")
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


def verify_election_consistency(election_id):
    election_folder = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY, election_id)
    verification_tool_path = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, "_build/belenios-tool")
    running_process = subprocess.Popen([verification_tool_path, "verify"], cwd=election_folder, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    process_timeout = 15 # seconds
    try:
        outs, errs = running_process.communicate(timeout=process_timeout) # It looks like all output of this program is in stderr
        match = re.search(r'^I: all checks passed$', errs, re.MULTILINE)
        if match:
            print("Verification of election consistency has been correctly processed")
            assert match
        else:
            raise Exception ("Error: Verification of election consistency is wrong. STDOUT was: " + outs + " STDERR was:" + errs)
    except subprocess.TimeoutExpired:
        running_process.kill()
        outs, errs = proc.communicate()
        raise Exception ("Error: Verification took longer than " + process_timeout + " seconds. STDOUT was: " + outs + " STDERR was:" + errs)


class element_has_non_empty_content(object):
  """
  An expectation for checking that an element has a non-empty innerText attribute.
  This class is meant to be used in combination with Selenium's `WebDriverWait::until()`. For example:
  ```
  custom_wait = WebDriverWait(browser, 10)
  smart_ballot_tracker_element = custom_wait.until(element_has_non_empty_content((By.ID, "my_id")))
  ```

  :param locator: Selenium locator used to find the element. For example: `(By.ID, "my_id")`
  :return: The WebElement once it has a non-empty innerText attribute
  """
  def __init__(self, locator):
    self.locator = locator

  def __call__(self, driver):
    element = driver.find_element(*self.locator)   # Finding the referenced element
    if not element:
        return False
    element_content = element.get_attribute('innerText').strip()
    if len(element_content) > 0:
        return element
    else:
        return False


class element_exists_and_contains_expected_text(object):
  """
  An expectation for checking that an element exists and its innerText attribute contains expected text.
  This class is meant to be used in combination with Selenium's `WebDriverWait::until()`. For example:
  ```
  custom_wait = WebDriverWait(browser, 10)
  smart_ballot_tracker_element = custom_wait.until(element_has_non_empty_content((By.ID, "my_id"), "my expected text"))
  ```

  :param locator: Selenium locator used to find the element. For example: `(By.ID, "my_id")`
  :param expected_text: Text expected in element's innerText attribute (parameter type: string)
  :return: The WebElement once its innerText attribute contains expected_text
  """
  def __init__(self, locator, expected_text):
    self.locator = locator
    self.expected_text = expected_text

  def __call__(self, driver):
    element = driver.find_element(*self.locator)   # Finding the referenced element
    if not element:
        return False
    element_content = element.get_attribute('innerText').strip()
    if self.expected_text in element_content:
        return element
    else:
        return False


def wait_for_element_exists_and_contains_expected_text(browser, css_selector, expected_text, wait_duration=10):
    """
    Waits for the presence of an element that matches CSS selector `css_selector` and that has an innerText attribute that contains string `expected_text`.
    :param browser: Selenium browser
    :param css_selector: CSS selector of the expected element
    :param expected_text: String of the expected text that element must contain
    :param wait_duration: Maximum duration in seconds that we wait for the presence of this element before raising an exception
    :return: The WebElement once it matches expected conditions
    """
    try:
        ignored_exceptions=(NoSuchElementException,StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        page_title_element = custom_wait.until(element_exists_and_contains_expected_text((By.CSS_SELECTOR, css_selector), expected_text))
        return page_title_element
    except:
        raise Exception("Could not find expected DOM element '" + css_selector + "' with text content '" + expected_text + "' until timeout of " + str(wait_duration) + " seconds")


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
    install_fake_sendmail_log_file()

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

    uninstall_fake_sendmail_log_file()


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
        browser.find_element_by_partial_link_text(local_login_link_label).click()
    else:
        # Edith has been given administrator rights on an online voting app called Belenios. She goes
        # to check out its homepage
        
        browser.get(SERVER_URL)

        # She notices the page title mentions an election
        assert 'Election Server' in browser.title, "Browser title was: " + browser.title

        # If a personal data policy modal appears (it does not appear after it has been accepted), she clicks on the "Accept" button
        accept_button_label = "Accept"
        button_elements = find_buttons_in_page_content_by_value(browser, accept_button_label)
        if len(button_elements) > 0:
            assert len(button_elements) is 1
            button_elements[0].click()

        wait_a_bit()

        # She clicks on "local" to go to the login page
        login_link_id = "login_local"
        login_element = browser.find_element_by_id(login_link_id)
        login_element.click()

    # She enters her identifier and password and submits the form to log in
    login_form_username_value = "user1"
    login_form_password_value = "phiexoey" # This is the 4th column of file demo/password_db.csv

    login_form_username_css_selector = '#main form input[name=username]'
    login_form_password_css_selector = '#main form input[name=password]'

    login_form_username_element = browser.find_element_by_css_selector(login_form_username_css_selector)
    login_form_password_element = browser.find_element_by_css_selector(login_form_password_css_selector)

    login_form_username_element.send_keys(login_form_username_value)
    login_form_password_element.send_keys(login_form_password_value)
    login_form_password_element.submit()

    # She verifies that she arrived on the administration page (instead of any login error page)

    # Here we use Selenium's Explicit Wait to wait for the h1 element of the page to contain expected text, meaning browser will have changed from login page to administration page. If we had used an Implicit Wait (with a defined duration) instead of an Explicit one, we risk to have some errors sometimes (we experienced them before doing this refactoring):
    # - Sometimes we get an error like `selenium.common.exceptions.StaleElementReferenceException: Message: The element reference of <h1> is stale; either the element is no longer attached to the DOM, it is not in the current frame context, or the document has been refreshed` or `selenium.common.exceptions.NoSuchElementException: Message: Unable to locate element: #header h1`. This is because page content changed in between two of our instructions.
    # - Value read from the page is still the value contained in previous page, because page content has not changed yet.

    page_title_css_selector = "#header h1"
    page_title_expected_content = "Administration"
    page_title_element = wait_for_element_exists_and_contains_expected_text(browser, page_title_css_selector, page_title_expected_content)
    assert page_title_element


  def log_out(self):
    browser = self.browser
    # In the header of the page, she clicks on the "Log out" link
    logout_link_css_id = "logout"
    logout_element = browser.find_element_by_id(logout_link_css_id)
    logout_element.click()

    wait_a_bit()

    # She arrives back on the logged out home page. She checks that a login link is present
    login_link_id = "login_local"
    login_element = browser.find_element_by_id(login_link_id)


  def administrator_creates_election(self):
    # # Setting up a new election (action of the administrator)

    # Edith has been given administrator rights on an online voting app called Belenios. She goes
    # to check out its homepage and logs in
    self.log_in_as_administrator()

    # She clicks on the "Prepare a new election" link
    browser = self.browser
    create_election_link_text = "Prepare a new election"
    links_css_selector = "#main a"
    links_elements = browser.find_elements_by_css_selector(links_css_selector)
    assert len(links_elements)
    create_election_link_element = links_elements[0]
    verify_element_label(create_election_link_element, create_election_link_text)
    create_election_link_element.click()

    wait_a_bit()

    # She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
    proceed_button_css_selector = "#main form input[type=submit]"
    proceed_button_element = browser.find_element_by_css_selector(proceed_button_css_selector)
    proceed_button_element.click()

    wait_a_bit()

    # She changes values of fields name and description of the election
    election_name_field_css_selector = "#main form input[name=__co_eliom_name]"
    election_name_field_element = browser.find_element_by_css_selector(election_name_field_css_selector)
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
    edit_questions_link_element = browser.find_element_by_css_selector(edit_questions_link_css_selector)
    edit_questions_link_element.click()

    wait_a_bit()

    # She arrives on the Questions page. She checks that the page title is correct
    find_element_by_css_selector_and_label(browser, "#header h1", "Questions for")

    # She removes answer 3
    question_to_remove = 3
    remove_button_css_selector = ".question_answers > div:nth-child("+str(question_to_remove)+") button:nth-child(2)"
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
    edit_voters_link_element = browser.find_element_by_css_selector(edit_voters_link_css_selector)
    edit_voters_link_element.click()

    wait_a_bit()

    # She types N e-mail addresses (the list of invited voters)
    self.voters_email_addresses = random_email_addresses_generator(NUMBER_OF_INVITED_VOTERS)
    voters_list_field_css_selector = "#main form textarea"
    voters_list_field_element = browser.find_element_by_css_selector(voters_list_field_css_selector)
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
    browser.find_element_by_partial_link_text(return_link_label).click()

    wait_a_bit()

    # She clicks on button "Generate on server"
    generate_on_server_button_label = "Generate on server"
    generate_on_server_button_element = find_button_in_page_content_by_value(browser, generate_on_server_button_label)
    generate_on_server_button_element.click()

    wait_a_bit()

    # (Server sends emails to voters.) She checks that server does not show any error that would happen when trying to send these emails (this can happen if sendmail is not configured)
    confirmation_sentence_expected_text = "Credentials have been generated and mailed!"
    confirmation_sentence_css_selector = "#main p"
    confirmation_sentence_element = browser.find_element_by_css_selector(confirmation_sentence_css_selector)
    verify_element_label(confirmation_sentence_element, confirmation_sentence_expected_text)

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
    email_address_found = find_in_sent_emails(text_to_look_for)
    assert email_address_found, "Text '" + email_address_to_look_for + "'' not found in fake sendmail log file"


    # She clicks on the "Proceed" link
    proceed_link_expected_label = "Proceed"
    proceed_link_css_selector = "#main a"
    proceed_link_element = browser.find_element_by_css_selector(proceed_link_css_selector)
    verify_element_label(proceed_link_element, proceed_link_expected_label)
    proceed_link_element.click()

    wait_a_bit()

    # In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
    generate_and_mail_missing_passwords_button_label = "Generate and mail missing passwords"
    generate_and_mail_missing_passwords_button_element = find_button_in_page_content_by_value(browser, generate_and_mail_missing_passwords_button_label)
    generate_and_mail_missing_passwords_button_element.click()

    wait_a_bit()

    # She checks that the page contains expected confirmation text, instead of an error (TODO: explain in which case an error can happen, and check that it does not show)
    confirmation_sentence_expected_text = "Passwords have been generated and mailed!"
    confirmation_sentence_css_selector = "#main p"
    confirmation_sentence_element = browser.find_element_by_css_selector(confirmation_sentence_css_selector)
    verify_element_label(confirmation_sentence_element, confirmation_sentence_expected_text)

    # She clicks on the "Proceed" link
    proceed_link_expected_label = "Proceed"
    proceed_link_css_selector = "#main a"
    proceed_link_element = browser.find_element_by_css_selector(proceed_link_css_selector)
    verify_element_label(proceed_link_element, proceed_link_expected_label)
    proceed_link_element.click()

    wait_a_bit()

    # In "Validate creation" section, she clicks on the "Create election" link
    create_election_link_label = "Create election"
    browser.find_element_by_partial_link_text(create_election_link_label).click()

    wait_a_bit()

    # She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!"

    # In the "Validate creation" section, she clicks on the "Create election" button
    create_election_button_label = "Create election"
    create_election_button_element = find_button_in_page_content_by_value(browser, create_election_button_label)
    create_election_button_element.click()

    wait_a_bit()

    # She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election"

    # She remembers the URL of the voting page, that is where the "Election home" link points to
    election_page_link_label = "Election home"
    election_page_link_element = browser.find_element_by_partial_link_text(election_page_link_label)
    self.election_page_url = election_page_link_element.get_attribute('href')
    print("election_page_url:", self.election_page_url)
    self.election_id = election_page_url_to_election_id(self.election_page_url)
    print("election_id:", self.election_id)

    # She checks that a "Close election" button is present (but she does not click on it)
    close_election_button_label = "Close election"
    close_election_button_element = find_button_in_page_content_by_value(browser, close_election_button_label)

    self.log_out()


  def administrator_regenerates_passwords_for_some_voters(self):
    # Edith has been contacted by some voters who say they lost their password. She wants to re-generate their passwords and have the platform send them by email. For this, she logs in as administrator.
    self.log_in_as_administrator()
    

    # She remembers the list of voters who contacted her and said they lost their password. For this, we pick NUMBER_OF_REGENERATED_PASSWORD_VOTERS voters from all the voters. TODO: We could pick them randomly or force an overlap with voters that are in NUMBER_OF_REVOTING_VOTERS
    browser = self.browser
    self.voters_email_addresses_who_have_lost_their_password = self.voters_email_addresses[0:NUMBER_OF_REGENERATED_PASSWORD_VOTERS]

    # She selects the election that she wants to edit (li a[href^="election/admin?uuid="][0])
    election_to_edit_css_selector = "#main li a[href^='election/admin?uuid=']"
    election_to_edit_elements = browser.find_elements_by_css_selector(election_to_edit_css_selector)
    assert len(election_to_edit_elements) > 0
    election_to_edit_elements[0].click()

    wait_a_bit()

    # She arrives to the election administration page. For each voter of the NUMBER_OF_REGENERATED_PASSWORD_VOTERS selected voters:
    for email_address in self.voters_email_addresses_who_have_lost_their_password:
        # She clicks on the "Regenerate and mail a password" link (a[href^="regenpwd?uuid="][0])
        regenerate_and_mail_a_password_link_css_selector = "#main a[href^='regenpwd?uuid=']"
        regenerate_and_mail_a_password_link_element = browser.find_element_by_css_selector(regenerate_and_mail_a_password_link_css_selector)
        regenerate_and_mail_a_password_link_element.click()

        wait_a_bit()

        # She types the e-mail address of the voter in the "Username" field (input[type=text])
        username_field_css_selector = "#main input[type=text]"
        username_field_element = browser.find_element_by_css_selector(username_field_css_selector)
        username_field_element.send_keys(email_address)

        # She clicks on the "Submit" button
        submit_button_label = "Submit"
        submit_button_element = find_button_in_page_content_by_value(browser, submit_button_label)
        submit_button_element.click()

        wait_a_bit()

        # She checks that the page shows a confirmation message similar to "A new password has been mailed to RMR4MY4XV5GUDNOR6XNH@mailinator.com"
        confirmation_sentence_expected_text = "A new password has been mailed to"
        confirmation_sentence_css_selector = "#main p"
        confirmation_sentence_element = browser.find_element_by_css_selector(confirmation_sentence_css_selector)
        verify_element_label(confirmation_sentence_element, confirmation_sentence_expected_text)

        # She clicks on the "Proceed" link
        proceed_link_expected_label = "Proceed"
        proceed_link_css_selector = "#main a"
        proceed_link_element = browser.find_element_by_css_selector(proceed_link_css_selector)
        verify_element_label(proceed_link_element, proceed_link_expected_label)
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
        assert count_occurences_in_sent_emails(text_to_look_for) is 3

    voters_email_addresses_who_have_not_lost_their_password = set(self.voters_email_addresses) - set(self.voters_email_addresses_who_have_lost_their_password)

    for email_address in voters_email_addresses_who_have_not_lost_their_password:
        text_to_look_for = 'To: "' + email_address + '"'
        assert count_occurences_in_sent_emails(text_to_look_for) is 2

    self.log_out()


  """
  :param voters: list of dict. Each element contains information about a voter (their e-mail address, the planned answers to each question they will cast)
  """
  def voters_cast_their_vote(self, voters):
    browser = self.browser
    voters_count = len(voters)
    for index, voter in enumerate(voters):
        print("#### Current voter casting their vote: " + str(index+1) + "/" + str(voters_count))
        # Bob has received 2 emails containing an invitation to vote and all necessary credentials (election page URL, username, password). He goes to the election page URL.
        browser.get(voter["election_page_url"])

        wait_a_bit()

        # He clicks on the "Start" button
        start_button_expected_label = "Start"
        start_button_css_selector = "#main button"
        start_button_element = browser.find_element_by_css_selector(start_button_css_selector)
        verify_element_label(start_button_element, start_button_expected_label)
        start_button_element.click()

        wait_a_bit()

        # A loading screen appears, then another screen appears. He clicks on the "Here" button
        try:
            here_button_expected_label = "here"
            here_button_css_selector = "#input_code button"
            here_button_element = WebDriverWait(browser, 10).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, here_button_css_selector))
            )
            verify_element_label(here_button_element, here_button_expected_label)
            here_button_element.click()
        except:
            raise Exception("Could not find expected DOM element until timeout")

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
        answers_elements = browser.find_elements_by_css_selector(answers_css_selector) # or we could use find_element_by_xpath("//div[@id='question_div']/input[@type='checkbox'][2]")
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
        next_button_element = browser.find_element_by_css_selector(next_button_css_selector)
        verify_element_label(next_button_element, next_button_expected_label)
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
        smart_ballot_tracker_css_id = "ballot_tracker"
        custom_wait = WebDriverWait(browser, 10)
        smart_ballot_tracker_element = custom_wait.until(element_has_non_empty_content((By.ID, smart_ballot_tracker_css_id)))
        smart_ballot_tracker_value = smart_ballot_tracker_element.get_attribute('innerText')
        assert len(smart_ballot_tracker_value) > 5 # TODO: Should we verify more accurately its value for coherence? What rules should we use?

        voter["smart_ballot_tracker"] = smart_ballot_tracker_value

        # He clicks on the "Continue" button
        next_button_expected_label = "Continue"
        next_button_css_selector = "#div_submit input[type=submit][value='" + next_button_expected_label + "']"
        next_button_element = browser.find_element_by_css_selector(next_button_css_selector)
        next_button_element.click()

        wait_a_bit()

        # He types his voter username and password, and submits the form
        username_field_css_selector = "#main input[name=username]"
        username_field_element = browser.find_element_by_css_selector(username_field_css_selector)
        username_field_element.send_keys(voter["username"])

        password_field_css_selector = "#main input[name=password]"
        password_field_element = browser.find_element_by_css_selector(password_field_css_selector)
        password_field_element.send_keys(voter["password"])

        password_field_element.submit()

        wait_a_bit()

        # He checks that the smart ballot tracker value that appears on screen is the same as the one he noted
        ballot_tracker_dom_id = "ballot_tracker"
        ballot_tracker_element = browser.find_element_by_id(ballot_tracker_dom_id)
        ballot_tracker_element_value = ballot_tracker_element.get_attribute('innerText')
        assert len(ballot_tracker_element_value) > 5
        assert ballot_tracker_element_value == voter["smart_ballot_tracker"]

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
        browser.find_element_by_partial_link_text(ballot_box_link_label).click()

        # He checks that his smart ballot tracker appears in the list
        all_smart_ballot_trackers_css_selector = "#main ul li a"
        all_smart_ballot_trackers_elements = browser.find_elements_by_css_selector(all_smart_ballot_trackers_css_selector)
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
    voters = repopulate_vote_confirmations_for_voters_from_sent_emails(voters, ELECTION_TITLE)
    for voter in voters:
        # He checks his mailbox to find a new email with confirmation of his vote, and verifies the value of the smart ballot tracker written in this email is the same as the one he noted.
        assert voter["smart_ballot_tracker"] == voter["smart_ballot_tracker_in_vote_confirmation_email"], "Ballot tracker read in vote confirmation email (" + voter["smart_ballot_tracker"] + ") is not the same as the one read on the vote confirmation page (" + voter["smart_ballot_tracker_in_vote_confirmation_email"] + ")"


  def all_voters_vote(self):
    voters_who_will_vote_now = self.voters_email_addresses[0:NUMBER_OF_VOTING_VOTERS] # TODO: "à faire avec K électeurs différents, avec au moins 3 sessions d'électeurs entrelacées"
    # TODO: Should we also handle a case where not all invited voters vote? (abstention)
    voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(voters_who_will_vote_now, ELECTION_TITLE)
    voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
    self.update_voters_data(voters_who_will_vote_now_data)
    self.voters_cast_their_vote(voters_who_will_vote_now_data)


  def some_voters_revote(self):
    voters_who_will_vote_now = self.voters_email_addresses[0:NUMBER_OF_REVOTING_VOTERS] # TODO: Should we pick these voters in a different way than as a sequential subset of initial set?
    voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(voters_who_will_vote_now, ELECTION_TITLE)
    voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
    self.update_voters_data(voters_who_will_vote_now_data)
    self.voters_cast_their_vote(voters_who_will_vote_now_data)


  def administrator_does_tallying_of_election(self):
    browser = self.browser

    # Alice goes to the election page
    election_url = self.election_page_url #self.voters_data[self.voters_email_addresses[0]]["election_page_url"]
    browser.get(election_url)

    wait_a_bit()

    # She clicks on the "Administer this election" link
    administration_link_label = "Administer this election"
    administration_link_element = browser.find_element_by_partial_link_text(administration_link_label)
    administration_link_element.click()

    # She logs in as administrator
    self.log_in_as_administrator(from_a_login_page=True)

    wait_a_bit()

    # She clicks on the "Close election" button
    close_election_button_label = "Close election"
    close_election_button_element = find_button_in_page_content_by_value(browser, close_election_button_label)
    close_election_button_element.click()

    wait_a_bit()

    # She clicks on the "Proceed to vote counting" button
    proceed_button_label = "Proceed to vote counting"
    proceed_button_element = find_button_in_page_content_by_value(browser, proceed_button_label)
    proceed_button_element.click()

    wait_a_bit()

    # FIXME: If no voter has cast their vote, it shows a "Internal Server Error" "Error 500" page 

    # She checks consistency of the result # FIXME: Does this mean that we check compared to what we know our fake voters have voted? Or only that the number of accepted ballots is the same as the number of voters who voted? For now, we choose the second option.

    """
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

    main_css_id = "main"
    main_element = browser.find_element_by_id(main_css_id)
    main_text_content = main_element.get_attribute('innerText')

    number_of_accepted_ballots = None
    match = re.search(r'Number of accepted ballots:\s*(\d+)\s', main_text_content, re.MULTILINE | re.DOTALL)
    if match:
        number_of_accepted_ballots = match.group(1)
        number_of_accepted_ballots = number_of_accepted_ballots.strip()
    else:
        raise Exception("Number of accepted ballots not found in election tally page: " + main_text_content)
    assert str(number_of_accepted_ballots) == str(NUMBER_OF_VOTING_VOTERS), "Number of accepted ballots (" + str(number_of_accepted_ballots) + ") is not the same as number of voters (" + str(NUMBER_OF_VOTING_VOTERS) + ")"


  def test_scenario_1_simple_vote(self):
    print("### Starting step: administrator_creates_election")
    self.administrator_creates_election()
    print("### Step complete: administrator_creates_election")

    print("### Starting step: administrator_regenerates_passwords_for_some_voters")
    self.administrator_regenerates_passwords_for_some_voters()
    print("### Step complete: administrator_regenerates_passwords_for_some_voters")

    print("### Starting step: verify_election_consistency (0)")
    verify_election_consistency(self.election_id)
    print("### Step complete: verify_election_consistency (0)")

    print("### Starting step: all_voters_vote")
    self.all_voters_vote()
    print("### Step complete: all_voters_vote")

    print("### Starting step: some_voters_revote")
    self.some_voters_revote()
    print("### Step complete: some_voters_revote")

    print("### Starting step: verify_election_consistency (1)")
    verify_election_consistency(self.election_id)
    print("### Step complete: verify_election_consistency (1)")

    print("### Starting step: administrator_does_tallying_of_election")
    self.administrator_does_tallying_of_election()
    print("### Step complete: administrator_does_tallying_of_election")

    print("### Starting step: verify_election_consistency (2)")
    verify_election_consistency(self.election_id)
    print("### Step complete: verify_election_consistency (2)")

    # TODO: also use `belenios-tool verify-diff`


if __name__ == "__main__":
  unittest.main()
