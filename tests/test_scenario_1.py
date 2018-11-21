import unittest
import time
import string
import random
import os
import shutil
import subprocess
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException


SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "demo/run-server.sh"
SERVER_URL = "http://localhost:8001"
DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY = "_run/spool"
FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "tests/tools/sendmail_fake.sh"
SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = "/tmp/sendmail_fake"
USE_HEADLESS_BROWSER = True
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


def remove_database_folder():
    shutil.rmtree(os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY), ignore_errors=True)


def wait_a_bit():
    time.sleep(WAIT_TIME_BETWEEN_EACH_STEP)

def install_fake_sendmail_log_file():
    subprocess.run(["rm", "-f", SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH])
    subprocess.run(["touch", SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH])

def uninstall_fake_sendmail_log_file():
    subprocess.run(["rm", "-f", SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH])


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
  def setUp(self):
    install_fake_sendmail_log_file()

    remove_database_folder()

    server_path = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY)
    fake_sendmail_absolute_path = os.path.join(GIT_REPOSITORY_ABSOLUTE_PATH, FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY)
    custom_environment_variables = dict(os.environ, BELENIOS_SENDMAIL=fake_sendmail_absolute_path)
    self.server = subprocess.Popen([server_path], stdout=subprocess.PIPE, env=custom_environment_variables)

    if USE_HEADLESS_BROWSER:
        from selenium.webdriver.firefox.options import Options
        options = Options()
        options.add_argument("--headless")
        options.log.level = "trace"
        self.driver = webdriver.Firefox(options=options)
    else:
        self.driver = webdriver.Firefox()


  def tearDown(self):
    self.driver.quit()

    self.server.kill()

    remove_database_folder()

    uninstall_fake_sendmail_log_file()


  def administrator_creates_election(self):
    # # Setting up a new election (action of the administrator)

    # Edith has been given administrator rights on an online voting app called Belenios. She goes
    # to check out its homepage
    browser = self.driver
    browser.get(SERVER_URL)

    # She notices the page title mentions an election
    assert 'Election Server' in browser.title, "Browser title was: " + browser.title

    # She clicks on the "Accept" button to accept the personal data policy
    accept_button_css_selector = '#main form input[type=submit]'
    button_elements = browser.find_elements_by_css_selector(accept_button_css_selector)
    assert len(button_elements) is 1
    button_elements
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

    # She clicks on the "Prepare a new election" link
    create_election_link_text = "Prepare a new election"
    links_css_selector = "#main a"
    links_elements = browser.find_elements_by_css_selector(links_css_selector)
    assert len(links_elements)
    create_election_link_element = links_elements[0]
    link_real_content = create_election_link_element.get_attribute('innerText')
    assert create_election_link_text in link_real_content, "Link text was: " + link_real_content
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
    page_visible_title_element = browser.find_element_by_css_selector("#header h1")
    page_title_expected_content = "Questions for"
    page_title_real_content = page_visible_title_element.get_attribute('innerText')
    assert page_title_expected_content in page_title_real_content, "Page title was: " + page_title_real_content

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
    save_changes_button_real_content = save_changes_button_element.get_attribute('innerText')
    assert save_changes_button_expected_label in save_changes_button_real_content, "Save button label was: " + save_changes_button_real_content
    save_changes_button_element.click()

    wait_a_bit()

    # She clicks on the "Edit voters" link, to then type the list of voters
    edit_voters_link_css_selector = "#edit_voters"
    edit_voters_link_element = browser.find_element_by_css_selector(edit_voters_link_css_selector)
    edit_voters_link_element.click()

    wait_a_bit()

    # She types N e-mail addresses (the list of invited voters)
    email_addresses = random_email_addresses_generator(NUMBER_OF_INVITED_VOTERS)
    voters_list_field_css_selector = "#main form textarea"
    voters_list_field_element = browser.find_element_by_css_selector(voters_list_field_css_selector)
    voters_list_field_element.clear()
    is_first = True
    for email_address in email_addresses:
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
    generate_on_server_button_css_selector = "#main form input[value='" + generate_on_server_button_label + "']"
    generate_on_server_button_element = browser.find_element_by_css_selector(generate_on_server_button_css_selector)
    generate_on_server_button_element.click()

    wait_a_bit()

    # (Server sends emails to voters.) She checks that server does not show any error that would happen when trying to send these emails (this can happen if sendmail is not configured)
    confirmation_sentence_expected_text = "Credentials have been generated and mailed!"
    confirmation_sentence_css_selector = "#main p"
    confirmation_sentence_element = browser.find_element_by_css_selector(confirmation_sentence_css_selector)
    confirmation_sentence_real_content = confirmation_sentence_element.get_attribute('innerText')
    assert confirmation_sentence_expected_text in confirmation_sentence_real_content, "Confirmation sentence was: " + confirmation_sentence_real_content

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

    email_address_to_look_for = email_addresses[0]
    text_to_look_for = 'To: "' + email_address_to_look_for + '"'
    email_address_found = find_in_sent_emails(text_to_look_for)
    assert email_address_found, "Text '" + email_address_to_look_for + "'' not found in fake sendmail log file"


    # She clicks on the "Proceed" link
    proceed_link_expected_label = "Proceed"
    proceed_link_css_selector = "#main a"
    proceed_link_element = browser.find_element_by_css_selector(proceed_link_css_selector)
    proceed_link_element_real_label = proceed_link_element.get_attribute('innerText')
    assert proceed_link_expected_label in proceed_link_element_real_label, "Proceed link label was: " + proceed_link_element_real_label
    proceed_link_element.click()

    wait_a_bit()

    # In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
    generate_and_mail_missing_passwords_button_label = "Generate and mail missing passwords"
    generate_and_mail_missing_passwords_button_css_selector = "#main form input[value='" + generate_and_mail_missing_passwords_button_label + "']"
    generate_and_mail_missing_passwords_button_element = browser.find_element_by_css_selector(generate_and_mail_missing_passwords_button_css_selector)
    generate_and_mail_missing_passwords_button_element.click()

    wait_a_bit()

    # She checks that the page contains expected confirmation text, instead of an error (TODO: explain in which case an error can happen, and check that it does not show)
    confirmation_sentence_expected_text = "Passwords have been generated and mailed!"
    confirmation_sentence_css_selector = "#main p"
    confirmation_sentence_element = browser.find_element_by_css_selector(confirmation_sentence_css_selector)
    confirmation_sentence_real_content = confirmation_sentence_element.get_attribute('innerText')
    assert confirmation_sentence_expected_text in confirmation_sentence_real_content, "Confirmation sentence was: " + confirmation_sentence_real_content

    # She clicks on the "Proceed" link
    proceed_link_expected_label = "Proceed"
    proceed_link_css_selector = "#main a"
    proceed_link_element = browser.find_element_by_css_selector(proceed_link_css_selector)
    proceed_link_element_real_label = proceed_link_element.get_attribute('innerText')
    assert proceed_link_expected_label in proceed_link_element_real_label, "Proceed link label was: " + proceed_link_element_real_label
    proceed_link_element.click()

    wait_a_bit()

    # In "Validate creation" section, she clicks on the "Create election" link
    create_election_link_label = "Create election"
    browser.find_element_by_partial_link_text(create_election_link_label).click()

    wait_a_bit()

    # She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!"

    # In the "Validate creation" section, she clicks on the "Create election" button
    create_election_button_label = "Create election"
    create_election_button_css_selector = "#main form input[value='" + create_election_button_label + "']"
    create_election_button_element = browser.find_element_by_css_selector(create_election_button_css_selector)
    create_election_button_element.click()

    wait_a_bit()

    # She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election"

    # She checks that a "Close election" button is present (but she does not click on it)
    close_election_button_label = "Close election"
    close_election_button_css_selector = "#main form input[value='" + close_election_button_label + "']"
    close_election_button_elements = browser.find_elements_by_css_selector(close_election_button_css_selector)
    assert len(close_election_button_elements)

    # In the header of the page, she clicks on the "Log out" link
    logout_link_css_id = "logout"
    logout_element = browser.find_element_by_id(logout_link_css_id)
    logout_element.click()

    # She arrives back on the logged out home page. She checks that a login link is present
    login_link_id = "login_local"
    login_element = browser.find_element_by_id(login_link_id)


  def administrator_regenerates_passwords_for_some_voters(self):
    # TODO: implement this step
    pass


  def test_scenario_1_simple_vote(self):
    print("### Starting step: administrator_creates_election")
    self.administrator_creates_election()
    print("### Step complete: administrator_creates_election")

    print("### Starting step: administrator_regenerates_passwords_for_some_voters")
    self.administrator_regenerates_passwords_for_some_voters()
    print("### Step complete: administrator_regenerates_passwords_for_some_voters")

    # TODO: implement next steps


if __name__ == "__main__":
  unittest.main()
