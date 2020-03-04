#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import subprocess
import re
import sys
from uuid import uuid4
from distutils.util import strtobool
from selenium.common.exceptions import UnexpectedAlertPresentException
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.selenium_tools import wait_for_element_exists, wait_for_elements_exist, wait_for_element_exists_and_contains_expected_text, wait_for_element_exists_and_has_non_empty_content, wait_for_an_element_with_partial_link_text_exists, set_element_attribute, wait_for_element_exists_and_has_non_empty_attribute, verify_all_elements_have_attribute_value, verify_some_elements_have_attribute_value
from util.election_testing import console_log, random_email_addresses_generator, remove_database_folder, wait_a_bit, build_css_selector_to_find_buttons_in_page_content_by_value, initialize_server, initialize_browser, election_page_url_to_election_id, verify_election_consistency, create_election_data_snapshot, delete_election_data_snapshot, log_in_as_administrator, log_out, administrator_starts_creation_of_election, administrator_edits_election_questions, administrator_sets_election_voters, administrator_validates_creation_of_election
from util.election_test_base import BeleniosElectionTestBase
import settings


def initialize_browser_for_scenario_2():
    return initialize_browser(for_scenario_2=True)


class BeleniosTestElectionScenario2Base(BeleniosElectionTestBase):
    """
    Properties:
    - server
    - browser
    - fake_sent_emails_manager: An instance of FakeSentEmailsManager
    - voters_email_addresses: A list of email addresses (strings). This is all users who are invited to vote
    - voters_email_addresses_who_have_lost_their_password: A list of email addresses (strings). This is all users who have asked for a new password.
    - voters_email_addresses_who_have_voted: A dictionary, indexed by email address (string), where each element value is True
    - voters_data: A dictionary, indexed by email address (string), where each element is a dictionary of fields for the voter who is identified by this email address. This is data about all users who have voted.
    - election_page_url: The election page URL (string). Example: "http://localhost:8001/elections/H5ecRG3wHZ21cp/"
    - election_id: The election ID (string). Example: "H5ecRG3wHZ21cp"
    - draft_election_administration_page_url: URL of the draft election administration page
    - credential_authority_link
    - credential_authority_file_paths
    - links_for_trustees
    - downloaded_files_paths_per_trustee
    - temporary_files_to_remove_after_test
    - closed_election_tally_links_for_trustees
    """


    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)

        self.draft_election_administration_page_url = None
        self.credential_authority_link = None
        self.credential_authority_file_paths = dict() # A dict where key is a label describing the file and value is the absolute path to file
        self.links_for_trustees = []
        self.downloaded_files_paths_per_trustee = dict() # A dict where key is trustee email address, and value is a dict where key is file label (for example "private key" or "public key"), and value is the absolute path to the file
        self.temporary_files_to_remove_after_test = []
        self.closed_election_tally_links_for_trustees = []


    def setUp(self):
        self.fake_sent_emails_manager = FakeSentEmailsManager(settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
        self.fake_sent_emails_manager.install_fake_sendmail_log_file()

        remove_database_folder()

        self.server = initialize_server()

        self.browser = initialize_browser_for_scenario_2()


    def tearDown(self):
        self.browser.quit()

        self.server.kill()

        remove_database_folder()

        self.fake_sent_emails_manager.uninstall_fake_sendmail_log_file()

        self.remove_temporary_files()


    def remember_temporary_file_to_remove_after_test(self, file_path):
        self.temporary_files_to_remove_after_test.append(file_path)


    def remove_temporary_files(self):
        for el in self.temporary_files_to_remove_after_test:
            subprocess.run(["rm", "-f", el]) # TODO: Execute a command that works on other OS, like `os.remove()`


    def administrator_starts_creation_of_manual_election(self):
        # # Setting up a new election (action of the administrator)

        browser = self.browser

        # Alice has been given administrator rights on an online voting app called Belenios. She goes
        # to check out its homepage and logs in
        log_in_as_administrator(browser)

        # She starts creation of the election:
        # - She clicks on the "Prepare a new election" link
        # - She picks the Credential management method: manual
        # (- She keeps default value for Authentication method: it is Password, not CAS)
        # - She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
        # - In the "Name and description of the election" section, she changes values of fields name and description of the election
        # - She clicks on the "Save changes button" (the one that is next to the election description field)
        # - In "Contact" section, she changes the value of "contact" field
        # - She clicks on the "Save changes" button (the one that is in the "Contact" section)
        administrator_starts_creation_of_election(browser, True)

        # She remembers the URL of the draft election administration page
        self.draft_election_administration_page_url = browser.current_url

        # She edits election's questions:
        # - She clicks on the "Edit questions" link, to write her own questions
        # - She arrives on the Questions page. She checks that the page title is correct
        # - She removes answer 3
        # - She clicks on the "Save changes" button (this redirects to the "Preparation of election" page)
        administrator_edits_election_questions(browser)

        # She sets election's voters:
        # - She clicks on the "Edit voters" link, to then type the list of voters
        # - She types N e-mail addresses (the list of invited voters)
        # - She clicks on the "Add" button to submit changes
        # - She clicks on "Return to draft page" link
        self.voters_email_addresses = random_email_addresses_generator(settings.NUMBER_OF_INVITED_VOTERS)
        administrator_sets_election_voters(browser, self.voters_email_addresses)

        # In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
        generate_and_mail_missing_passwords_button_label = "Generate and mail missing passwords"
        generate_and_mail_missing_passwords_button_css_selector = "#main input[type=submit][value='" + generate_and_mail_missing_passwords_button_label + "']"
        generate_and_mail_missing_passwords_button_element = wait_for_element_exists(browser, generate_and_mail_missing_passwords_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        generate_and_mail_missing_passwords_button_element.click() # FIXME: This click does not get triggered when we have maximized the browser window

        wait_a_bit()

        # She checks that the page contains expected confirmation text, instead of an error (TODO: explain in which case an error can happen, and check that it does not show)
        confirmation_sentence_expected_text = "Passwords have been generated and mailed!"
        confirmation_sentence_css_selector = "#main p"
        wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, settings.EXPLICIT_WAIT_TIMEOUT)

        # She clicks on the "Proceed" link (this redirects to the "Preparation of election" page)
        proceed_link_expected_label = "Proceed"
        proceed_link_css_selector = "#main a"
        proceed_link_element = wait_for_element_exists_and_contains_expected_text(browser, proceed_link_css_selector, proceed_link_expected_label, settings.EXPLICIT_WAIT_TIMEOUT)
        proceed_link_element.click()

        wait_a_bit()

        # In "Credentials" section, she clicks on "Credential management" link
        credential_management_expected_label = "Credential management"
        credential_management_link_element = wait_for_an_element_with_partial_link_text_exists(browser, credential_management_expected_label)
        credential_management_link_element.click()

        wait_a_bit()

        # She fills in her public name, then clicks on "Set"
        credential_authority_css_selector = "#main form input[name=__co_eliom_name]"
        credential_authority_element = wait_for_element_exists(browser, credential_authority_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        credential_authority_element.clear()
        credential_authority_element.send_keys("Cecily");
        credential_authority_set_css_selector = "#main form input[type=submit]"
        credential_authority_set_element = browser.find_element_by_css_selector(credential_authority_set_css_selector)
        credential_authority_set_element.click()

        wait_a_bit()

        # She clicks on the "Proceed" link
        proceed_link_expected_label = "Proceed"
        proceed_link_css_selector = "#main a"
        proceed_link_element = wait_for_element_exists_and_contains_expected_text(browser, proceed_link_css_selector, proceed_link_expected_label, settings.EXPLICIT_WAIT_TIMEOUT)
        proceed_link_element.click()

        wait_a_bit()

        # She remembers the link displayed
        link_for_credential_authority_css_selector = "#credential_authority_link"
        link_for_credential_authority_element = wait_for_element_exists_and_has_non_empty_content(browser, link_for_credential_authority_css_selector)
        link_label = link_for_credential_authority_element.get_attribute('innerText').strip()
        self.credential_authority_link = link_label

        # She sends the remembered link to the credential authority by email (actually we don't need to send anything because we will act as the credential authority)

        # Optionnaly, she logs out
        # log_out(browser)

        # She closes the browser window
        browser.quit()


    def credential_authority_sends_credentials_to_voters(self):
        # Cecily, the Credential Authority, receives the email sent by Alice, and opens the link in it
        self.browser = initialize_browser_for_scenario_2()
        browser = self.browser
        browser.get(self.credential_authority_link)

        wait_a_bit()

        # She remembers what the link to the election will be, so that she will be able to send it to voters by email with their private credential
        # TODO: use a better selector: edit Belenios page to use an ID in this DOM element
        future_election_link_css_selector = "#main ul li"
        future_election_link_element = wait_for_element_exists_and_has_non_empty_content(browser, future_election_link_css_selector)
        self.election_page_url = future_election_link_element.get_attribute('innerText').strip()

        # She clicks on the "Generate" button
        generate_button_css_selector = "#interactivity button"
        generate_button_element = wait_for_element_exists(browser, generate_button_css_selector)
        generate_button_element.click()

        wait_a_bit()

        # She clicks on the "private credentials" and "public credentials" links and downloads these files. Files are by default downloaded to /tmp using filenames `creds.txt` and `public_creds.txt` respectively, but we choose to name them using an unique identifier instead.
        link_css_ids = ["creds"]
        file_labels = ["private credentials"]
        link_css_selectors = ["#" + el for el in link_css_ids]
        for idx, link_css_id in enumerate(link_css_ids):
            link_element = wait_for_element_exists(browser, link_css_selectors[idx])
            target_filename = str(uuid4())
            set_element_attribute(browser, link_css_id, 'download', target_filename)
            link_element.click()
            file_absolute_path = os.path.join(settings.BROWSER_DOWNLOAD_FOLDER, target_filename)
            self.credential_authority_file_paths[file_labels[idx]] = file_absolute_path # we save the filename in a class instance property, so that we can read the file afterwards (to extract trustee credentials and send them by email to trustees)
            self.remember_temporary_file_to_remove_after_test(file_absolute_path)

        wait_a_bit()

        # She clicks on the "Submit public credentials" button
        submit_button_css_selector = "#submit_form input[type=submit]"
        submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
        submit_button_element.click()

        wait_a_bit()

        # She checks that redirected page shows correct confirmation sentence
        expected_content_text = "Credentials have been received and checked!"
        expected_content_css_selector = "#main"
        wait_for_element_exists_and_contains_expected_text(browser, expected_content_css_selector, expected_content_text)

        wait_a_bit()

        # She closes the window
        browser.quit()

        # She reads the private credentials file (creds.txt) and sends credential emails to voters
        # TODO: Should we check that creds.txt contains the exact same voters email addresses as the ones that admin has added?
        private_credentials_file_path = self.credential_authority_file_paths["private credentials"]
        self.credential_authority_sends_credentials_to_voters_from_credentials_file(private_credentials_file_path)


    def credential_authority_sends_credentials_to_voters_from_credentials_file(self, private_credentials_file_path, voters_email_addresses=None):
        from_email_address = settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS
        subject = "Your credential for election " + settings.ELECTION_TITLE
        content = """You are listed as a voter for the election

  {election_title}

You will find below your credential.  To cast a vote, you will also
need a password, sent in a separate email.  Be careful, passwords and
credentials look similar but play different roles.  You will be asked
to enter your credential before entering the voting booth.  Login and
passwords are required once your ballot is ready to be cast.

Credential: {credential}
Page of the election: {election_url}

Note that you are allowed to vote several times.  Only the last vote
counts."""
        with open(private_credentials_file_path) as myfile:
            i = 0
            for line in myfile:
                voter_email_address = None
                match = re.search(r'^(\S+)\s(\S+)$', line)
                if match:
                    if voters_email_addresses is not None:
                        voter_email_address = voters_email_addresses[i]
                    else:
                        voter_email_address = match.group(1)
                    voter_private_credential = match.group(2)
                else:
                    raise Exception("File creds.txt has wrong format")
                custom_content = content.format(election_title=settings.ELECTION_TITLE, credential=voter_private_credential, election_url=self.election_page_url)
                self.fake_sent_emails_manager.send_email(from_email_address, voter_email_address, subject, custom_content)
                i += 1

    def administrator_invites_trustees(self):
        self.browser = initialize_browser_for_scenario_2()
        browser = self.browser

        log_in_as_administrator(browser)

        browser.get(self.draft_election_administration_page_url)

        wait_a_bit()

        # In the trustees section, she clicks on the "here" link
        # TODO: use a better selector: edit Belenios page to use an ID in this DOM element
        setup_election_key_link_label = "here"
        setup_election_key_link_element = wait_for_an_element_with_partial_link_text_exists(browser, setup_election_key_link_label)
        setup_election_key_link_element.click()

        wait_a_bit()

        # She adds two trustees (their email address), and remembers the link she will send to each trustee
        self.links_for_trustees = []
        email_address_field_css_selector = "#main form input[type=text]"
        submit_button_css_selector = "#main form input[type=submit][value=Add]"

        for idx, email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            email_address_field_element = wait_for_element_exists(browser, email_address_field_css_selector)
            email_address_field_element.clear()
            email_address_field_element.send_keys(email_address)

            submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
            submit_button_element.click()

            wait_a_bit()

            trustee_link_css_selector = "#main table tr:nth-of-type(" + str(idx + 3) + ") td:nth-of-type(4) a" # First row of table corresponds to column titles. Second row correpond to server trustee.
            trustee_link_element = wait_for_element_exists_and_has_non_empty_content(browser, trustee_link_css_selector)
            self.links_for_trustees.append(trustee_link_element.get_attribute('href'))

            wait_a_bit()

        # She sends to each trustee an email containing their own link
        subject = "Link to generate the decryption key"
        content_format = """\
Dear trustee,

You will find below the link to generate your private decryption key, used to tally the election.

{link_for_trustee}

Here's the instructions:
1. click on the link
2. click on "generate a new key pair"
3. your private key will appear in another window or tab. Make sure
you SAVE IT properly otherwise it will not possible to tally and the
election will be canceled.
4. in the first window, click on "submit" to send the public part of
your key, used encrypt the votes. For verification purposes, you
should save this part (that starts with "pok" "challenge"), for
example sending yourself an email.

Regarding your private key, it is crucial you save it (otherwise the
election will be canceled) and store it securely (if your private key
is known together with the private keys of the other trustees, then
vote privacy is no longer guaranteed). We suggest two options:
1. you may store the key on a USB stick and store it in a safe.
2. Or you may simply print it and store it in a safe.
Of course, more cryptographic solutions are welcome as well.

Thank you for your help,

--
The election administrator.\
"""
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            custom_content = content_format.format(link_for_trustee=self.links_for_trustees[idx])
            self.fake_sent_emails_manager.send_email(settings.ADMINISTRATOR_EMAIL_ADDRESS, trustee_email_address, subject, custom_content)

        # Optionnaly, she logs out
        # log_out(browser)

        # She closes the window
        browser.quit()


    def trustees_generate_election_private_keys(self):
        # Each trustee (Tom and Taylor) will do the following process
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            # Trustee opens link that has been sent to him by election administrator
            link_for_this_trustee = self.links_for_trustees[idx] # TODO: Decide either not send trustee email at all or read trustee link from email content
            self.browser = initialize_browser_for_scenario_2()
            browser = self.browser
            browser.get(link_for_this_trustee)

            # He checks that the page content shows the same election URL as the one the administrator saw
            election_url_css_selector = "#main ul li"
            election_url_element = wait_for_element_exists_and_has_non_empty_content(browser, election_url_css_selector)
            election_url_content = election_url_element.get_attribute('innerText').strip()
            assert election_url_content == self.election_page_url

            # He clicks on the "Generate a new keypair" button
            generate_button_css_selector = "#interactivity button"
            generate_button_expected_label = "Generate a new keypair"
            generate_button_element = wait_for_element_exists_and_contains_expected_text(browser, generate_button_css_selector, generate_button_expected_label)
            generate_button_element.click()

            # He clicks on the "private key" and "public key" links, to download the private key and the public key (files are respectively saved by default as `private_key.json` and `public_key.json`, but we decide to save them as a unique file name)
            link_css_ids = ["private_key"]
            link_expected_labels = ["private key"]
            self.downloaded_files_paths_per_trustee[trustee_email_address] = dict()
            for idx2, link_css_id in enumerate(link_css_ids):
                link_target_filename = str(uuid4())
                set_element_attribute(browser, link_css_id, 'download', link_target_filename)
                link_expected_label = link_expected_labels[idx2]
                link_element = wait_for_an_element_with_partial_link_text_exists(browser, link_expected_label)
                assert link_element.get_attribute('id') == link_css_id
                link_element.click()
                file_absolute_path = os.path.join(settings.BROWSER_DOWNLOAD_FOLDER, link_target_filename)
                # We save the filename in a class instance property, so that we can import the file afterwards (during partial decryption step)
                self.downloaded_files_paths_per_trustee[trustee_email_address][link_expected_labels[idx2]] = file_absolute_path
                self.remember_temporary_file_to_remove_after_test(file_absolute_path)

            # He clicks on the "Submit public key" button
            submit_button_expected_label = "Submit public key"
            submit_button_css_selector = "#main input[type=submit][value='" + submit_button_expected_label + "']"
            submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
            submit_button_element.click()

            # He checks that the next page shows the expected confirmation sentence
            expected_confirmation_label = "Your key has been received and checked!"
            expected_confirmation_css_selector = "#main"
            wait_for_element_exists_and_contains_expected_text(browser, expected_confirmation_css_selector, expected_confirmation_label)

            # He closes the window
            browser.quit()


    def administrator_completes_creation_of_election(self):
        # Alice, as an administrator of an election, wants to finalize her draft election creation, to start the vote.
        # She opens a browser
        self.browser = initialize_browser_for_scenario_2()
        browser = self.browser

        # She logs in as administrator
        log_in_as_administrator(browser)

        # She goes to the draft election administration page
        browser.get(self.draft_election_administration_page_url)

        # - In "Validate creation" section, she clicks on the "Create election" link
        # - (She arrives on the "Checklist" page, that lists all main parameters of the election for review, and that flags incoherent or misconfigured parameters. For example, in this test scenario, it displays 2 warnings: "Warning: No trustees were set. This means that the server will manage the election key by itself.", and "Warning: No contact was set!")
        # - In the "Validate creation" section, she clicks on the "Create election" button
        # - (She arrives back on the "My test election for Scenario 1 — Administration" page. Its contents have changed. There is now a text saying "The election is open. Voters can vote.", and there are now buttons "Close election", "Archive election", "Delete election")
        # - She remembers the URL of the voting page, that is where the "Election home" link points to
        # - She checks that a "Close election" button is present (but she does not click on it)
        self.election_page_url = administrator_validates_creation_of_election(browser)
        console_log("election_page_url:", self.election_page_url)
        self.election_id = election_page_url_to_election_id(self.election_page_url)
        console_log("election_id:", self.election_id)

        # She logs out
        log_out(browser, self.election_id)

        # She closes the window, and re-opens it (for next emulated user)
        browser.quit()
        self.browser = initialize_browser_for_scenario_2()


    def administrator_starts_tallying_of_election(self, with_threshold=None):
        browser = self.browser

        # Alice goes to the election page
        election_url = self.election_page_url # Could also be obtained with self.voters_data[self.voters_email_addresses[0]]["election_page_url"]
        browser.get(election_url)

        wait_a_bit()

        # She clicks on the "Administer this election" link
        administration_link_label = "Administer this election"
        administration_link_element = wait_for_an_element_with_partial_link_text_exists(browser, administration_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
        administration_link_element.click()

        # She logs in as administrator
        log_in_as_administrator(browser, from_a_login_page=True)

        wait_a_bit()

        # She clicks on the "Close election" button
        close_election_button_label = "Close election"
        close_election_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(close_election_button_label)
        close_election_button_element = wait_for_element_exists(browser, close_election_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        close_election_button_element.click()

        wait_a_bit()

        # She clicks on the "Proceed to vote counting" button
        proceed_button_label = "Proceed to vote counting"
        proceed_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(proceed_button_label)
        proceed_button_element = wait_for_element_exists(browser, proceed_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        proceed_button_element.click()

        wait_a_bit()

        if with_threshold is not None:
            # She checks the presence of text "We are now waiting for trustees... At least ${U} trustee(s) must act."
            expected_confirmation_label = "We are now waiting for trustees... At least " + str(with_threshold) + " trustee(s) must act."
            expected_confirmation_css_selector = "#main"
            wait_for_element_exists_and_contains_expected_text(browser, expected_confirmation_css_selector, expected_confirmation_label)

            # She checks that in the table on every content row, the "DONE?" column is "No"
            elements_css_selector = "#main table tr td:nth-of-type(4)"
            attribute_name = "innerText"
            attribute_value = "No"
            verify_all_elements_have_attribute_value(browser, elements_css_selector, attribute_name, attribute_value)

        # She remembers the link to send to each trustee, so they can tally the election
        row_padding = 3
        if with_threshold:
            row_padding = 2
        self.closed_election_tally_links_for_trustees = []
        for idx, email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            trustee_link_css_selector = "#main table tr:nth-of-type(" + str(idx + row_padding) + ") td:nth-of-type(3) a" # First row consists in column titles. If there is no threshold for trustees, second row is for server.
            trustee_link_element = wait_for_element_exists_and_has_non_empty_content(browser, trustee_link_css_selector)
            self.closed_election_tally_links_for_trustees.append(trustee_link_element.get_attribute('href'))

        # She sends to each trustee an email containing their own link
        subject = "Link to tally the election"
        content_format = """\
Dear trustee,

The election is now closed. Here's the link to proceed to tally:

{link_for_trustee}

Here's the instructions:
1. Follow the link.
2. Enter your private decryption key in the first box and click on
"generate decryption factors"
3. The second box is now filled with crypto material. Please press the
button "submit".

Thank you again for your help,

-- 
The election administrator.\
"""
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            custom_content = content_format.format(link_for_trustee=self.closed_election_tally_links_for_trustees[idx])
            self.fake_sent_emails_manager.send_email(settings.ADMINISTRATOR_EMAIL_ADDRESS, trustee_email_address, subject, custom_content)

        # She logs out
        log_out(browser)

        # She closes the window
        browser.quit()


    def trustees_do_partial_decryption(self, max_trustees=None):
        # Each of the `T` trustees (limited to `max_trustees`) will do the following process:
        for idx, trustee_email_address in enumerate(settings.TRUSTEES_EMAIL_ADDRESSES):
            if max_trustees is not None and idx >= max_trustees: # TODO: Maybe we should pick trustees randomly in the list of trustees instead of always the first few ones?
                break
            # He opens the link that Alice (the election administrator) has sent to him
            self.browser = initialize_browser_for_scenario_2()
            browser = self.browser
            link_for_trustee = self.closed_election_tally_links_for_trustees[idx]
            browser.get(link_for_trustee)

            wait_a_bit()

            # He verifies that the "private key" input field is empty (at the beginning)
            private_key_field_css_selector = "#private_key"
            private_key_field_element = wait_for_element_exists(browser, private_key_field_css_selector)
            assert private_key_field_element.get_attribute('value') == ""

            # One trustee uploads his private key file, the other copy-pastes its contents into the form field
            private_key_file = self.downloaded_files_paths_per_trustee[trustee_email_address]["private key"]
            if idx % 2 == 0:
                # He clicks on the "Browse..." button and selects his private key file (initially downloaded as `private_key.json` by default)
                browse_button_css_selector = "input[id=private_key_file][type=file]"
                browse_button_element = wait_for_element_exists(browser, browse_button_css_selector)
                path_of_file_to_upload = private_key_file
                browse_button_element.clear()
                browse_button_element.send_keys(path_of_file_to_upload)

                # He waits until the "private key" input field (that has id "#private_key") becomes not empty anymore. This is because once the user has selected the file to upload, the Javascript code in the page detects that a file has been selected, reads it, and fills "private key" input field with file's contents. The computation triggered by click on the "Compute decryption factors" button will use the value of this field, not directly the uploaded file contents.
                private_key_field_expected_non_empty_attribute = "value"
                wait_for_element_exists_and_has_non_empty_attribute(browser, private_key_field_css_selector, private_key_field_expected_non_empty_attribute)
            else:
                with open(private_key_file) as myfile:
                    private_key_field_element.send_keys(myfile.read())
                wait_a_bit()

            # He clicks on the "Compute decryption factors" button
            compute_button_css_selector = "button[id=compute]"
            compute_button_element = wait_for_element_exists(browser, compute_button_css_selector)
            compute_button_element.click()

            # He checks that the text field below (used as visual feedback) now contains text
            visual_feedback_css_selector = "#pd"
            visual_feedback_expected_non_empty_attribute = "value"
            try:
                wait_for_element_exists_and_has_non_empty_attribute(browser, visual_feedback_css_selector, visual_feedback_expected_non_empty_attribute, 60 * 2)
            except UnexpectedAlertPresentException as e:
                raise Exception("An alert was displayed at a moment when no alert should be displayed. Alert displayed probably contains error information about uploaded file contents.") from e

            # He clicks on the "Submit" button
            submit_button_css_selector = "#pd_done input[type=submit]"
            submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
            submit_button_element.click()

            wait_a_bit()

            # He checks that next screen contains a confirmation sentence
            confirmation_sentence_expected_text = "Your partial decryption has been received and checked!"
            confirmation_sentence_css_selector = "#main p"
            wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, settings.EXPLICIT_WAIT_TIMEOUT)

            # He closes the window
            browser.quit()


    def administrator_finishes_tallying_of_election(self, max_trustees=None):
        self.browser = initialize_browser_for_scenario_2()
        browser = self.browser

        # Alice goes to the election page
        election_url = self.election_page_url
        browser.get(election_url)

        wait_a_bit()

        # She clicks on the "Administer this election" link
        administration_link_label = "Administer this election"
        administration_link_element = wait_for_an_element_with_partial_link_text_exists(browser, administration_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
        administration_link_element.click()

        # She logs in as administrator
        log_in_as_administrator(browser, from_a_login_page=True)

        wait_a_bit()

        # She checks that the "DONE?" column of each trustee is to "Yes" (or if there is a threshold on trustees, she checks that at least `max_trustees` have a "Yes")
        expected_label = "Yes"
        yes_cells_selector = "#main table tr td:nth-last-child(1)"
        attribute_name = "innerText"
        if max_trustees is None:
            verify_all_elements_have_attribute_value(browser, yes_cells_selector, attribute_name, expected_label)
        else:
            verify_some_elements_have_attribute_value(browser, yes_cells_selector, attribute_name, expected_label, max_trustees)

        # She clicks on the "Compute the result" button
        compute_result_button_expected_label = "Compute the result"
        compute_result_button_css_selector = "#main input[type=submit][value='" + compute_result_button_expected_label + "']"
        compute_result_button_element = wait_for_element_exists(browser, compute_result_button_css_selector)
        compute_result_button_element.click()

        wait_a_bit()

        self.administrator_verifies_vote_results()


class BeleniosTestElectionScenario2(BeleniosTestElectionScenario2Base):

    def test_scenario_2_manual_vote(self):
        console_log("### Running test method BeleniosTestElectionScenario2::test_scenario_2_manual_vote()")
        console_log("### Starting step: administrator_starts_creation_of_manual_election")
        self.administrator_starts_creation_of_manual_election()
        console_log("### Step complete: administrator_starts_creation_of_manual_election")

        console_log("### Starting step: credential_authority_sends_credentials_to_voters")
        self.credential_authority_sends_credentials_to_voters()
        console_log("### Step complete: credential_authority_sends_credentials_to_voters")

        console_log("### Starting step: administrator_invites_trustees")
        self.administrator_invites_trustees()
        console_log("### Step complete: administrator_invites_trustees")

        console_log("### Starting step: trustees_generate_election_private_keys")
        self.trustees_generate_election_private_keys()
        console_log("### Step complete: trustees_generate_election_private_keys")

        console_log("### Starting step: administrator_completes_creation_of_election")
        self.administrator_completes_creation_of_election()
        console_log("### Step complete: administrator_completes_creation_of_election")

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

        console_log("### Starting step: administrator_starts_tallying_of_election")
        self.administrator_starts_tallying_of_election()
        console_log("### Step complete: administrator_starts_tallying_of_election")

        console_log("### Starting step: trustees_do_partial_decryption")
        self.trustees_do_partial_decryption()
        console_log("### Step complete: trustees_do_partial_decryption")

        console_log("### Starting step: administrator_finishes_tallying_of_election")
        self.administrator_finishes_tallying_of_election()
        console_log("### Step complete: administrator_finishes_tallying_of_election")

        console_log("### Starting step: verify_election_consistency using `belenios_tool verify` (3)")
        verify_election_consistency(self.election_id)
        console_log("### Step complete: verify_election_consistency using `belenios_tool verify` (3)")


if __name__ == "__main__":
    random_seed = os.getenv('RANDOM_SEED', None)
    if not random_seed:
        random_seed = random.randrange(sys.maxsize)
    console_log("Python random seed being used:", random_seed)
    random.seed(random_seed)

    if os.getenv('USE_HEADLESS_BROWSER', None):
        settings.USE_HEADLESS_BROWSER = bool(strtobool(os.getenv('USE_HEADLESS_BROWSER')))

    settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = os.getenv('SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH', settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    settings.WAIT_TIME_BETWEEN_EACH_STEP = float(os.getenv('WAIT_TIME_BETWEEN_EACH_STEP', settings.WAIT_TIME_BETWEEN_EACH_STEP))
    settings.EXPLICIT_WAIT_TIMEOUT = int(os.getenv('EXPLICIT_WAIT_TIMEOUT', settings.EXPLICIT_WAIT_TIMEOUT))
    settings.NUMBER_OF_INVITED_VOTERS = int(os.getenv('NUMBER_OF_INVITED_VOTERS', settings.NUMBER_OF_INVITED_VOTERS))
    settings.NUMBER_OF_VOTING_VOTERS = int(os.getenv('NUMBER_OF_VOTING_VOTERS', settings.NUMBER_OF_VOTING_VOTERS))
    settings.NUMBER_OF_REVOTING_VOTERS = int(os.getenv('NUMBER_OF_REVOTING_VOTERS', settings.NUMBER_OF_REVOTING_VOTERS))
    settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS = int(os.getenv('NUMBER_OF_REGENERATED_PASSWORD_VOTERS', settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS))
    settings.ADMINISTRATOR_USERNAME = os.getenv('ADMINISTRATOR_USERNAME', settings.ADMINISTRATOR_USERNAME)
    settings.ADMINISTRATOR_PASSWORD = os.getenv('ADMINISTRATOR_PASSWORD', settings.ADMINISTRATOR_PASSWORD)
    settings.ELECTION_TITLE = os.getenv('ELECTION_TITLE', settings.ELECTION_TITLE)
    settings.ELECTION_DESCRIPTION = os.getenv('ELECTION_DESCRIPTION', settings.ELECTION_DESCRIPTION)
    settings.INITIATOR_CONTACT = os.getenv('INITIATOR_CONTACT', settings.INITIATOR_CONTACT)
    settings.BROWSER_DOWNLOAD_FOLDER = os.getenv('BROWSER_DOWNLOAD_FOLDER', settings.BROWSER_DOWNLOAD_FOLDER)
    settings.ADMINISTRATOR_EMAIL_ADDRESS = os.getenv('ADMINISTRATOR_EMAIL_ADDRESS', settings.ADMINISTRATOR_EMAIL_ADDRESS)
    settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS = os.getenv('CREDENTIAL_AUTHORITY_EMAIL_ADDRESS', settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS)
    # TODO: settings.TRUSTEES_EMAIL_ADDRESSES (it cannot be manipulated the same way because it is an array)

    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH:", settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)
    console_log("ELECTION_TITLE:", settings.ELECTION_TITLE)
    console_log("ELECTION_DESCRIPTION:", settings.ELECTION_DESCRIPTION)
    console_log("INITIATOR_CONTACT:", settings.INITIATOR_CONTACT)
    console_log("BROWSER_DOWNLOAD_FOLDER:", settings.BROWSER_DOWNLOAD_FOLDER)
    console_log("ADMINISTRATOR_EMAIL_ADDRESS:", settings.ADMINISTRATOR_EMAIL_ADDRESS)
    console_log("CREDENTIAL_AUTHORITY_EMAIL_ADDRESS:", settings.CREDENTIAL_AUTHORITY_EMAIL_ADDRESS)
    console_log("TRUSTEES_EMAIL_ADDRESSES:", settings.TRUSTEES_EMAIL_ADDRESSES)

    unittest.main()
