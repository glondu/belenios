#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
import csv
from distutils.util import strtobool
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.selenium_tools import wait_for_element_exists, wait_for_element_exists_and_has_non_empty_content, wait_for_element_exists_and_contains_expected_text
from util.election_testing import console_log, remove_database_folder, wait_a_bit, initialize_server, verify_election_consistency, populate_credential_and_password_for_voters_from_sent_emails, populate_random_votes_for_voters, admin_election_draft_page_url_to_election_id, belenios_tool_generate_credentials, remove_credentials_files, belenios_tool_generate_ballots
from test_scenario_2 import BeleniosTestElectionScenario2Base, initialize_browser_for_scenario_2
import settings




class BeleniosLoadTestingSetUp(BeleniosTestElectionScenario2Base):
    """
    Properties:
    - credential_file_id: Path and base filename (without extension) of the credential files generated by `belenios-tool credgen` command
    - distant_fake_sent_emails_manager: An instance of FakeSentEmailsManager, that corresponds to a text file that contains all fake emails sent by a distant Belenios server
    - fake_sent_emails_initial_lines_count: The number of lines that the fake sent emails file initially has on the server
    """

    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)
        self.credential_file_id = None
        self.distant_fake_sent_emails_manager = None
        self.fake_sent_emails_initial_lines_count = None


    def setUp(self):
        self.fake_sent_emails_manager = FakeSentEmailsManager(settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
        self.fake_sent_emails_manager.install_fake_sendmail_log_file()
        if settings.START_SERVER:
            remove_database_folder()
            self.server = initialize_server()
        self.browser = initialize_browser_for_scenario_2()


    def tearDown(self):
        self.browser.quit()
        if settings.START_SERVER:
            self.server.kill()
            remove_database_folder()
        remove_credentials_files(self.credential_file_id)
        # delete_election_data_snapshot(snapshot_folder)
        self.fake_sent_emails_manager.uninstall_fake_sendmail_log_file()
        if self.distant_fake_sent_emails_manager is not None:
            self.distant_fake_sent_emails_manager.uninstall_fake_sendmail_log_file()


    def credential_authority_sends_locally_generated_credentials_to_server(self):
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

        # She executes local (not server's) CLI belenios-tool to generate a number of credentials corresponding to the number of voters. This creates some local files.
        console_log("#### Starting step: belenios_tool_generate_credentials")
        self.credential_file_id = belenios_tool_generate_credentials(self.election_id)
        console_log("#### Step complete: belenios_tool_generate_credentials")
        console_log("#### Credential file id:", self.credential_file_id)

        # She uploads the file that corresponds to the public part of the genereated credentials. For this, she clicks on the 'Browse' button and selects the file with `.pubcreds` extension
        browse_button_css_selector = "form input[name=public_creds][type=file]"
        browse_button_element = wait_for_element_exists(browser, browse_button_css_selector)
        path_of_file_to_upload = self.credential_file_id + ".pubcreds"
        browse_button_element.clear()
        browse_button_element.send_keys(path_of_file_to_upload)

        # She clicks on 'Submit' button
        submit_button_css_selector = "form input[type=submit][value=Submit]"
        submit_button_element = wait_for_element_exists(browser, submit_button_css_selector)
        submit_button_element.click()

        wait_a_bit()

        # She checks that page contains title "Success"
        page_title_css_selector = "#header h1"
        page_title_expected_content = "Success"
        wait_for_element_exists_and_contains_expected_text(browser, page_title_css_selector, page_title_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)

        # She checks that page contains text "Credentials have been received and checked!"
        page_content_css_selector = "#main p"
        page_content_expected_content = "Credentials have been received and checked!"
        wait_for_element_exists_and_contains_expected_text(browser, page_content_css_selector, page_content_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)

        # We act like if credential authority had also sent each private credential to a different voter. For ease of parsing, we write these emails into the same text file as Belenios server's.
        self.credential_authority_sends_credentials_to_voters_from_credentials_file(self.credential_file_id + ".privcreds", self.voters_email_addresses)

        # She closes the browser window
        browser.quit()


    def generate_vote_ballots(self):
        invited_voters_who_will_vote = random.sample(self.voters_email_addresses, settings.NUMBER_OF_VOTING_VOTERS)
        invited_voters_who_will_vote_data = populate_credential_and_password_for_voters_from_sent_emails(self.distant_fake_sent_emails_manager, invited_voters_who_will_vote, settings.ELECTION_TITLE)
        invited_voters_who_will_vote_data = populate_random_votes_for_voters(invited_voters_who_will_vote_data)
        self.update_voters_data(invited_voters_who_will_vote_data)

        belenios_tool_generate_ballots(self.voters_data, self.credential_file_id, self.election_page_url)

    def export_all_votes_csv(self):
        generated_files_destination_folder = settings.GIT_REPOSITORY_ABSOLUTE_PATH # TODO: generate a temporary folder and remove it after load test has run
        csv_file_path = os.path.join(generated_files_destination_folder, 'all_votes.csv')
        with open(csv_file_path, 'w', newline='') as csvfile:
            csvwriter = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
            csvwriter.writerow(['voter_email_address', 'voter_password', 'voter_credential', 'voter_encrypted_ballot_file_path', 'election_page_url'])
            i = 0
            for k, v in self.voters_data.items():
                i += 1
                voter_email_address = k
                voter_password = v['password']
                voter_credential = v['credential']
                voter_crypted_ballot_file = "voter_row_" + str(i) + "_crypted_ballot.json"
                voter_encrypted_ballot_file_path = os.path.join(generated_files_destination_folder, voter_crypted_ballot_file)
                election_page_url = v['election_page_url']
                csvwriter.writerow([voter_email_address, voter_password, voter_credential, voter_encrypted_ballot_file_path, election_page_url])


    def download_all_sent_emails(self, target_fake_sent_emails_manager=None):
        from urllib.parse import urljoin
        import urllib.request
        if not target_fake_sent_emails_manager:
            target_fake_sent_emails_manager = FakeSentEmailsManager()
        distant_fake_emails_file_url = urljoin(settings.SERVER_URL, settings.FAKE_SENT_EMAILS_FILE_RELATIVE_URL) # TODO: maybe we should build this URL by picking link value in alert banner on distant server home page
        urllib.request.urlretrieve(distant_fake_emails_file_url, target_fake_sent_emails_manager.log_file_path)
        console_log("#### Distant fake sent emails have been saved in:", target_fake_sent_emails_manager.log_file_path)
        return target_fake_sent_emails_manager


    def test_load_testing_set_up(self):
        # Download server's sent emails text file, so that we know up to which line number we have to ignore its contents (this is its last line)
        try:
            temporary_fake_sent_emails_manager = self.download_all_sent_emails()
            self.fake_sent_emails_initial_lines_count = temporary_fake_sent_emails_manager.count_lines()
            console_log("### Initial lines count of server's fake sent emails file:", self.fake_sent_emails_initial_lines_count)
        finally:
            if temporary_fake_sent_emails_manager:
                temporary_fake_sent_emails_manager.uninstall_fake_sendmail_log_file()

        console_log("### Running test method BeleniosLoadTestingSetUp::test_load_testing_set_up()")
        console_log("### Starting step: administrator_starts_creation_of_manual_election")
        self.administrator_starts_creation_of_manual_election()
        console_log("### Step complete: administrator_starts_creation_of_manual_election")

        self.election_id = admin_election_draft_page_url_to_election_id(self.draft_election_administration_page_url)

        console_log("### Starting step: credential_authority_sends_locally_generated_credentials_to_server")
        self.credential_authority_sends_locally_generated_credentials_to_server()
        console_log("### Step complete: credential_authority_sends_locally_generated_credentials_to_server")

        console_log("### Starting step: administrator_invites_trustees")
        self.administrator_invites_trustees()
        console_log("### Step complete: administrator_invites_trustees")

        console_log("### Starting step: trustees_generate_election_private_keys")
        self.trustees_generate_election_private_keys()
        console_log("### Step complete: trustees_generate_election_private_keys")

        console_log("### Starting step: administrator_completes_creation_of_election")
        self.administrator_completes_creation_of_election()
        console_log("### Step complete: administrator_completes_creation_of_election")

        if settings.SERVER_URL == "http://localhost:8001":
            console_log("### Starting step: verify_election_consistency using `belenios_tool verify`")
            verify_election_consistency(self.election_id)
            console_log("### Step complete: verify_election_consistency using `belenios_tool verify`")

        console_log("### Starting step: download_all_sent_emails")
        self.distant_fake_sent_emails_manager = self.download_all_sent_emails()
        console_log("### Step complete: download_all_sent_emails")

        # Concatenate (distant) Belenios server's sent emails file (starting after line `fake_sent_emails_initial_lines_count`) and local credential authority's sent emails file into file `self.distant_fake_sent_emails_manager.log_file_path`, so that `self.generate_vote_ballots()` can parse it and find all information it needs.
        import subprocess
        import tempfile
        (file_handle, log_file_path) = tempfile.mkstemp(text=True)
        with open(log_file_path, 'w') as f:
            subprocess.run(["tail", "-n", "+" + str(self.fake_sent_emails_initial_lines_count + 1), self.distant_fake_sent_emails_manager.log_file_path], stdout=f)
            subprocess.run(["cat", self.fake_sent_emails_manager.log_file_path], stdout=f)
        subprocess.run(["cp", log_file_path, self.distant_fake_sent_emails_manager.log_file_path])
        subprocess.run(["rm", "-f", log_file_path])

        # Generate ballot for each voter
        console_log("### Starting step: generate_vote_ballots")
        self.generate_vote_ballots()
        console_log("### Step complete: generate_vote_ballots")

        # Export a CSV file to be imported by the jmeter load testing script. It contains fields 'voter_email_address', 'voter_password', 'voter_credential', 'voter_encrypted_ballot_file_path', 'election_page_url'
        console_log("### Starting step: export_all_votes_csv")
        self.export_all_votes_csv()
        console_log("### Step complete: export_all_votes_csv")


if __name__ == "__main__":
    random_seed = os.getenv('RANDOM_SEED', None)
    if not random_seed:
        random_seed = random.randrange(sys.maxsize)
    console_log("Python random seed being used:", random_seed)
    random.seed(random_seed)

    settings.SERVER_URL = os.getenv('SERVER_URL', settings.SERVER_URL)
    if os.getenv('START_SERVER', None):
        settings.START_SERVER = bool(strtobool(os.getenv('START_SERVER')))
    settings.FAKE_SENT_EMAILS_FILE_RELATIVE_URL = os.getenv('FAKE_SENT_EMAILS_FILE_RELATIVE_URL', settings.FAKE_SENT_EMAILS_FILE_RELATIVE_URL)

    if os.getenv('USE_HEADLESS_BROWSER', None):
        settings.USE_HEADLESS_BROWSER = bool(strtobool(os.getenv('USE_HEADLESS_BROWSER')))

    settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = os.getenv('SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH', settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    settings.WAIT_TIME_BETWEEN_EACH_STEP = float(os.getenv('WAIT_TIME_BETWEEN_EACH_STEP', settings.WAIT_TIME_BETWEEN_EACH_STEP))
    settings.EXPLICIT_WAIT_TIMEOUT = int(os.getenv('EXPLICIT_WAIT_TIMEOUT', settings.EXPLICIT_WAIT_TIMEOUT))
    settings.NUMBER_OF_INVITED_VOTERS = int(os.getenv('NUMBER_OF_INVITED_VOTERS', settings.NUMBER_OF_INVITED_VOTERS))
    settings.NUMBER_OF_VOTING_VOTERS = int(os.getenv('NUMBER_OF_VOTING_VOTERS', settings.NUMBER_OF_VOTING_VOTERS))
    settings.NUMBER_OF_REVOTING_VOTERS = int(os.getenv('NUMBER_OF_REVOTING_VOTERS', settings.NUMBER_OF_REVOTING_VOTERS))
    settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS = int(os.getenv('NUMBER_OF_REGENERATED_PASSWORD_VOTERS', settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS))
    settings.LOGIN_MODE = os.getenv('LOGIN_MODE', settings.LOGIN_MODE)
    settings.ADMINISTRATOR_USERNAME = os.getenv('ADMINISTRATOR_USERNAME', settings.ADMINISTRATOR_USERNAME)
    settings.ADMINISTRATOR_PASSWORD = os.getenv('ADMINISTRATOR_PASSWORD', settings.ADMINISTRATOR_PASSWORD)
    settings.ELECTION_TITLE = os.getenv('ELECTION_TITLE', settings.ELECTION_TITLE)
    settings.ELECTION_DESCRIPTION = os.getenv('ELECTION_DESCRIPTION', settings.ELECTION_DESCRIPTION)

    console_log("SERVER_URL:", settings.SERVER_URL)
    console_log("START_SERVER:", settings.START_SERVER)
    console_log("FAKE_SENT_EMAILS_FILE_RELATIVE_URL:", settings.FAKE_SENT_EMAILS_FILE_RELATIVE_URL)
    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH:", settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("NUMBER_OF_INVITED_VOTERS:", settings.NUMBER_OF_INVITED_VOTERS)
    console_log("NUMBER_OF_VOTING_VOTERS:", settings.NUMBER_OF_VOTING_VOTERS)
    console_log("NUMBER_OF_REVOTING_VOTERS:", settings.NUMBER_OF_REVOTING_VOTERS)
    console_log("NUMBER_OF_REGENERATED_PASSWORD_VOTERS:", settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)
    console_log("LOGIN_MODE:", settings.LOGIN_MODE)
    console_log("ELECTION_TITLE:", settings.ELECTION_TITLE)
    console_log("ELECTION_DESCRIPTION:", settings.ELECTION_DESCRIPTION)

    unittest.main()
