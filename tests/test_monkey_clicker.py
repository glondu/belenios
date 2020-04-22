#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
from urllib.parse import urljoin, urlsplit
# from html.parser import HTMLParser
from distutils.util import strtobool
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.election_testing import console_log, remove_database_folder, remove_election_from_database, initialize_server, wait_a_bit, populate_credential_and_password_for_voters_from_sent_emails, populate_random_votes_for_voters
from test_scenario_2 import BeleniosTestElectionScenario2Base, initialize_browser
import settings


# class LinkHTMLParser(HTMLParser):
#     def reset(self):
#         super().reset()
#         self.links = []

#     def handle_starttag(self, tag, attrs):
#         attributes = {attr_name: attr_value for attr_name, attr_value in attrs}

#         if tag == "a" and "href" in attributes:
#             # print("Found:", tag, attributes)
#             self.links.append(attributes["href"])


def get_link_element_url(link_element):
    return link_element.get_attribute('href')


def representation_of_element(element):
    return element.get_attribute("outerHTML")


def verify_fence(initial_url, href_value):
    """
    A kind of geofencing: We filter out URLs which are out of the scope of the test
    """
    target_url = urljoin(initial_url, href_value)

    # If this link points to a different host (domain name), we abort
    if urlsplit(target_url).hostname != urlsplit(initial_url).hostname:
        return False

    # If this link points to a downloadable element which works correctly for sure or which we don't want to test (for example because it would be tested too often or would take too much resources to download), we abort
    if "belenios.tar.gz" in target_url:
        return False
    return True


def fence_filter_generator(initial_page_url):
    def inner(link_element):
        link_url = get_link_element_url(link_element)
        return verify_fence(initial_page_url, link_url)
    return inner


def element_is_visible_filter(el):
    return el.is_displayed()


def get_all_visible_links(browser):
    all_links = browser.find_elements_by_css_selector("a[href]")

    displayed_links = list(filter(element_is_visible_filter, all_links))
    return displayed_links


def get_all_clickable_elements_in_page(browser, fence_filter):
    # TODO: add buttons, etc
    return get_all_clickable_links_in_page(browser, fence_filter) + get_all_input_type_submit_buttons_in_page(browser)


def get_all_clickable_links_in_page(browser, fence_filter):
    all_visible_links = get_all_visible_links(browser)
    # console_log("### all_visible_links:")
    # for link_element in all_visible_links:
    #     console_log(get_link_element_url(link_element))

    accepted_links = list(filter(fence_filter, all_visible_links))
    # console_log("### accepted_links:")
    # for link_element in accepted_links:
    #     console_log(get_link_element_url(link_element))
    return accepted_links


def get_all_input_type_submit_buttons_in_page(browser):
    all_input_type_submit_buttons = browser.find_elements_by_css_selector("button, input[type=submit]")
    # console_log("### all_input_type_submit_buttons:")
    # for element in all_input_type_submit_buttons:
    #     console_log(representation_of_element(element))
    displayed_elements = list(filter(element_is_visible_filter, all_input_type_submit_buttons))
    # console_log("### displayed_elements:")
    # for element in displayed_elements:
    #     console_log(representation_of_element(element))
    return displayed_elements


def verify_page_is_not_an_error_page(browser):
    # Belenios web server returns a "Unauthorized" "Error 401" page in several situations, for example when we pick the "local" login method and submit an empty login form. For now, we consider this behaviour as normal.
    # But what we consider an unexpected error is other types of errors returned by the server, for example "Internal Server Error", "Error 500".
    error_content = ["Internal Server Error", "Error 500"]
    page_source = browser.page_source
    for content in error_content:
        if content in page_source:
            raise Exception("Server returned an unexpected error page. Page content was:", page_source)


class SeleniumMonkeyClicker():
    browser = None
    initial_page_url = None

    def __init__(self, browser, initial_page_url):
        self.browser = browser
        self.initial_page_url = initial_page_url

    def go_back(self):
        self.browser.back()
        wait_a_bit()

    def start(self, maximum_actions_in_visit=100):
        probability_to_go_back = 0.25
        probability_to_go_back_when_dead_end = 1 # 0.25

        console_log("## First action in visit goes to page:", self.initial_page_url)
        self.browser.get(self.initial_page_url)
        current_actions_in_visit = 1

        fence_filter = fence_filter_generator(self.initial_page_url)

        while current_actions_in_visit < maximum_actions_in_visit:
            current_actions_in_visit += 1
            console_log("## current_actions_in_visit:", current_actions_in_visit)
            verify_page_is_not_an_error_page(self.browser)
            random_result = random.random()
            if random_result < probability_to_go_back:
                if current_actions_in_visit >= 2:
                    console_log("### Deciding to go back")
                    self.go_back()
            else:
                clickable_elements = get_all_clickable_elements_in_page(self.browser, fence_filter)
                if not len(clickable_elements):
                    console_log("### No more clickable element to click on.")
                    random_result2 = random.random()
                    if random_result2 < probability_to_go_back_when_dead_end:
                        console_log("### Deciding to go back")
                        self.go_back()
                        continue
                    else:
                        console_log("### Deciding to end visit here.")
                        break
                else:
                    selected_element = random.choice(clickable_elements)
                    console_log("### We choose randomly this element:", representation_of_element(selected_element))
                    selected_element.click()
                    wait_a_bit()

        console_log("### SeleniumMonkeyClicker visit is now complete.")



def get_election_url(election_id):
    return "/".join([settings.SERVER_URL, "elections", election_id, ""])


class BeleniosMonkeyTestClicker(BeleniosTestElectionScenario2Base):

    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)
        self.distant_fake_sent_emails_manager = None
        self.fake_sent_emails_initial_lines_count = None


    def setUp(self):
        self.fake_sent_emails_manager = FakeSentEmailsManager(settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
        self.fake_sent_emails_manager.install_fake_sendmail_log_file()
        if settings.START_SERVER:
            if settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_DATABASE:
                remove_database_folder()
            elif settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_ELECTION or settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.DO_NOTHING:
                pass
            self.server = initialize_server()
        self.browser = initialize_browser()
        if settings.ELECTION_ID:
            self.election_id = settings.ELECTION_ID
        else:
            # Download server's sent emails text file, so that we know up to which line number we have to ignore its contents (this is its last line)
            temporary_fake_sent_emails_manager = None
            try:
                temporary_fake_sent_emails_manager = self.download_all_sent_emails()
                self.fake_sent_emails_initial_lines_count = temporary_fake_sent_emails_manager.count_lines()
                console_log("### Initial lines count of server's fake sent emails file:", self.fake_sent_emails_initial_lines_count)
            finally:
                if temporary_fake_sent_emails_manager:
                    temporary_fake_sent_emails_manager.uninstall_fake_sendmail_log_file()

            self.administrator_creates_election()

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

            invited_voters_who_will_vote = random.sample(self.voters_email_addresses, settings.NUMBER_OF_VOTING_VOTERS)
            invited_voters_who_will_vote_data = populate_credential_and_password_for_voters_from_sent_emails(self.distant_fake_sent_emails_manager, invited_voters_who_will_vote, settings.ELECTION_TITLE)
            invited_voters_who_will_vote_data = populate_random_votes_for_voters(invited_voters_who_will_vote_data)
            self.update_voters_data(invited_voters_who_will_vote_data)

            settings.VOTER_USERNAME = invited_voters_who_will_vote_data[0]["username"]
            settings.VOTER_PASSWORD = invited_voters_who_will_vote_data[0]["password"]
        console_log("Going to vote as VOTER_USERNAME:", settings.VOTER_USERNAME)
        console_log("Going to vote as VOTER_PASSWORD:", settings.VOTER_PASSWORD)


    def tearDown(self):
        self.browser.quit()
        if settings.START_SERVER:
            self.server.kill()
            if settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_DATABASE:
                remove_database_folder()
            elif settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.REMOVE_ELECTION:
                if self.election_id:
                    remove_election_from_database(self.election_id)
            elif settings.CLEAN_UP_POLICY == settings.CLEAN_UP_POLICIES.DO_NOTHING:
                pass
        self.fake_sent_emails_manager.uninstall_fake_sendmail_log_file()
        if self.distant_fake_sent_emails_manager is not None:
            self.distant_fake_sent_emails_manager.uninstall_fake_sendmail_log_file()


    def download_all_sent_emails(self, target_fake_sent_emails_manager=None):
        from urllib.parse import urljoin
        import urllib.request
        if not target_fake_sent_emails_manager:
            import tempfile
            (file_handle, log_file_path) = tempfile.mkstemp(text=True)
            target_fake_sent_emails_manager = FakeSentEmailsManager(log_file_path)
        distant_fake_emails_file_url = urljoin(settings.SERVER_URL, settings.FAKE_SENT_EMAILS_FILE_RELATIVE_URL) # TODO: maybe we should build this URL by picking link value in alert banner on distant server home page
        console_log("distant_fake_emails_file_url:", distant_fake_emails_file_url)
        urllib.request.urlretrieve(distant_fake_emails_file_url, target_fake_sent_emails_manager.log_file_path)
        console_log("#### Distant fake sent emails have been saved in:", target_fake_sent_emails_manager.log_file_path)
        return target_fake_sent_emails_manager


    def test_monkey_clicker(self):
        browser = self.browser
        console_log("# test_monkey_clicker()")
        console_log("## Going to election page")
        election_url = get_election_url(self.election_id)

        monkey = SeleniumMonkeyClicker(browser, election_url)
        monkey.start(100)



if __name__ == "__main__":
    if not hasattr(settings, "LOGIN_MODE"):
        settings.LOGIN_MODE = "local"
    if not hasattr(settings, "START_SERVER"):
        settings.START_SERVER = True

    random_seed = os.getenv('RANDOM_SEED', None)
    if not random_seed:
        random_seed = random.randrange(sys.maxsize)
    console_log("Python random seed being used:", random_seed)
    random.seed(random_seed)

    settings.SERVER_URL = os.getenv('SERVER_URL', settings.SERVER_URL)
    if os.getenv('START_SERVER', None):
        settings.START_SERVER = bool(strtobool(os.getenv('START_SERVER')))

    if os.getenv('USE_HEADLESS_BROWSER', None):
        settings.USE_HEADLESS_BROWSER = bool(strtobool(os.getenv('USE_HEADLESS_BROWSER')))

    settings.ELECTION_ID = os.getenv('ELECTION_ID', None) or None
    settings.VOTER_USERNAME = os.getenv('VOTER_USERNAME', None) or None
    settings.VOTER_PASSWORD = os.getenv('VOTER_PASSWORD', None) or None
    settings.FAKE_SENT_EMAILS_FILE_RELATIVE_URL = os.getenv('FAKE_SENT_EMAILS_FILE_RELATIVE_URL', "static/mail.txt")

    settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = os.getenv('SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH', settings.SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH)
    settings.WAIT_TIME_BETWEEN_EACH_STEP = float(os.getenv('WAIT_TIME_BETWEEN_EACH_STEP', settings.WAIT_TIME_BETWEEN_EACH_STEP)) # Do not set a value below 0.02 seconds, otherwise hypothesis test becomes flaky.
    settings.EXPLICIT_WAIT_TIMEOUT = int(os.getenv('EXPLICIT_WAIT_TIMEOUT', settings.EXPLICIT_WAIT_TIMEOUT))
    if os.getenv('CLEAN_UP_POLICY', None):
        input_clean_up_policy = os.getenv('CLEAN_UP_POLICY')
        if hasattr(settings.CLEAN_UP_POLICIES, input_clean_up_policy):
            settings.CLEAN_UP_POLICY = getattr(settings.CLEAN_UP_POLICIES, input_clean_up_policy)
        else:
            raise Exception("Error: Unknown value for CLEAN_UP_POLICY:", input_clean_up_policy)

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
