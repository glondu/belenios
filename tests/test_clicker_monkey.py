#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
import time
from urllib.parse import urljoin, urlsplit
# from html.parser import HTMLParser
from distutils.util import strtobool
from selenium.webdriver.common.alert import Alert
from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.election_testing import console_log, remove_database_folder, remove_election_from_database, initialize_server, wait_a_bit, populate_credential_and_password_for_voters_from_sent_emails, populate_random_votes_for_voters, try_several_times
from util.selenium_tools import wait_for_an_element_with_link_text_exists, wait_for_element_exists, wait_for_element_exists_and_contains_expected_text, wait_for_element_exists_and_has_non_empty_content
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

class SeleniumPageObjectModel():
    browser = None


    def __init__(self, browser, timeout):
        self.browser = browser
        self.timeout = timeout


class VerifiablePage(SeleniumPageObjectModel):
    def verify_page(self):
        pass


class VoterLoginPage(VerifiablePage):
    login_form_username_css_selector = '#main form input[name=username]'
    login_form_password_css_selector = '#main form input[name=password]'


    def verify_page(self):
        find_visible_element_which_contains_expected_text(self.browser, "h1", "Password login", self.timeout)


    def log_in(self, username, password):
        # She enters her identifier and password and submits the form to log in
        login_form_username_value = username # correct value: settings.ADMINISTRATOR_USERNAME
        login_form_password_value = password # correct value: settings.ADMINISTRATOR_PASSWORD

        login_form_username_element = wait_for_element_exists(self.browser, self.login_form_username_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        login_form_password_element = wait_for_element_exists(self.browser, self.login_form_password_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)

        login_form_username_element.send_keys(login_form_username_value)
        login_form_password_element.send_keys(login_form_password_value)

        wait_a_bit()

        login_form_password_element.submit()

        wait_a_bit()


class ElectionHomePage(VerifiablePage):
    def click_on_language_link(self, language_link_label):
        language_link_element = wait_for_an_element_with_link_text_exists(self.browser, language_link_label, self.timeout)
        language_link_element.click()


    def click_on_start_button(self):
        start_button_label = "Start"
        start_button_css_selector = "#main button"
        start_button_element = wait_for_element_exists_and_contains_expected_text(self.browser, start_button_css_selector, start_button_label, self.timeout)
        start_button_element.click()

    # TODO: verify_page, click_on_advanced_mode_link, click_on_see_accepted_ballots_link, etc.


class NormalVoteGenericStepPage(VerifiablePage):
    current_step_css_selector = ".current_step"
    expected_step_content = "Step"


    def verify_step_title(self):
        console_log(f"**Verifying that current step contains '{self.expected_step_content}")
        find_visible_element_which_contains_expected_text(self.browser, self.current_step_css_selector, self.expected_step_content, self.timeout)


    def verify_page(self):
        self.verify_step_title()


class NormalVoteStep1Page(NormalVoteGenericStepPage):
    expected_step_content = "Step 1/6: Input credential"


    def click_on_here_button_and_type_voter_credential(self, voter_credential):
        here_button_label = "here"
        here_button_css_selector = "#main button"
        here_button_element = wait_for_element_exists_and_contains_expected_text(self.browser, here_button_css_selector, here_button_label, self.timeout)
        here_button_element.click()

        wait_a_bit()

        # A modal opens (it is an HTML modal created using Window.prompt()), with an input field. He types his credential.
        credential_prompt = Alert(self.browser)
        credential_prompt.send_keys(voter_credential)
        credential_prompt.accept()


class NormalVoteStep2Page(NormalVoteGenericStepPage):
    expected_step_content = "Step 2/6: Answer to questions"


    def click_on_next_button(self):
        step_2_parent_css_selector = "#question_div"
        next_button = find_visible_element_which_contains_expected_text(self.browser, step_2_parent_css_selector + " button", "Next", self.timeout)
        next_button.click()


class NormalVoteGenericStepWithBallotTrackerPage(NormalVoteGenericStepPage):
    def get_smart_ballot_tracker_value(self):
        smart_ballot_tracker_css_selector = "#ballot_tracker"
        smart_ballot_tracker_element = wait_for_element_exists_and_has_non_empty_content(self.browser, smart_ballot_tracker_css_selector, self.timeout)
        smart_ballot_tracker_value = smart_ballot_tracker_element.get_attribute('innerText')
        return smart_ballot_tracker_value


    def verify_ballot_tracker_value(self):
        smart_ballot_tracker_value = self.get_smart_ballot_tracker_value()
        assert len(smart_ballot_tracker_value) > 5


class NormalVoteStep3Page(NormalVoteGenericStepWithBallotTrackerPage):
    expected_step_content = "Step 3/6: Review and encrypt"


    def verify_page_body(self):
        step_3_parent_css_selector = "#ballot_div"
        step_3_expected_success_content = "Your ballot has been successfully encrypted"
        find_visible_element_which_contains_expected_text(self.browser, step_3_parent_css_selector, step_3_expected_success_content, self.timeout)
        self.verify_ballot_tracker_value()


    def verify_page(self):
        NormalVoteGenericStepWithBallotTrackerPage.verify_page(self)
        self.verify_page_body()


    def click_on_continue_button(self):
        continue_button = find_visible_element_and_attribute_contains_expected_text(self.browser, "input[type=submit]", "value", "Continue", self.timeout)
        continue_button.click()

    # TODO: click_on_restart_button


class NormalVoteStep5Page(NormalVoteGenericStepWithBallotTrackerPage):
    expected_step_content = "Step 5/6: Confirm"


    def verify_page_body(self, expected_ballot_tracker, expected_username):
        step_5_parent_css_selector = "#main"
        step_5_expected_success_content = "has been received, but not recorded yet"
        find_visible_element_which_contains_expected_text(self.browser, step_5_parent_css_selector, step_5_expected_success_content, self.timeout)
        self.verify_ballot_tracker_value()
        ballot_tracker_value = self.get_smart_ballot_tracker_value()
        assert ballot_tracker_value == expected_ballot_tracker

        find_visible_element_which_contains_expected_text(self.browser, step_5_parent_css_selector, expected_username, self.timeout)


    def verify_page(self, expected_ballot_tracker, expected_username):
        NormalVoteGenericStepWithBallotTrackerPage.verify_page(self)
        self.verify_page_body(expected_ballot_tracker, expected_username)


    def click_on_i_cast_my_vote_button(self):
        i_cast_my_vote_button_label = "I cast my vote"
        i_cast_my_vote_button_element = find_visible_element_and_attribute_contains_expected_text(self.browser, "input[type=submit]", "value", i_cast_my_vote_button_label, self.timeout)
        i_cast_my_vote_button_element.click()

    # TODO: click_on_go_back_to_election_link


class NormalVoteStep6Page(NormalVoteGenericStepWithBallotTrackerPage):
    expected_step_content = "Step 6/6: Thank you for voting!"


    def verify_page_body(self, expected_ballot_tracker):
        step_6_parent_css_selector = "#main"
        expected_step_6_body_content = "has been accepted"
        find_visible_element_which_contains_expected_text(self.browser, step_6_parent_css_selector, expected_step_6_body_content, self.timeout)
        self.verify_ballot_tracker_value()
        ballot_tracker_value = self.get_smart_ballot_tracker_value()
        assert ballot_tracker_value == expected_ballot_tracker


    def verify_page(self, expected_ballot_tracker):
        NormalVoteGenericStepWithBallotTrackerPage.verify_page(self)
        self.verify_page_body(expected_ballot_tracker)


    def click_on_ballot_box_link(self):
        ballot_box_label = "ballot box"
        ballot_box_link_element = wait_for_an_element_with_link_text_exists(self.browser, ballot_box_label)
        ballot_box_link_element.click()

    # TODO: click_on_go_back_to_election_link


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
    return get_all_clickable_links_in_page(browser, fence_filter) + get_all_input_type_submit_buttons_in_page(browser)


def get_all_clickable_links_in_page(browser, fence_filter):
    all_visible_links = get_all_visible_links(browser)
    accepted_links = list(filter(fence_filter, all_visible_links))
    return accepted_links


def get_all_input_type_submit_buttons_in_page(browser):
    all_input_type_submit_buttons = browser.find_elements_by_css_selector("button, input[type=submit]")
    displayed_elements = list(filter(element_is_visible_filter, all_input_type_submit_buttons))
    return displayed_elements


def get_all_input_type_checkbox_elements(browser_or_parent_element):
    all_input_type_checkbox = browser_or_parent_element.find_elements_by_css_selector("input[type=checkbox]")
    displayed_elements = list(filter(element_is_visible_filter, all_input_type_checkbox))
    return displayed_elements


def verify_page_is_not_an_error_page(browser):
    # Belenios web server returns a "Unauthorized" "Error 401" page in several situations, for example when we pick the "local" login method and submit an empty login form. For now, we consider this behaviour as normal.
    # But what we consider an unexpected error is other types of errors returned by the server, for example "Internal Server Error", "Error 500".
    error_content = ["Internal Server Error", "Error 500"]
    page_source = browser.page_source
    for content in error_content:
        if content in page_source:
            page_source = str(browser.page_source.encode("utf-8"))
            raise Exception(f"Server returned an unexpected error page. Page source was: {page_source}")


class SeleniumClickerMonkey():
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

        console_log("### SeleniumClickerMonkey visit is now complete.")


class SeleniumFormFillerMonkey():
    browser = None
    form_css_selector = None


    def __init__(self, browser, form_css_selector="form"):
        self.browser = browser
        self.form_css_selector = form_css_selector


    def fill_form(self):
        form_element = self.browser.find_element_by_css_selector(self.form_css_selector)

        all_input_type_checkbox_elements = get_all_input_type_checkbox_elements(form_element)
        # v1: This does not work when monkey checks zero checkbox, because in this case Belenios displays an Alert saying that voter is forced to check at least one checkbox
        # probability_to_click_a_checkbox = 0.5
        # for element in all_input_type_checkbox_elements:
        #     random_result = random.random()
        #     console_log("fill_form random_result:", random_result)
        #     if random_result < probability_to_click_a_checkbox:
        #         console_log("clicking element", representation_of_element(element))
        #         element.click()

        # v2: Define a random number of checkboxes to check, X, between 1 and the number of checkboxes. Pick X checkboxes at random and check them.
        if len(all_input_type_checkbox_elements) > 0:
            number_of_checkboxes_to_check = random.randint(1, len(all_input_type_checkbox_elements))
            checkboxes_to_check = random.sample(all_input_type_checkbox_elements, number_of_checkboxes_to_check)
            for element in checkboxes_to_check:
                console_log("clicking element", representation_of_element(element))
                element.click()

        # TODO: handle other types of form fields (examples: input[type=text], input[type=password], textarea, input[type=file], input[type=radio])


    # def click_on_submit_button(self):
    #     form_element = self.browser.find_element_by_css_selector(self.form_css_selector)
    #     submit_button = form_element.find_element_by_css_selector("input[type=submit]")
    #     submit_button.click()


def get_election_url(election_id):
    return "/".join([settings.SERVER_URL, "elections", election_id, ""])


@try_several_times(max_attempts=3, sleep_duration=3)
def find_visible_element_and_attribute_contains_expected_text(browser, css_selector, attribute_name, expected_content, timeout):
    # In normal vote page, several elements have the "current_step" CSS class, and they have different `innerText`. The actual current step on screen corresponds to the `innerText` value of such an element which is visible (that is which has no ancestor which has `style="display: none;"`)
    all_current_step_elements = browser.find_elements_by_css_selector(css_selector)
    visible_current_step_elements = filter(element_is_visible_filter, all_current_step_elements)
    for element in visible_current_step_elements:
        if expected_content in element.get_attribute(attribute_name):
            return element
    page_source = str(browser.page_source.encode("utf-8"))
    raise Exception(f"Page does not contain a visible element which would have an attribute '{attribute_name}' valued '{expected_content}'. Page source was: {page_source}")


def find_visible_element_which_contains_expected_text(browser, css_selector, expected_content, timeout):
    return find_visible_element_and_attribute_contains_expected_text(browser, css_selector, 'innerText', expected_content, timeout)


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

            selected_voter = invited_voters_who_will_vote_data[0]
            settings.VOTER_USERNAME = selected_voter["username"]
            settings.VOTER_PASSWORD = selected_voter["password"]
            settings.VOTER_CREDENTIAL = selected_voter["credential"]
        console_log("Going to vote as VOTER_USERNAME:", settings.VOTER_USERNAME)
        console_log("Going to vote as VOTER_PASSWORD:", settings.VOTER_PASSWORD)
        console_log("Going to vote as VOTER_CREDENTIAL:", settings.VOTER_CREDENTIAL)


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


    @unittest.skip("Bug discovered using Monkey Testing: Logging in and clicking browser's Back button and logging in again produces an Internal Server Error")
    def test_clicker_monkey_on_election_home(self):
        console_log("# test_clicker_monkey_on_election_home()")
        browser = self.browser
        election_url = get_election_url(self.election_id)
        console_log("## Going to election page:", election_url)

        monkey = SeleniumClickerMonkey(browser, election_url)
        monkey.start(100)


    def test_sometimes_smart_monkey_votes(self):
        console_log("# test_clicker_monkey_after_login()")
        browser = self.browser
        timeout = settings.EXPLICIT_WAIT_TIMEOUT
        election_url = get_election_url(self.election_id)
        console_log("## Going to election page:", election_url)
        browser.get(election_url)

        wait_a_bit()

        console_log("## Starting monkey behaviour for a few clicks:")
        monkey = SeleniumClickerMonkey(browser, election_url)
        monkey.start(3)

        console_log("## Going to election page again", election_url)
        browser.get(election_url)

        wait_a_bit()

        console_log("## Clicking on 'en' language link")
        election_home_page = ElectionHomePage(browser, timeout)
        election_home_page.click_on_language_link("en")

        wait_a_bit()

        console_log("## Clicking on 'Start' button")
        election_home_page.click_on_start_button()

        wait_a_bit()

        console_log("## Verifying that we are on step 1 page")
        step_1_page = NormalVoteStep1Page(browser, timeout)
        step_1_page.verify_page()

        # Here:
        # We cannot have a monkey behaviour, because there is only one button to click (the "here" button).
        # We can go back. This goes back to election home page

        console_log("## Clicking on 'here' button")
        step_1_page.click_on_here_button_and_type_voter_credential(settings.VOTER_CREDENTIAL)

        wait_a_bit()

        step_2_page = NormalVoteStep2Page(browser, timeout)
        step_2_page.verify_page()

        # Here:
        # We can check any checkbox for the question (check 0 to n checkboxes)
        # We can click on the "Next" button
        # We can go back. This would go back to the election home page (not to the "Step 1" page, which would probably have been the intuitive behaviour)

        console_log("## Answering vote question by checking randomly some checkboxes")
        step_2_parent_css_selector = "#question_div"
        form_filler_monkey = SeleniumFormFillerMonkey(browser, step_2_parent_css_selector) # Warning: In the DOM of the vote page, step 2, checkboxes are not in a `<form>`.
        form_filler_monkey.fill_form()

        console_log("## Click on the 'Next' button")
        step_2_page.click_on_next_button()

        wait_a_bit()

        console_log("## Verify that we are on step 3 and that page content is correct (ballot tracker is not empty)")
        step_3_page = NormalVoteStep3Page(browser, timeout)
        step_3_page.verify_page()
        step_3_smart_ballot_tracker_value = step_3_page.get_smart_ballot_tracker_value()

        # Here:
        # We can click on the "Continue" button (`<input style="font-size:30px;" value="Continue" type="submit">`)
        # We can click on the "Restart" button (`<button onclick="location.reload();">Restart</button>`). This goes back to step 1.

        console_log("## Click on the 'Continue' button")
        step_3_page.click_on_continue_button()

        wait_a_bit()

        # We arrive on the login form (if we have not already logged in during this visit, which could happen if we do a complex navigation after a first login. If we have already logged in, we arrive directly on the step 5 page)
        console_log("## Verify that we are on login page")
        login_page = VoterLoginPage(browser, timeout)
        login_page.verify_page()

        # Here:
        # We can click on the "Login" button without filling the username nor password field
        # We can click on the "Login" button after having filled the username and password fields with wrong data
        # We can click on the "Login" button after having filled the username and password fields with correct data
        # We can go back. This goes back to step 3.

        # If we don't fill the form, or fill the form with wrong username/password, and click on the "Login" button, we arrive on an "Unauthorized" "Error 401" page.

        # If we fill the form with correct data and click on the "Login" button, we arrive on step 5 page.

        console_log("## Filling log in form and submitting it")
        login_page.log_in(settings.VOTER_USERNAME, settings.VOTER_PASSWORD)

        console_log("## Verify that we are on step 5 and that page content is correct (page contains 'has been received, but not recorded yet'; page contains a ballot tracker which is not empty; page contains voter's username)")
        step_5_page = NormalVoteStep5Page(browser, timeout)
        step_5_page.verify_page(step_3_smart_ballot_tracker_value, settings.VOTER_USERNAME)

        # Here:
        # We can click on the Belenios logo on the top-left of the screen
        # We can click on a link in the footer
        # We can click on the "I cast my vote" button
        # We can click on the "Go back to election" link
        # We can go back. This goes back to the Login page which immediately redirects to this same step 5 page.

        console_log("Click on the 'I cast my vote' button")
        step_5_page.click_on_i_cast_my_vote_button()

        wait_a_bit()

        console_log("## Verify that we are on step 6 and that page content is correct (page contains 'has been accepted'; page contains a ballot tracker which is not empty)")
        step_6_page = NormalVoteStep6Page(browser, timeout)
        step_6_page.verify_page(step_3_smart_ballot_tracker_value)

        # Here:
        # We can click on the Belenios logo on the top-left of the screen
        # We can click on a link in the footer
        # We can click on the "ballot box" link
        # We can click on the "Go back to election" link
        # We can go back. This goes to another page which looks like the "Advanced mode" page. This looks like a small bug.

        console_log("Click on the 'ballot box' link")
        step_6_page.click_on_ballot_box_link()

        wait_a_bit()

        console_log("Verify that ballot box page contains a link labelled as voter's smart ballot tracker, and click on it")
        my_ballot_link = wait_for_an_element_with_link_text_exists(browser, step_3_smart_ballot_tracker_value)
        my_ballot_link.click()
        verify_page_is_not_an_error_page(browser)


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
    settings.VOTER_CREDENTIAL = os.getenv('VOTER_CREDENTIAL', None) or None
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
