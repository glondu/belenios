#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
import time
from distutils.util import strtobool

from util.fake_sent_emails_manager import FakeSentEmailsManager
from util.election_testing import remove_database_folder, remove_election_from_database, initialize_server, wait_a_bit, populate_credential_and_password_for_voters_from_sent_emails, populate_random_votes_for_voters
from util.page_objects import ElectionHomePage, NormalVoteStep1Page, NormalVoteStep2Page, NormalVoteStep3Page, VoterLoginPage, NormalVoteStep5Page, NormalVoteStep6Page, BallotBoxPage, UnauthorizedPage, ServerHomePage, AdvancedModeVotePage
from util.monkeys import SeleniumClickerMonkey, SeleniumFormFillerMonkey, StateForSmartMonkey, SmartMonkeyWithMemoryAndKnownStateMachine
from util.execution import console_log
from test_scenario_2 import BeleniosTestElectionScenario2Base, initialize_browser
import settings


def verify_page_is_not_an_error_page(browser):
    # Belenios web server returns a "Unauthorized" "Error 401" page in several situations, for example when we pick the "local" login method and submit an empty login form. For now, we consider this behaviour as normal.
    # But what we consider an unexpected error is other types of errors returned by the server, for example "Internal Server Error", "Error 500".
    error_content = ["Internal Server Error", "Error 500"]
    page_source = browser.page_source
    for content in error_content:
        if content in page_source:
            page_source = str(browser.page_source.encode("utf-8"))
            raise Exception(f"Server returned an unexpected error page. Page source was: {page_source}")


def get_election_url(election_id):
    return "/".join([settings.SERVER_URL, "elections", election_id, ""])


class ElectionHomePageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = ElectionHomePage(self.browser, self.timeout)

    def click_on_start_button_generator(self):
        def click_on_start_button(in_memory=None):
            self.page.click_on_language_link("en") # For the rest of the test, we need language to be English, because DOM selectors for links and buttons use English language
            self.page.click_on_start_button()
            return NormalVoteStep1PageState(self.browser, self.timeout, ElectionHomePageState)
        return click_on_start_button

    def get_all_possible_actions(self):
        def click_on_start_button(in_memory=None):
            self.page.click_on_language_link("en") # For the rest of the test, we need language to be English, because DOM selectors for links and buttons use English language
            self.page.click_on_start_button()
            return NormalVoteStep1PageState(self.browser, self.timeout, ElectionHomePageState)

        def click_on_en_language_link(in_memory=None):
            self.page.click_on_language_link("en")
            return self

        def click_on_fr_language_link(in_memory=None):
            self.page.click_on_language_link("fr")
            return self

        def click_on_accept_personal_data_policy_link(in_memory=None):
            try:
                if self.browser.find_element_by_link_text("Accept"):
                    self.page.click_on_accept_personal_data_policy_link()
            finally:
                return self

        def click_on_advanced_mode_link(in_memory=None):
            self.page.click_on_advanced_mode_link()
            return AdvancedModeVotePageState(self.browser, self.timeout, ElectionHomePageState)

        def click_on_see_accepted_ballots_link(in_memory=None):
            self.page.click_on_see_accepted_ballots_link()
            return BallotBoxPageState(self.browser, self.timeout, ElectionHomePageState)

        return [
            click_on_start_button,
            click_on_en_language_link,
            click_on_fr_language_link,
            click_on_accept_personal_data_policy_link,
            click_on_advanced_mode_link,
            click_on_see_accepted_ballots_link,
        ]


class NormalVoteStep1PageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        # console_log("NormalVoteStep1PageState::__init__() with", args, kwargs)
        super().__init__(*args, **kwargs)
        self.page = NormalVoteStep1Page(self.browser, self.timeout)


    def get_all_possible_actions(self):
        def click_on_here_button_and_type_correct_voter_credential(in_memory=None):
            self.page.click_on_here_button_and_type_voter_credential(in_memory["voter_credential"])
            return NormalVoteStep2PageState(self.browser, self.timeout, ElectionHomePageState) # Why previous state of step2 is not step1 ?

        def click_on_here_button_and_type_wrong_voter_credential(in_memory=None):
            self.page.click_on_here_button_and_type_wrong_voter_credential("aaa") # TODO: randomize input (fuzz). Also sometimes the second alert message is not caught, so maybe we should create a wait_* function for alerts
            return self

        return [
            click_on_here_button_and_type_correct_voter_credential,
            click_on_here_button_and_type_wrong_voter_credential
        ]


class NormalVoteStep2PageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = NormalVoteStep2Page(self.browser, self.timeout)
        self.form_has_been_filled = False


    def get_all_possible_actions(self):
        def click_on_next_button(in_memory=None):
            if self.form_has_been_filled:
                self.page.click_on_next_button()
                return NormalVoteStep3PageState(self.browser, self.timeout, ElectionHomePageState) # Why previous state of step2 is not step1 ?
            else:
                self.page.click_on_next_button_but_form_is_not_filled()
                return self

        def fill_form(in_memory=None):
            step_2_parent_css_selector = "#question_div"
            form_filler_monkey = SeleniumFormFillerMonkey(self.browser, step_2_parent_css_selector) # Warning: In the DOM of the vote page, step 2, checkboxes are not in a `<form>`.
            form_filler_monkey.fill_form()
            self.form_has_been_filled = True
            return self
        return [
            click_on_next_button,
            fill_form
        ]


class NormalVoteStep3PageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = NormalVoteStep3Page(self.browser, self.timeout)
        self.smart_ballot_tracker = None


    def save_smart_ballot_tracker_value(self, in_memory):
        self.smart_ballot_tracker = self.page.get_smart_ballot_tracker_value()
        in_memory["voter_temporary_smart_ballot_tracker"] = self.smart_ballot_tracker


    def get_all_possible_actions(self):
        def click_on_restart_button(in_memory):
            self.page.click_on_restart_button()
            return NormalVoteStep1PageState(self.browser, self.timeout, ElectionHomePageState)

        def click_on_continue_button(in_memory):
            self.save_smart_ballot_tracker_value(in_memory)
            self.page.click_on_continue_button()
            if in_memory.get('voter_has_logged_in', False):
                return NormalVoteStep5PageState(self.browser, self.timeout, NormalVoteStep1PageState) # Why previous state of step5 is not step3? (or login step?)
            else:
                return NormalVoteLoginPageState(self.browser, self.timeout, NormalVoteStep1PageState) # Why previous state of login step (which is the fourth step) is not step3?

        return [
            click_on_restart_button,
            click_on_continue_button,
        ]


class NormalVoteLoginPageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = VoterLoginPage(self.browser, self.timeout)
        self.form_is_filled_with_correct_data = False


    def get_all_possible_actions(self):
        def fill_form_with_wrong_data(in_memory=None):
            self.page.fill_form("aaa", "aaa") # TODO: randomize input (fuzz)
            self.form_is_filled_with_correct_data = False
            return self

        def fill_form_with_correct_data(in_memory):
            self.page.fill_form(in_memory["voter_username"], in_memory["voter_password"])
            self.form_is_filled_with_correct_data = True
            return self

        def click_on_login_button(in_memory=None):
            self.page.click_on_login_button()
            if self.form_is_filled_with_correct_data:
                in_memory["voter_has_logged_in"] = True
                return NormalVoteStep5PageState(self.browser, self.timeout, ElectionHomePageState) # Why previous state of step5 is not step3?
            else:
                return UnauthorizedPageState(self.browser, self.timeout, NormalVoteLoginPageState)

        def click_on_logo_image(in_memory=None):
            self.page.click_on_logo_image()
            return ServerHomePageState(self.browser, self.timeout, NormalVoteLoginPageState)

        return [
            fill_form_with_wrong_data,
            fill_form_with_correct_data,
            click_on_login_button,
            click_on_logo_image,
        ]


class NormalVoteStep5PageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = NormalVoteStep5Page(self.browser, self.timeout)


    def verify_page(self, in_memory):
        console_log("NormalVoteStep5PageState::verify_page()")
        return self.page.verify_page(in_memory["voter_temporary_smart_ballot_tracker"], in_memory["voter_username"])


    def get_all_possible_actions(self):
        def click_on_i_cast_my_vote_button(in_memory=None):
            self.page.click_on_i_cast_my_vote_button()
            in_memory["voter_validated_smart_ballot_tracker"] = in_memory["voter_temporary_smart_ballot_tracker"]
            return NormalVoteStep6PageState(self.browser, self.timeout)

        def click_on_go_back_to_election_link(in_memory=None):
            self.page.click_on_go_back_to_election_link()
            return ElectionHomePageState(self.browser, self.timeout, NormalVoteStep5PageState)

        def click_on_logo_image(in_memory=None):
            self.page.click_on_logo_image()
            return ServerHomePageState(self.browser, self.timeout, NormalVoteStep5PageState)

        return [
            click_on_i_cast_my_vote_button,
            click_on_go_back_to_election_link,
            click_on_logo_image,
        ]


class UnauthorizedPageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = UnauthorizedPage(self.browser, self.timeout)


    def get_all_possible_actions(self):
        return []


class ServerHomePageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = ServerHomePage(self.browser, self.timeout)


    def get_all_possible_actions(self):
        return []


class NormalVoteStep6PageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = NormalVoteStep6Page(self.browser, self.timeout)
        self.previous_state_class = AdvancedModeVotePageState # Why is it so? (Is it because as our vote has been confirmed, step5 has no meaning anymore? Then we should probably find a better replacement)


    def verify_page(self, in_memory):
        console_log("NormalVoteStep6PageState::verify_page()")
        return self.page.verify_page(in_memory["voter_temporary_smart_ballot_tracker"])


    def get_all_possible_actions(self):
        def click_on_ballot_box_link(in_memory=None):
            self.click_on_ballot_box_link()
            return BallotBoxPageState(self.browser, self.timeout, NormalVoteStep6PageState)

        def click_on_go_back_to_election_link(in_memory=None):
            self.page.click_on_go_back_to_election_link()
            return ElectionHomePageState(self.browser, self.timeout, AdvancedModeVotePageState) # Why is it so? It seems to be because all cookies are deleted, so server does not know anything anymore about user session state

        return [
            click_on_ballot_box_link,
            click_on_go_back_to_election_link,
        ]


class BallotBoxPageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = BallotBoxPage(self.browser, self.timeout)


    def verify_page(self, in_memory):
        console_log("BallotBoxPageState::verify_page()")
        smart_ballot_tracker = in_memory.get("voter_validated_smart_ballot_tracker", None)
        return self.page.verify_page(smart_ballot_tracker)


    def get_all_possible_actions(self):
        def click_on_go_back_to_election_link(in_memory=None):
            self.page.click_on_go_back_to_election_link()
            return ElectionHomePageState(self.browser, self.timeout)

        return [
            click_on_go_back_to_election_link
        ]


class AdvancedModeVotePageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = AdvancedModeVotePage(self.browser, self.timeout)


    def get_all_possible_actions(self):
        def click_on_back_to_election_home_link(in_memory=None):
            self.page.click_on_back_to_election_home_link()
            return ElectionHomePageState(self.browser, self.timeout)

        return [
            click_on_back_to_election_home_link,
        ] # TODO: other available actions


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
        console_log("Going to vote using VOTER_USERNAME:", settings.VOTER_USERNAME)
        console_log("Going to vote using VOTER_PASSWORD:", settings.VOTER_PASSWORD)
        console_log("Going to vote using VOTER_CREDENTIAL:", settings.VOTER_CREDENTIAL)


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

        monkey = SeleniumClickerMonkey(browser, election_url, verify_page_is_not_an_error_page)
        monkey.start(100)


    def test_very_smart_monkey_votes(self):
        console_log("# test_very_smart_monkey_votes()")
        browser = self.browser
        timeout = settings.EXPLICIT_WAIT_TIMEOUT
        election_url = get_election_url(self.election_id)
        console_log("## Going to election page:", election_url)
        browser.get(election_url)

        election_home_page_state = ElectionHomePageState(browser, timeout)
        election_home_page_state.page.click_on_language_link("en")
        in_memory = {
            "voter_username": settings.VOTER_USERNAME,
            "voter_password": settings.VOTER_PASSWORD,
            "voter_credential": settings.VOTER_CREDENTIAL,
        }
        smart_monkey = SmartMonkeyWithMemoryAndKnownStateMachine(election_home_page_state, in_memory=in_memory)
        console_log(f"smart_monkey.current_state: {smart_monkey.current_state}")
        console_log(f"smart_monkey.in_memory: {smart_monkey.in_memory}")
        current_iteration = 1
        while not isinstance(smart_monkey.current_state, NormalVoteStep6PageState):
            smart_monkey.verify_page()
            current_iteration += 1
            console_log(f"executing action number {current_iteration}")
            try:
                smart_monkey.execute_a_random_action()
            except Exception as e:
                console_log("Exception while executing `smart_monkey.execute_a_random_action()`:", repr(e))
                time.sleep(10)
                raise Exception("Exception while executing `smart_monkey.execute_a_random_action()`") from e
            console_log(f"smart_monkey.current_state: {smart_monkey.current_state}")
            console_log(f"smart_monkey.in_memory: {smart_monkey.in_memory}")

            if isinstance(smart_monkey.current_state, NormalVoteStep6PageState):
                console_log("Ending monkey behaviour here because we have completed the vote")
                smart_monkey.verify_page()
                console_log("Clicking on the ballot box link and verifying presence of voter's smart ballot tracker")
                smart_monkey.current_state.page.click_on_ballot_box_link()
                ballot_box_page = BallotBoxPage(browser, timeout)
                ballot_box_page.verify_page(smart_monkey.get_memory_element("voter_validated_smart_ballot_tracker"))


    @unittest.skip("TODO: remove skip")
    def test_sometimes_smart_monkey_votes(self):
        console_log("# test_sometimes_smart_monkey_votes()")
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

        console_log("## Verify that we are on step 5 and that page content is correct (page contains 'has been received, but not recorded yet'; page contains a ballot tracker which is the same as the one we noted; page contains voter's username)")
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

        console_log("## Verify that we are on step 6 and that page content is correct (page contains 'has been accepted'; page contains a ballot tracker which is the same as the one we noted)")
        step_6_page = NormalVoteStep6Page(browser, timeout)
        step_6_page.verify_page(step_3_smart_ballot_tracker_value)

        # Here:
        # We can click on the Belenios logo on the top-left of the screen
        # We can click on a link in the footer
        # We can click on the "ballot box" link
        # We can click on the "Go back to election" link
        # We can go back. This goes to another page which looks like the "Advanced mode" page. This looks like a small bug.

        console_log("## Click on the 'ballot box' link")
        step_6_page.click_on_ballot_box_link()

        wait_a_bit()

        console_log("## Verify that ballot box page contains a link labelled as voter's smart ballot tracker, and click on it")
        ballot_box_page = BallotBoxPage(browser, timeout)
        ballot_box_page.verify_page(step_3_smart_ballot_tracker_value)
        ballot_box_page.click_on_ballot_link(step_3_smart_ballot_tracker_value)

        console_log("## Verify that my ballot page is not an error page")
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
