#!/usr/bin/python
# coding: utf-8
from util.monkeys import SeleniumFormFillerMonkey, StateForSmartMonkey
from util.page_objects import ElectionHomePage, NormalVoteStep1Page, NormalVoteStep2Page, NormalVoteStep3Page, VoterLoginPage, NormalVoteStep6Page, BallotBoxPage, UnauthorizedPage, ServerHomePage, AdvancedModeVotePage, LoginFailedPage
from util.execution import console_log


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
        super().__init__(*args, **kwargs)
        self.page = NormalVoteStep1Page(self.browser, self.timeout)


    def get_all_possible_actions(self):
        def click_on_here_button_and_type_correct_voter_credential(in_memory=None):
            self.page.click_on_here_button_and_type_voter_credential(in_memory["voter_credential"])
            return NormalVoteStep2PageState(self.browser, self.timeout, ElectionHomePageState) # Why previous state of step2 is not step1 ?

        def click_on_here_button_and_type_wrong_voter_credential(in_memory=None):
            self.page.click_on_here_button_and_type_wrong_voter_credential("aaa") # TODO: randomize input (fuzz). Also sometimes the second alert message is not caught, so maybe we should create a wait_* function for alerts
            return self

        def click_on_here_button_and_cancel(in_memory=None):
            self.page.click_on_here_button_and_cancel()
            return self

        return [
            click_on_here_button_and_type_correct_voter_credential,
            # click_on_here_button_and_type_wrong_voter_credential, # TODO: fix flaky detection of Alert
            click_on_here_button_and_cancel,
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
            decided_vote = in_memory.get("voter_decided_vote", None)
            if decided_vote:
                self.page.fill_vote_form(decided_vote)
            else:
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
                return NormalVoteStep6PageState(self.browser, self.timeout, NormalVoteLoginPageState)
            else:
                return LoginFailedPageState(self.browser, self.timeout, NormalVoteLoginPageState)

        def click_on_logo_image(in_memory=None):
            self.page.click_on_logo_image()
            return ServerHomePageState(self.browser, self.timeout, NormalVoteLoginPageState)

        return [
            fill_form_with_wrong_data,
            fill_form_with_correct_data,
            click_on_login_button,
            click_on_logo_image,
        ]


class UnauthorizedPageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = UnauthorizedPage(self.browser, self.timeout)


    def get_all_possible_actions(self):
        return []


class LoginFailedPageState(StateForSmartMonkey):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.page = LoginFailedPage(self.browser, self.timeout)


    def get_all_possible_actions(self):
        def click_on_logo_image(in_memory=None):
            self.page.click_on_logo_image()
            return ServerHomePageState(self.browser, self.timeout, LoginFailedPageState)

        def click_on_try_to_log_in_again_link(in_memory=None):
            self.page.click_on_try_to_log_in_again_link()
            return NormalVoteLoginPageState(self.browser, self.timeout, NormalVoteLoginPageState)

        return [
            click_on_logo_image,
            click_on_try_to_log_in_again_link,
        ]


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
