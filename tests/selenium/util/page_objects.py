#!/usr/bin/python
# coding: utf-8
import time
from selenium.webdriver.common.alert import Alert
from selenium.webdriver.support.select import Select
from util.selenium_tools import wait_for_an_element_exists_and_is_visible_and_contains_expected_text, wait_for_an_element_exists_and_is_visible_and_attribute_contains_expected_text, wait_for_element_exists, wait_for_elements_exist, wait_for_elements_exist_and_are_visible, wait_for_an_element_with_link_text_exists, wait_for_element_exists_and_contains_expected_text, wait_for_element_exists_and_has_non_empty_content, wait_for_an_alert
from util.election_testing import wait_a_bit, election_home_find_start_button, find_buttons_in_page_content_by_value


class SeleniumPageObjectModel():
    """
    Classes which inherit from SeleniumPageObjectModel are meant to follow the "Page Object" design pattern of Selenium, as described here: https://www.selenium.dev/documentation/en/guidelines_and_recommendations/page_object_models/
    """
    def __init__(self, browser, timeout):
        self.browser = browser
        self.timeout = timeout


    def click_on_link_with_expected_label(self, expected_label):
        link_element = wait_for_an_element_with_link_text_exists(self.browser, expected_label, self.timeout)
        link_element.click()


class VerifiablePage(SeleniumPageObjectModel):
    def verify_page(self):
        raise NotImplementedError()


class ClickableLogoPage(SeleniumPageObjectModel):
    def click_on_logo_image(self):
        logo_image_element = wait_for_element_exists(self.browser, "#header a", self.timeout) # maybe we should edit the DOM of the page to allow for a more specific CSS selector?
        logo_image_element.click()


class ElectionHomePage(VerifiablePage):
    def verify_page(self):
        election_home_find_start_button(self.browser)


    def click_on_language_link(self, language_link_label):
        select = Select(wait_for_element_exists(self.browser, ".lang_box select", self.timeout))
        select.select_by_value(language_link_label)


    def click_on_start_button(self):
        start_button_label = "Start"
        start_button_css_selector = "#main button"
        start_button_element = wait_for_element_exists_and_contains_expected_text(self.browser, start_button_css_selector, start_button_label, self.timeout)
        start_button_element.click()


    def click_on_advanced_mode_link(self):
        self.click_on_language_link("en") # in order to see the following label in the correct language
        self.click_on_link_with_expected_label("Advanced mode")


    def click_on_see_accepted_ballots_link(self):
        self.click_on_language_link("en") # in order to see the following label in the correct language
        self.click_on_link_with_expected_label("See accepted ballots")


    def click_on_accept_personal_data_policy_link(self):
        self.click_on_link_with_expected_label("Accept")


class NormalVoteGenericStepPage(VerifiablePage):
    current_step_css_selector = ".current_step"
    expected_step_content = "Step"


    def verify_step_title(self):
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, self.current_step_css_selector, self.expected_step_content, self.timeout)


    def verify_page(self):
        self.verify_step_title()


class ResponsiveFrontendSelectors:
    current_step_css_selector = ".breadcrumb__step--current"
    current_step_title_css_selector = ".breadcrumb__step--current .breadcrumb__step__title"


class ResponsiveBoothGenericStepPage(VerifiablePage):
    current_step_title_css_selector = ResponsiveFrontendSelectors.current_step_title_css_selector
    expected_breadcrumb_title = "" # will be customized by children

    def verify_breadcrumb(self):
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, self.current_step_title_css_selector, self.expected_breadcrumb_title, self.timeout)

    def verify_page(self):
        self.verify_breadcrumb()


class ResponsiveBoothStep1Page(ResponsiveBoothGenericStepPage):
    expected_breadcrumb_title = "Input credential"
    credential_input_css_selector = "#credential"
    next_button_css_selector = ".input-credential-section__button"

    def type_voter_credential(self, voter_credential):
        credential_input_element = wait_for_element_exists(self.browser, self.credential_input_css_selector, self.timeout)
        credential_input_element.clear()
        credential_input_element.send_keys(voter_credential)

    def click_next_button(self):
        next_button_element = wait_for_element_exists(self.browser, self.next_button_css_selector, self.timeout)
        next_button_element.click()


class NormalVoteStep1Page(NormalVoteGenericStepPage):
    expected_step_content = "Step 1/6: Input credential"


    def click_on_here_button(self):
        here_button_label = "here"
        here_button_css_selector = "#main button"
        here_button_element = wait_for_element_exists_and_contains_expected_text(self.browser, here_button_css_selector, here_button_label, self.timeout)
        here_button_element.click()

        # A modal opens (it is an HTML modal created using Window.prompt()), with an input field.


    def click_on_here_button_and_type_voter_credential(self, voter_credential):
        self.click_on_here_button()

        wait_a_bit()

        # A modal opens (it is an HTML modal created using Window.prompt()), with an input field. He types his credential and clicks on "OK" button of the modal.
        # credential_prompt = Alert(self.browser)
        credential_prompt = wait_for_an_alert(self.browser)
        credential_prompt.send_keys(voter_credential)
        credential_prompt.accept()


    def click_on_here_button_and_type_wrong_voter_credential(self, voter_credential):
        self.click_on_here_button_and_type_voter_credential(voter_credential)

        # Another modal opens (it is an HTML modal created using Window.alert()), saying that this is a wrong credential. He clicks on the "OK" button of the second modal.

        time.sleep(1)
        # failure_alert = Alert(self.browser)
        failure_alert = wait_for_an_alert(self.browser)
        failure_alert.accept()


    def click_on_here_button_and_cancel(self):
        self.click_on_here_button()

        wait_a_bit()

        # A modal opens (it is an HTML modal created using Window.prompt()), with an input field. He clicks on the "Cancel" button of the modal.
        # credential_prompt = Alert(self.browser)
        credential_prompt = wait_for_an_alert(self.browser)
        credential_prompt.dismiss()


class ResponsiveBoothStep2Page(ResponsiveBoothGenericStepPage):
    expected_breadcrumb_title = "Answer to questions"
    answers_css_selector = ".classic-vote-candidates-list label" # includes blank vote
    next_button_css_selector = ".vote-navigation__next-button-container .nice-button"

    def verify_page_body(self):
        answers_elements = wait_for_elements_exist(self.browser, self.answers_css_selector, self.timeout)
        assert len(answers_elements) == 2

    def verify_page(self):
        super().verify_page()
        self.verify_page_body()


    def fill_vote_form(self, vote_data):
        page = NormalVoteStep2Page(self.browser, self.timeout)
        page.answers_css_selector = self.answers_css_selector
        page.fill_vote_form(vote_data)


    def click_next_button(self):
        next_button_element = wait_for_element_exists(self.browser, self.next_button_css_selector, self.timeout)
        next_button_element.click()


class NormalVoteStep2Page(NormalVoteGenericStepPage):
    expected_step_content = "Step 2/6: Answer to questions"
    answers_css_selector = ".answer_div input"


    def verify_page_body(self):
        answers_elements = wait_for_elements_exist(self.browser, self.answers_css_selector, self.timeout)
        assert len(answers_elements) == 2


    def verify_page(self):
        NormalVoteGenericStepPage.verify_page(self)
        self.verify_page_body()


    def click_on_next_button(self):
        step_2_parent_css_selector = "#question_div"
        next_button = wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, step_2_parent_css_selector + " button", "Next", self.timeout)
        next_button.click()


    def click_on_next_button_but_form_is_not_filled(self):
        self.click_on_next_button()

        # A modal opens (it is an HTML modal created using Window.alert()).
        failure_alert = Alert(self.browser)
        failure_alert.accept()


    def fill_vote_form(self, vote_data):

        """
        Parameter `vote_data` is a dict with the following structure:
        ```
        {
            "question1": {
                "answer1": False,
                "answer2": True,
            }
        }
        ```
        For now, only one question is supported, with only 2 possible answers.
        """

        answers_elements = wait_for_elements_exist_and_are_visible(self.browser, self.answers_css_selector, self.timeout) # or we could use find_element_by_xpath("//div[@id='question_div']/input[@type='checkbox'][2]")

        assert len(answers_elements) == 2
        anwser1_element_is_proxy = answers_elements[0].tag_name == "label"
        anwser2_element_is_proxy = answers_elements[1].tag_name == "label"
        question1_answer1_element = wait_for_element_exists(self.browser, "#" + answers_elements[0].get_attribute("for"), self.timeout) if anwser1_element_is_proxy else answers_elements[0]
        question1_answer2_element = wait_for_element_exists(self.browser, "#" + answers_elements[1].get_attribute("for"), self.timeout) if anwser2_element_is_proxy else answers_elements[1]
        voter_vote_to_question_1_answer_1 = vote_data["question1"]["answer1"]
        voter_vote_to_question_1_answer_2 = vote_data["question1"]["answer2"]
        if question1_answer1_element.get_attribute('type') == 'checkbox':
            voter_vote_to_question_1_answer_1_is_checked = question1_answer1_element.is_selected()
            voter_vote_to_question_1_answer_2_is_checked = question1_answer2_element.is_selected()
            if voter_vote_to_question_1_answer_1 and not voter_vote_to_question_1_answer_1_is_checked:
                answers_elements[0].click()
            if not voter_vote_to_question_1_answer_1 and voter_vote_to_question_1_answer_1_is_checked:
                answers_elements[0].click()
            if voter_vote_to_question_1_answer_2 and not voter_vote_to_question_1_answer_2_is_checked:
                answers_elements[1].click()
            if not voter_vote_to_question_1_answer_2 and voter_vote_to_question_1_answer_2_is_checked:
                answers_elements[1].click()
        elif question1_answer1_element.get_attribute('type') == 'radio':
            voter_vote_to_question_1_answer_1_is_checked = question1_answer1_element.is_selected()
            voter_vote_to_question_1_answer_2_is_checked = question1_answer2_element.is_selected()
            if voter_vote_to_question_1_answer_1 and not voter_vote_to_question_1_answer_1_is_checked:
                answers_elements[0].click()
            if voter_vote_to_question_1_answer_2 and not voter_vote_to_question_1_answer_2_is_checked:
                answers_elements[1].click()
        else: # this handles the case of non homomorphic questions
            if voter_vote_to_question_1_answer_1:
                question1_answer1_element.send_keys("1")
            if voter_vote_to_question_1_answer_2:
                question1_answer2_element.send_keys("1")


class BallotTrackerPage(SeleniumPageObjectModel):
    smart_ballot_tracker_css_selector = "#ballot_tracker"

    def get_smart_ballot_tracker_value(self):
        smart_ballot_tracker_element = wait_for_element_exists_and_has_non_empty_content(self.browser, self.smart_ballot_tracker_css_selector, self.timeout)
        smart_ballot_tracker_value = smart_ballot_tracker_element.get_attribute('innerText')
        return smart_ballot_tracker_value

    def verify_smart_ballot_tracker_value(self):
        smart_ballot_tracker_value = self.get_smart_ballot_tracker_value()
        assert len(smart_ballot_tracker_value) > 5


class ResponsiveBoothStep3Page(ResponsiveBoothGenericStepPage):
    expected_breadcrumb_title = "Review and encrypt"

    def __init__(self, browser, timeout):
        super().__init__(browser, timeout)
        self.ballot_tracker_page = BallotTrackerPage(browser, timeout)
        self.ballot_tracker_page.smart_ballot_tracker_css_selector = "#smart_ballot_tracker"

    def verify_page_body(self):
        # We could also verify that the recap content corresponds to what has been voted in previous page
        step_3_parent_css_selector = "#ballot_div"
        step_3_expected_success_content = "Your ballot has been encrypted"
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, step_3_parent_css_selector, step_3_expected_success_content, self.timeout)
        self.ballot_tracker_page.verify_smart_ballot_tracker_value()

    def verify_page(self):
        super().verify_page()
        self.verify_page_body()

    def click_next_button(self):
        next_button_element = wait_for_an_element_exists_and_is_visible_and_attribute_contains_expected_text(self.browser, ".nice-button.nice-button--blue", "innerText", "Next", self.timeout)
        next_button_element.click()

    def click_previous_button(self):
        previous_button_element = wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, ".nice-button.nice-button--default", "Previous", self.timeout)
        previous_button_element.click()

    def get_smart_ballot_tracker_value(self):
        return self.ballot_tracker_page.get_smart_ballot_tracker_value()

    def verify_smart_ballot_tracker_value(self):
        return self.ballot_tracker_page.verify_smart_ballot_tracker_value()


class NormalVoteGenericStepWithBallotTrackerPage(NormalVoteGenericStepPage):
    def __init__(self, browser, timeout):
        super().__init__(browser, timeout)
        self.ballot_tracker_page = BallotTrackerPage(browser, timeout)
        self.ballot_tracker_page.smart_ballot_tracker_css_selector = "#ballot_tracker"

    def get_smart_ballot_tracker_value(self):
        return self.ballot_tracker_page.get_smart_ballot_tracker_value()

    def verify_smart_ballot_tracker_value(self):
        return self.ballot_tracker_page.verify_smart_ballot_tracker_value()


class NormalVoteStep3Page(NormalVoteGenericStepWithBallotTrackerPage):
    expected_step_content = "Step 3/6: Review and encrypt"


    def verify_page_body(self):
        # We could also verify that the recap content corresponds to what has been voted in previous page
        step_3_parent_css_selector = "#ballot_div"
        step_3_expected_success_content = "Your ballot has been encrypted"
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, step_3_parent_css_selector, step_3_expected_success_content, self.timeout)
        self.verify_smart_ballot_tracker_value()


    def verify_page(self):
        NormalVoteGenericStepWithBallotTrackerPage.verify_page(self)
        self.verify_page_body()


    def click_on_continue_button(self):
        continue_button_element = wait_for_an_element_exists_and_is_visible_and_attribute_contains_expected_text(self.browser, "input[type=submit]", "value", "Continue", self.timeout)
        continue_button_element.click()


    def click_on_restart_button(self):
        restart_button_element = wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, "button", "Restart", self.timeout)
        restart_button_element.click()


class VoterLoginPage(VerifiablePage, ClickableLogoPage):
    login_form_username_css_selector = '#main form input[name=username]'
    login_form_password_css_selector = '#main form input[name=password]'
    login_form_submit_css_selector = '#main form input[type=submit]'


    def verify_page(self):
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, "h1", "with", self.timeout)


    def fill_form(self, username, password):
        login_form_username_value = username # correct value: settings.ADMINISTRATOR_USERNAME
        login_form_password_value = password # correct value: settings.ADMINISTRATOR_PASSWORD

        login_form_username_element = wait_for_element_exists(self.browser, self.login_form_username_css_selector, self.timeout)
        login_form_password_element = wait_for_element_exists(self.browser, self.login_form_password_css_selector, self.timeout)

        login_form_username_element.clear()
        login_form_username_element.send_keys(login_form_username_value)
        login_form_password_element.clear()
        login_form_password_element.send_keys(login_form_password_value)


    def click_on_login_button(self):
        login_button_element = wait_for_element_exists(self.browser, self.login_form_submit_css_selector, self.timeout)
        login_button_element.submit()


    def log_in(self, username, password):
        self.fill_form(username, password)
        wait_a_bit()
        self.click_on_login_button()


class UnauthorizedPage(VerifiablePage):
    def verify_page(self):
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, "h1", "Unauthorized", self.timeout)


class LoginFailedPage(VerifiablePage, ClickableLogoPage):
    def verify_page(self):
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, "h1", "Authentication failed", self.timeout)


    def click_on_try_to_log_in_again_link(self):
        self.click_on_link_with_expected_label("try to log in again")


class ServerHomePage(VerifiablePage):
    def verify_page(self):
        assert self.browser.current_url.endswith("/admin") is True # There seems to be no content-based way to test that we are on the server home page. Another test we could use is this one: `assert 'Election server' in browser.title, "Browser title was: " + browser.title`


    def click_on_login_link(self, login_type):
        public_link_element = wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, "#header a", login_type, self.timeout)
        public_link_element.click()


    def click_on_accept_button_in_personal_data_policy_modal_if_available(self):
        # If a personal data policy modal appears (it does not appear after it has been accepted), she clicks on the "Accept" button
        accept_button_label = "Accept"
        button_elements = find_buttons_in_page_content_by_value(self.browser, accept_button_label)
        if len(button_elements) > 0:
            assert len(button_elements) == 1
            button_elements[0].click()


class NormalVoteStep6Page(NormalVoteGenericStepWithBallotTrackerPage):
    expected_step_content = "Thank you for voting!"


    def verify_page_body(self, expected_ballot_tracker):
        step_6_parent_css_selector = "#main"
        expected_step_6_body_content = "has been accepted"
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, step_6_parent_css_selector, expected_step_6_body_content, self.timeout)
        self.verify_smart_ballot_tracker_value()
        ballot_tracker_value = self.get_smart_ballot_tracker_value()
        assert ballot_tracker_value == expected_ballot_tracker


    def verify_page(self, expected_ballot_tracker):
        NormalVoteGenericStepWithBallotTrackerPage.verify_page(self)
        self.verify_page_body(expected_ballot_tracker)


    def click_on_ballot_box_link(self):
        self.click_on_link_with_expected_label("ballot box")


    def click_on_go_back_to_election_link(self):
        self.click_on_link_with_expected_label("Go back to election")


class BallotBoxPage(VerifiablePage):
    def verify_header(self):
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, "#header h1", "Accepted ballots", self.timeout)


    def verify_presence_of_expected_ballot_tracker(self, expected_ballot_tracker):
        all_smart_ballot_trackers_css_selector = "#main ul li a"
        all_smart_ballot_trackers_elements = wait_for_elements_exist(self.browser, all_smart_ballot_trackers_css_selector, self.timeout)
        assert len(all_smart_ballot_trackers_elements)
        matches = [element for element in all_smart_ballot_trackers_elements if element.get_attribute('innerText') == expected_ballot_tracker]
        assert len(matches) == 1


    def verify_page(self, expected_ballot_tracker=None):
        self.verify_header()
        if expected_ballot_tracker:
            self.verify_presence_of_expected_ballot_tracker(expected_ballot_tracker)


    def click_on_ballot_link(self, ballot_tracker):
        self.click_on_link_with_expected_label(ballot_tracker)


    def click_on_go_back_to_election_link(self):
        self.click_on_link_with_expected_label("Go back to election")


class AdvancedModeVotePage(VerifiablePage):
    def verify_page(self):
        wait_for_element_exists(self.browser, "input[type=file][name=encrypted_vote]")


    def click_on_back_to_election_home_link(self):
        self.click_on_link_with_expected_label("Back to election home")

    # TODO: other links in the page, fill both forms, submit them


class AdministrationHomeLoggedInPage(VerifiablePage):
    def verify_page(self):
        wait_for_an_element_exists_and_is_visible_and_contains_expected_text(self.browser, "h1", "Administration", self.timeout)
        wait_for_an_element_with_link_text_exists(self.browser, "Log out", self.timeout)
