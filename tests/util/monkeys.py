#!/usr/bin/python
# coding: utf-8
import random
from urllib.parse import urljoin, urlsplit
from util.election_testing import wait_a_bit
from util.execution import console_log
from util.selenium_tools import representation_of_element, element_is_visible_filter


def get_link_element_url(link_element):
    return link_element.get_attribute('href')


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
    error_content = ["Internal Server Error", "Unauthorized", "Error 500", "Error 401"]
    page_source = browser.page_source
    for content in error_content:
        if content in page_source:
            page_source = str(browser.page_source.encode("utf-8"))
            raise Exception(f"Server returned an unexpected error page. Page source was: {page_source}")


class SeleniumClickerMonkey():
    def __init__(self, browser, initial_page_url, verify_page_is_not_an_error_page_function=None):
        self.browser = browser
        self.initial_page_url = initial_page_url
        if verify_page_is_not_an_error_page_function is None:
            self.verify_page_is_not_an_error_page_function = verify_page_is_not_an_error_page
        else:
            self.verify_page_is_not_an_error_page_function = verify_page_is_not_an_error_page_function


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
            if self.verify_page_is_not_an_error_page_function:
                self.verify_page_is_not_an_error_page_function(self.browser)
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
                if not element.is_selected():
                    element.click()

        # TODO: handle other types of form fields (examples: input[type=text], input[type=password], textarea, input[type=file], input[type=radio])


    # def click_on_submit_button(self):
    #     form_element = self.browser.find_element_by_css_selector(self.form_css_selector)
    #     submit_button = form_element.find_element_by_css_selector("input[type=submit]")
    #     submit_button.click()


class StateForSmartMonkey():
    def __init__(self, browser, timeout, previous_state_class=None):
        # console_log("StateForSmartMonkey::__init__() with previous_state_class:", previous_state_class)
        self.browser = browser
        self.timeout = timeout
        self.previous_state_class = previous_state_class
        self.page = None


    def go_back(self):
        if not self.previous_state_class:
            raise NotImplementedError()
        self.browser.back()
        return self.previous_state_class(self.browser, self.timeout)


    def get_all_possible_actions(self):
        raise NotImplementedError() # or return []


    def verify_page(self, in_memory):
        """
        Child classes can override this method and make use of `in_memory` parameter to pass necessary data to `self.page.verify_page()`.
        """
        if self.page:
            return self.page.verify_page()


class SmartMonkeyWithMemoryAndKnownStateMachine():
    def __init__(self, initial_state, in_memory=None, probability_to_go_back=0.25):
        self.current_state = initial_state
        self.probability_to_go_back = probability_to_go_back
        if in_memory:
            self.in_memory = in_memory
        else:
            self.in_memory = dict()


    def get_memory(self):
        return self.in_memory


    def get_memory_element(self, key, default_value=None):
        return self.in_memory.get(key, default_value)


    def set_memory_element(self, key, value):
        self.in_memory[key] = value


    def go_back(self):
        console_log("Trying to go back")
        try:
            self.current_state = self.current_state.go_back()
        except Exception as e:
            raise Exception("Failed going back.") from e

    def execute_a_random_action(self):
        if random.random() < self.probability_to_go_back:
            try:
                self.go_back()
                return
            except Exception as e:
                console_log("Failed going back. Trying something else. Exception was:", e)

        possible_actions = self.current_state.get_all_possible_actions()
        console_log("possible_actions:", possible_actions)
        if len(possible_actions):
            random_action = random.choice(possible_actions)
            console_log("random_action:", random_action)
            self.current_state = random_action(in_memory=self.in_memory)
        else:
            console_log("List of possible actions is empty. Trying to go back")
            try:
                self.go_back()
                return
            except Exception as e:
                raise Exception("Cannot execute a random action, because list of posible actions is empty, and cannot go back.")


    def has_possible_actions(self):
        possible_actions = self.current_state.get_all_possible_actions()
        return len(possible_actions) > 0


    def verify_page(self):
        return self.current_state.verify_page(in_memory=self.in_memory)
