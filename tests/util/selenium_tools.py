#!/usr/bin/python
# -*- coding: utf-8 -*
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import StaleElementReferenceException


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
    smart_ballot_tracker_element = custom_wait.until(element_exists_and_contains_expected_text((By.ID, "my_id"), "my expected text"))
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
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(element_exists_and_contains_expected_text((By.CSS_SELECTOR, css_selector), expected_text))
        return element
    except Exception:
        raise Exception("Could not find expected DOM element '" + css_selector + "' with text content '" + expected_text + "' until timeout of " + str(wait_duration) + " seconds")


def wait_for_element_exists_and_has_non_empty_content(browser, css_selector, wait_duration=10):
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(element_has_non_empty_content((By.CSS_SELECTOR, css_selector)))
        return element
    except Exception:
        raise Exception("Could not find expected DOM element '" + css_selector + "' with non-empty content until timeout of " + str(wait_duration) + " seconds")


def wait_for_element_exists(browser, css_selector, wait_duration=10):
    try:
        return WebDriverWait(browser, wait_duration).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, css_selector))
        )
    except Exception:
        raise Exception("Could not find expected DOM element '" + css_selector + "' until timeout of " + str(wait_duration) + " seconds")


def wait_for_elements_exist(browser, css_selector, wait_duration=10):
    try:
        return WebDriverWait(browser, wait_duration).until(
            EC.presence_of_all_elements_located((By.CSS_SELECTOR, css_selector))
        )
    except Exception:
        raise Exception("Could not find expected DOM elements '" + css_selector + "' until timeout of " + str(wait_duration) + " seconds")
