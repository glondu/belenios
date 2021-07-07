#!/usr/bin/python
# coding: utf-8
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException, StaleElementReferenceException, NoAlertPresentException


DEFAULT_WAIT_DURATION = 10 # In seconds


def printable_page_source(browser):
    return "Page source was: " + str(browser.page_source.encode("utf-8"))


def element_is_visible_filter(el):
    return el.is_displayed()


def representation_of_element(element):
    return element.get_attribute("outerHTML")


class an_alert_is_present(object):
    """
    An expectation for checking that an Alert is present.
    """
    def __init__(self):
        pass

    def __call__(self, driver):
        alert = driver.switch_to.alert
        return alert


def wait_for_an_alert(browser, wait_duration=DEFAULT_WAIT_DURATION):
    """
    Waits for the presence of an Alert.
    :param browser: Selenium browser
    :param wait_duration: Maximum duration in seconds that we wait for the presence of this element before raising an exception
    :return: The Alert
    """
    try:
        ignored_exceptions = (NoAlertPresentException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        alert = custom_wait.until(an_alert_is_present())
        return alert
    except Exception as e:
        raise Exception(f"Could not find expected Alert until timeout of {str(wait_duration)} seconds. " + printable_page_source(browser)) from e


class elements_exist_and_are_visible(object):
    def __init__(self, locator):
        self.locator = locator

    def __call__(self, driver):
        elements = driver.find_elements(*self.locator)
        if not elements:
            return False
        visible_elements = list(filter(element_is_visible_filter, elements))
        if not visible_elements:
            return False
        return visible_elements


def wait_for_elements_exist_and_are_visible(browser, css_selector, wait_duration=DEFAULT_WAIT_DURATION):
    """
    Waits for the presence of elements that match CSS selector `css_selector` and which are currently visible in the page.
    :param browser: Selenium browser
    :param css_selector: CSS selector of the expected element
    :param wait_duration: Maximum duration in seconds that we wait for the presence of this element before raising an exception
    :return: The list of WebElement once they match expected conditions
    """
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        elements = custom_wait.until(elements_exist_and_are_visible((By.CSS_SELECTOR, css_selector)))
        return elements
    except Exception as e:
        raise Exception(f"Could not find expected visible DOM elements matching '{css_selector}' until timeout of {str(wait_duration)} seconds. " + printable_page_source(browser)) from e


class an_element_exists_and_is_visible_and_attribute_contains_expected_text(object):
    def __init__(self, locator, attribute_name, expected_content):
        self.locator = locator
        self.attribute_name = attribute_name
        self.expected_content = expected_content

    def __call__(self, driver):
        elements = driver.find_elements(*self.locator)
        if not elements:
            return False
        visible_elements = filter(element_is_visible_filter, elements)
        if not visible_elements:
            return False
        for element in visible_elements:
            if self.expected_content in element.get_attribute(self.attribute_name):
                return element
        return False


def wait_for_an_element_exists_and_is_visible_and_attribute_contains_expected_text(browser, css_selector, attribute_name, expected_text, wait_duration=DEFAULT_WAIT_DURATION):
    """
    Waits for the presence of an element that matches CSS selector `css_selector` and which is currently visible in the page, and which has an attribute `attribute_name` which contains string `expected_text`.
    :param browser: Selenium browser
    :param css_selector: CSS selector of the expected element
    :param attribute_name: Name of the HTML attribute of the DOM element which should contain `expected_text`
    :param expected_text: String of the expected text that element must contain
    :param wait_duration: Maximum duration in seconds that we wait for the presence of this element before raising an exception
    :return: The WebElement once it matches expected conditions
    """
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(an_element_exists_and_is_visible_and_attribute_contains_expected_text((By.CSS_SELECTOR, css_selector), attribute_name, expected_text))
        return element
    except Exception as e:
        raise Exception(f"Could not find expected DOM element '{css_selector}' with attribute '{attribute_name}' which contains text content '{expected_text}' until timeout of {str(wait_duration)} seconds. " + printable_page_source(browser)) from e


def wait_for_an_element_exists_and_is_visible_and_contains_expected_text(browser, css_selector, expected_text, wait_duration=DEFAULT_WAIT_DURATION):
    return wait_for_an_element_exists_and_is_visible_and_attribute_contains_expected_text(browser, css_selector, "innerText", expected_text, wait_duration)


class element_has_non_empty_attribute(object):
    """
    An expectation for checking that an element has a non-empty value for given attribute.
    This class is meant to be used in combination with Selenium's `WebDriverWait::until()`. For example:
    ```
    custom_wait = WebDriverWait(browser, 10)
    smart_ballot_tracker_element = custom_wait.until(element_has_non_empty_attribute((By.ID, "my_id"), 'value'))
    ```

    :param locator: Selenium locator used to find the element. For example: `(By.ID, "my_id")`
    :param attribute: HTML attribute. For example 'innerText' (see `element_has_non_empty_content()` for this), or 'value'
    :return: The WebElement once it has a non-empty innerText attribute
    """
    def __init__(self, locator, attribute):
        self.locator = locator
        self.attribute = attribute

    def __call__(self, driver):
        element = driver.find_element(*self.locator)   # Finding the referenced element
        if not element:
            return False
        element_content = element.get_attribute(self.attribute).strip()
        if len(element_content) > 0:
            return element
        else:
            return False


class element_has_non_empty_content(element_has_non_empty_attribute):
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
        super().__init__(locator, 'innerText')


class an_element_with_partial_link_text_exists(object):
    def __init__(self, partial_link_text):
        self.partial_link_text = partial_link_text

    def __call__(self, driver):
        element = driver.find_element_by_partial_link_text(self.partial_link_text)
        if not element:
            return False
        return element


class an_element_with_link_text_exists(object):
    def __init__(self, link_text):
        self.link_text = link_text

    def __call__(self, driver):
        element = driver.find_element_by_link_text(self.link_text)
        if not element:
            return False
        return element


class element_exists_and_attribute_contains_expected_text(object):
    """
    An expectation for checking that an element exists and its given attribute contains expected text.
    This class is meant to be used in combination with Selenium's `WebDriverWait::until()`. For example:
    ```
    custom_wait = WebDriverWait(browser, 10)
    smart_ballot_tracker_element = custom_wait.until(element_exists_and_attribute_contains_expected_text((By.ID, "my_id"), "class", "hello"))
    ```

    :param locator: Selenium locator used to find the element. For example: `(By.ID, "my_id")`
    :param attribute: Attribute of the element that should contain expected text (parameter type: string)
    :param expected_text: Text expected in element's given attribute (parameter type: string)
    :return: The WebElement once its innerText attribute contains expected_text
    """
    def __init__(self, locator, attribute, expected_text):
        self.locator = locator
        self.attribute = attribute
        self.expected_text = expected_text

    def transform_attribute_content(self, content):
        return content

    def __call__(self, driver):
        element = driver.find_element(*self.locator)   # Finding the referenced element
        if not element:
            return False
        element_content = self.transform_attribute_content(element.get_attribute(self.attribute))
        if element_content and self.expected_text in element_content:
            return element
        else:
            return False


class element_exists_and_contains_expected_text(element_exists_and_attribute_contains_expected_text):
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
        super().__init__(locator, 'innerText', expected_text)

    def transform_attribute_content(self, content):
        if content:
            return content.strip()
        else:
            return content


class element_exists_and_does_not_contain_expected_text(object):
    """
    An expectation for checking that an element exists and its innerText attribute does not contain expected text.
    This class is meant to be used in combination with Selenium's `WebDriverWait::until()`. For example:
    ```
    custom_wait = WebDriverWait(browser, 10)
    smart_ballot_tracker_element = custom_wait.until(element_exists_and_does_not_contain_expected_text((By.ID, "my_id"), "my expected text"))
    ```

    :param locator: Selenium locator used to find the element. For example: `(By.ID, "my_id")`
    :param expected_text: Text expected to not be present in element's innerText attribute (parameter type: string)
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
        if self.expected_text not in element_content:
            return element
        else:
            return False


def wait_for_element_exists_and_contains_expected_text(browser, css_selector, expected_text, wait_duration=DEFAULT_WAIT_DURATION):
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
    except Exception as e:
        raise Exception("Could not find expected DOM element '" + css_selector + "' with text content '" + expected_text + "' until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e


def wait_for_element_exists_and_attribute_contains_expected_text(browser, css_selector, attribute, expected_text, wait_duration=DEFAULT_WAIT_DURATION):
    """
    Waits for the presence of an element that matches CSS selector `css_selector` and that has an innerText attribute that contains string `expected_text`.
    :param browser: Selenium browser
    :param css_selector: CSS selector of the expected element
    :param attribute: String. Name of the element's attribute that will be inspected
    :param expected_text: String of the expected text that element must contain
    :param wait_duration: Maximum duration in seconds that we wait for the presence of this element before raising an exception
    :return: The WebElement once it matches expected conditions
    """
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(element_exists_and_attribute_contains_expected_text((By.CSS_SELECTOR, css_selector), attribute, expected_text))
        return element
    except Exception as e:
        raise Exception("Could not find expected DOM element '" + css_selector + "' with text content '" + expected_text + "' until timeout of " + str(wait_duration) + " seconds. Page source was: " + str(browser.page_source.encode("utf-8"))) from e


def wait_for_element_exists_and_does_not_contain_expected_text(browser, css_selector, expected_text, wait_duration=DEFAULT_WAIT_DURATION):
    """
    Waits for the presence of an element that matches CSS selector `css_selector` and that has an innerText attribute that does not contain string `expected_text`.
    :param browser: Selenium browser
    :param css_selector: CSS selector of the expected element
    :param expected_text: String of the expected text that element must not contain
    :param wait_duration: Maximum duration in seconds that we wait for the presence of this element before raising an exception
    :return: The WebElement once it matches expected conditions
    """
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(element_exists_and_does_not_contain_expected_text((By.CSS_SELECTOR, css_selector), expected_text))
        return element
    except Exception as e:
        raise Exception("Could not find expected DOM element '" + css_selector + "' that does not contain string '" + expected_text + "' until timeout of " + str(wait_duration) + " seconds. Page source was: " + str(browser.page_source.encode("utf-8"))) from e


def wait_for_element_exists_and_has_non_empty_attribute(browser, css_selector, attribute, wait_duration=DEFAULT_WAIT_DURATION):
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(element_has_non_empty_attribute((By.CSS_SELECTOR, css_selector), attribute))
        return element
    except Exception as e:
        raise Exception("Could not find expected DOM element '" + css_selector + "' with non-empty attribute '" + attribute + "' until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e


def wait_for_element_exists_and_has_non_empty_content(browser, css_selector, wait_duration=DEFAULT_WAIT_DURATION):
    return wait_for_element_exists_and_has_non_empty_attribute(browser, css_selector, 'innerText', wait_duration)


def wait_for_an_element_with_partial_link_text_exists(browser, partial_link_text, wait_duration=DEFAULT_WAIT_DURATION):
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(an_element_with_partial_link_text_exists(partial_link_text))
        return element
    except Exception as e:
        raise Exception("Could not find a DOM element that contains expected partial link text '" + partial_link_text + "' until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e


def wait_for_an_element_with_link_text_exists(browser, link_text, wait_duration=DEFAULT_WAIT_DURATION):
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(an_element_with_link_text_exists(link_text))
        return element
    except Exception as e:
        raise Exception("Could not find a DOM element that has expected link text '" + link_text + "' until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e


def wait_for_element_exists(browser, css_selector, wait_duration=DEFAULT_WAIT_DURATION):
    try:
        ignored_exceptions = (NoSuchElementException, StaleElementReferenceException,)
        custom_wait = WebDriverWait(browser, wait_duration, ignored_exceptions=ignored_exceptions)
        element = custom_wait.until(
            EC.presence_of_element_located((By.CSS_SELECTOR, css_selector))
        )
        return element
    except Exception as e:
        raise Exception("Could not find expected DOM element '" + css_selector + "' until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e



def wait_for_element_visible(browser, css_selector, wait_duration=DEFAULT_WAIT_DURATION):
    try:
        return WebDriverWait(browser, wait_duration).until(
            EC.visibility_of_element_located((By.CSS_SELECTOR, css_selector))
        )
    except Exception as e:
        raise Exception("Could not find expected visible DOM element '" + css_selector + "' until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e



def wait_for_elements_exist(browser, css_selector, wait_duration=DEFAULT_WAIT_DURATION):
    try:
        return WebDriverWait(browser, wait_duration).until(
            EC.presence_of_all_elements_located((By.CSS_SELECTOR, css_selector))
        )
    except Exception as e:
        raise Exception("Could not find expected DOM elements '" + css_selector + "' until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e


def wait_until_page_url_changes(browser, old_url, wait_duration=DEFAULT_WAIT_DURATION):
    try:
        return WebDriverWait(browser, wait_duration).until(
            lambda driver: old_url != driver.current_url
        )
    except Exception as e:
        raise Exception("Could not detect a change in page URL until timeout of " + str(wait_duration) + " seconds." + printable_page_source(browser)) from e


def set_element_attribute(browser, element_dom_id, attribute_key, attribute_value):
    browser.execute_script("let el = document.getElementById('" + element_dom_id + "'); el.setAttribute('" + attribute_key + "','" + attribute_value + "');")


def verify_element_label(element, expected_label):
    element_real_label = element.get_attribute('innerText')
    assert expected_label in element_real_label, 'Expected label "' + expected_label + '" not found in element label "' + element_real_label + "'"


def verify_all_elements_have_attribute_value(browser, elements_css_selector, attribute_name, attribute_value, wait_duration=DEFAULT_WAIT_DURATION, extractor=(lambda x: x)):
    elements = wait_for_elements_exist(browser, elements_css_selector, wait_duration)
    assert len(elements) > 0, "Error: could not find any element in page matching this CSS selector" + printable_page_source(browser)
    for element in extractor(elements):
        assert element.get_attribute(attribute_name) == attribute_value, "Error: One of the elements corresponding to this CSS selector has a value of '" + element.get_attribute(attribute_name) + "' instead of expected '" + attribute_value + "'" + printable_page_source(browser)


def verify_some_elements_have_attribute_value(browser, elements_css_selector, attribute_name, attribute_value, necessary_elements):
    elements = wait_for_elements_exist(browser, elements_css_selector)
    assert len(elements) > 0, "Error: could not find any element in page matching this CSS selector." + printable_page_source(browser)
    elements_matching_condition = 0
    for element in elements:
        if element.get_attribute(attribute_name) == attribute_value:
            elements_matching_condition += 1
        if elements_matching_condition >= necessary_elements:
            break
    assert elements_matching_condition >= necessary_elements, "Error: Not enough elements corresponding to this CSS selector have a value of '" + attribute_value + "' (" + str(elements_matching_condition) + " instead of expected minimum of " + str(necessary_elements) + ")." + printable_page_source(browser)
