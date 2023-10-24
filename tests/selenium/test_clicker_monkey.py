#!/usr/bin/python
# coding: utf-8
import unittest
import random
import os
import sys
from urllib.parse import urljoin, urlsplit

from util.election_testing import strtobool, wait_a_bit
from util.page_objects import ElectionHomePage, ResponsiveBoothStep1Page, ResponsiveBoothStep2Page, ResponsiveBoothStep3Page, VoterLoginPage, NormalVoteStep6Page, BallotBoxPage
from util.monkeys import SeleniumClickerMonkey, SeleniumFormFillerMonkey
from util.execution import console_log
from test_fuzz_vote import BeleniosTestElectionWithCreationBase
import settings


def verify_page_is_not_an_error_page(browser):
    # Belenios web server returns a "Unauthorized" "Error 401" page in several situations, for example when we pick the "local" login method and submit an empty login form. For now, we consider this behaviour as normal.
    # But what we consider an unexpected error is other types of errors returned by the server, for example "Internal Server Error", "Error 500".
    error_content = ["Internal Server Error", "Error 500", "Not Found", "Error 404"]
    page_source = browser.page_source
    if not page_source or not len(page_source):
        raise Exception(f"Server returned an unexpected blank page. Page source was: {page_source}")
    for content in error_content:
        if content in page_source:
            page_source = str(browser.page_source.encode("utf-8"))
            raise Exception(f"Server returned an unexpected error page. Page source was: {page_source}")


def belenios_fence_filter(initial_page_url, href_value):
    """
    A kind of geofencing: We filter out URLs which are out of the scope of the test
    """
    target_url = urljoin(initial_page_url, href_value)

    # If this link points to a different host (domain name), we abort
    if urlsplit(target_url).hostname != urlsplit(initial_page_url).hostname:
        return False

    # We abort if this link:
    # - points to a downloadable element which works correctly for sure or which we don't want to test (for example because it would be tested too often or would take too much resources to download)
    # - is the election creation page (if monkey accesses the administration panel by logging in using the "demo" mode)
    # - is the election edition page (if monkey accesses the administration panel by logging in using the "demo" mode)
    forbidden_urls = ["belenios.tar.gz", ".bel", "/draft/new", "/draft/election?uuid="]
    for url in forbidden_urls:
        if url in target_url:
            return False
    return True


def get_election_url(election_id):
    return "/".join([settings.SERVER_URL, "elections", election_id, ""])


class BeleniosMonkeyTestClicker(BeleniosTestElectionWithCreationBase):
    def test_clicker_monkey_on_election_home(self):
        console_log("# test_clicker_monkey_on_election_home()")
        browser = self.browser
        election_url = get_election_url(self.election_id)
        console_log("## Going to election page:", election_url)

        monkey = SeleniumClickerMonkey(browser, election_url, 0.25, belenios_fence_filter, verify_page_is_not_an_error_page)
        monkey.start(100)


    def test_sometimes_smart_monkey_votes(self):
        console_log("# test_sometimes_smart_monkey_votes()")
        browser = self.browser
        timeout = settings.EXPLICIT_WAIT_TIMEOUT
        election_url = get_election_url(self.election_id)
        console_log("## Going to election page:", election_url)
        browser.get(election_url)

        wait_a_bit()

        console_log("## Starting clicker monkey behaviour")
        monkey = SeleniumClickerMonkey(browser, election_url, 0.25, belenios_fence_filter, verify_page_is_not_an_error_page)
        maximum_monkey_clicks = 50
        monkey.start(maximum_monkey_clicks)
        console_log("## End of clicker monkey behaviour")

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
        step_1_page = ResponsiveBoothStep1Page(browser, timeout)
        step_1_page.verify_page()

        step_1_page.type_voter_credential(settings.VOTER_CREDENTIAL)
        step_1_page.click_next_button()

        wait_a_bit()

        step_2_page = ResponsiveBoothStep2Page(browser, timeout)
        step_2_page.verify_page()

        # Here:
        # We can check any checkbox for the question (check 0 to n checkboxes)
        # We can click on the "Next" button
        # We can go back. This would go back to the election home page (not to the "Step 1" page, which would probably have been the intuitive behaviour)

        console_log("## Answering vote question by checking randomly some checkboxes")
        step_2_parent_css_selector = '.question-with-votable-answers'
        form_filler_monkey = SeleniumFormFillerMonkey(browser, step_2_parent_css_selector) # Warning: In the DOM of the vote page, step 2, checkboxes are not in a `<form>`.
        form_filler_monkey.fill_form()

        console_log("## Click on the 'Next' button")
        step_2_page.click_next_button()

        wait_a_bit()

        console_log("## Verify that we are on step 3 and that page content is correct (ballot tracker is not empty)")
        step_3_page = ResponsiveBoothStep3Page(browser, timeout)
        step_3_page.verify_page()
        step_3_smart_ballot_tracker_value = step_3_page.get_smart_ballot_tracker_value()

        # Here:
        # We can click on the "Continue" button (`<input style="font-size:30px;" value="Continue" type="submit">`)
        # We can click on the "Restart" button (`<button onclick="location.reload();">Restart</button>`). This goes back to step 1.

        console_log("## Click on the 'Continue' button")
        step_3_page.click_next_button()

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

    unittest.main(failfast=True)
