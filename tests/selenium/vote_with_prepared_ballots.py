#!/usr/bin/python
# coding: utf-8
import unittest
import os
import csv
from selenium.webdriver.support.select import Select
from distutils.util import strtobool
from util.selenium_tools import wait_for_element_exists, wait_for_element_exists_and_contains_expected_text, wait_for_an_element_with_link_text_exists, wait_for_element_exists_and_has_non_empty_content, verify_all_elements_have_attribute_value
from util.election_testing import console_log, wait_a_bit
from util.execution import ConsoleLogDuration, try_several_times
from test_scenario_2 import BeleniosTestElectionScenario2Base, initialize_browser_for_scenario_2
import settings


class BeleniosVoteWithPreparedBallots(BeleniosTestElectionScenario2Base):

    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)


    def setUp(self):
        self.browser = initialize_browser_for_scenario_2()
        self.browser.set_page_load_timeout(60 * 2) # If we don't set a page load timeout, sometimes some WebDriverWait stay stuck because page has not finished loading


    def tearDown(self):
        self.browser.quit()


    def cast_all_votes_from_csv(self):
        browser = self.browser
        generated_files_destination_folder = settings.GENERATED_FILES_DESTINATION_FOLDER
        csv_file_path = os.path.join(generated_files_destination_folder, 'all_votes.csv')
        with open(csv_file_path, 'r', newline='') as csvfile:
            csvreader = csv.DictReader(csvfile, delimiter=',', quotechar='|')
            current_row = 0
            for row in csvreader:
                current_row += 1
                if current_row <= settings.SKIP_ROWS_IN_CSV_FILE:
                    continue

                voter_email_address = row['voter_email_address']
                voter_password = row['voter_password']
                voter_encrypted_ballot_file_name = row['voter_encrypted_ballot_file_name']
                election_page_url = row['election_page_url']

                with ConsoleLogDuration(f"Row {current_row} (voter {voter_email_address})"):
                    # Go to election home
                    browser.get(election_page_url)

                    wait_a_bit()

                    # She clicks on "en" language
                    select = Select(wait_for_element_exists(browser, ".lang_box select", settings.EXPLICIT_WAIT_TIMEOUT))
                    select.select_by_value("en")
                    submit = wait_for_element_exists(browser, ".lang_box input[type=submit]", settings.EXPLICIT_WAIT_TIMEOUT)
                    submit.click()

                    wait_a_bit()

                    # Click on advanced mode
                    advanced_mode_link_expected_label = "Advanced mode"
                    advanced_mode_link_element = wait_for_an_element_with_link_text_exists(browser, advanced_mode_link_expected_label, settings.EXPLICIT_WAIT_TIMEOUT)
                    advanced_mode_link_element.click()

                    wait_a_bit()

                    # Browse file and submit it
                    browse_button_css_selector = "form input[name=encrypted_vote][type=file]"
                    browse_button_element = wait_for_element_exists(browser, browse_button_css_selector)
                    path_of_file_to_upload = os.path.join(generated_files_destination_folder, voter_encrypted_ballot_file_name)
                    browse_button_element.clear()
                    browse_button_element.send_keys(path_of_file_to_upload)
                    browse_button_element.submit()

                    wait_a_bit()

                    # Submit login form
                    username_field_css_selector = "form input[name=username]"
                    username_field_element = wait_for_element_exists(browser, username_field_css_selector)
                    username_field_element.clear()
                    username_field_element.send_keys(voter_email_address)
                    password_field_css_selector = "form input[name=password]"
                    password_field_element = wait_for_element_exists(browser, password_field_css_selector)
                    password_field_element.clear()
                    password_field_element.send_keys(voter_password)
                    password_field_element.submit()

                    wait_a_bit()

                    @try_several_times(max_attempts=3)
                    def verify_step_5_and_6(browser, timeout):
                        # Verify that page contains a ballot tracker
                        smart_ballot_tracker_css_selector = "#ballot_tracker"
                        smart_ballot_tracker_element = wait_for_element_exists_and_has_non_empty_content(browser, smart_ballot_tracker_css_selector, timeout)
                        my_smart_ballot_tracker_value = smart_ballot_tracker_element.get_attribute('innerText')
                        assert len(my_smart_ballot_tracker_value) > 5

                        # Click "I cast my vote" button
                        submit_button_css_selector = "form input[type=submit]"
                        submit_button_expected_content = "I cast my vote"
                        verify_all_elements_have_attribute_value(browser, submit_button_css_selector, "value", submit_button_expected_content, timeout)
                        submit_button_element = wait_for_element_exists(browser, submit_button_css_selector, timeout)
                        submit_button_element.click()

                        wait_a_bit()

                        # Verify that vote has been accepted by the server

                        @try_several_times(max_attempts=3)
                        def verify_that_we_are_on_step_6(browser, timeout):
                            all_ballots_link_expected_label = "ballot box"
                            all_ballots_element = wait_for_an_element_with_link_text_exists(browser, all_ballots_link_expected_label, timeout)

                            current_step_css_selector = ".current_step"
                            current_step_expected_content = "Step 6/6: Thank you for voting!"
                            wait_for_element_exists_and_contains_expected_text(browser, current_step_css_selector, current_step_expected_content, timeout)

                            return all_ballots_element

                        all_ballots_element = verify_that_we_are_on_step_6(browser, timeout)

                        # Go to all ballots page
                        all_ballots_element.click()

                        wait_a_bit()

                        # Verify presence of my ballot
                        my_smart_ballot_tracker_link_element = wait_for_an_element_with_link_text_exists(browser, my_smart_ballot_tracker_value, settings.EXPLICIT_WAIT_TIMEOUT)
                        my_smart_ballot_tracker_link_element.click()

                    timeout = settings.EXPLICIT_WAIT_TIMEOUT
                    verify_step_5_and_6(browser, timeout)


    def test_vote_with_prepared_ballots(self):
        # Generate ballot for each voter
        console_log("### Starting step: cast_all_votes_from_csv")
        self.cast_all_votes_from_csv()
        console_log("### Step complete: cast_all_votes_from_csv")


if __name__ == "__main__":
    if os.getenv('USE_HEADLESS_BROWSER', None):
        settings.USE_HEADLESS_BROWSER = bool(strtobool(os.getenv('USE_HEADLESS_BROWSER')))

    settings.WAIT_TIME_BETWEEN_EACH_STEP = float(os.getenv('WAIT_TIME_BETWEEN_EACH_STEP', settings.WAIT_TIME_BETWEEN_EACH_STEP))
    settings.EXPLICIT_WAIT_TIMEOUT = int(os.getenv('EXPLICIT_WAIT_TIMEOUT', settings.EXPLICIT_WAIT_TIMEOUT))

    settings.GENERATED_FILES_DESTINATION_FOLDER = os.getenv('GENERATED_FILES_DESTINATION_FOLDER', settings.GENERATED_FILES_DESTINATION_FOLDER)

    settings.SKIP_ROWS_IN_CSV_FILE = int(os.getenv('SKIP_ROWS_IN_CSV_FILE', 0))

    console_log("USE_HEADLESS_BROWSER:", settings.USE_HEADLESS_BROWSER)
    console_log("WAIT_TIME_BETWEEN_EACH_STEP:", settings.WAIT_TIME_BETWEEN_EACH_STEP)
    console_log("EXPLICIT_WAIT_TIMEOUT:", settings.EXPLICIT_WAIT_TIMEOUT)
    console_log("GENERATED_FILES_DESTINATION_FOLDER:", settings.GENERATED_FILES_DESTINATION_FOLDER)
    console_log("SKIP_ROWS_IN_CSV_FILE:", settings.SKIP_ROWS_IN_CSV_FILE)

    unittest.main()
