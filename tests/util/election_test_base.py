#!/usr/bin/python
# coding: utf-8
import unittest
import random
import re
from urllib.parse import urlencode
from selenium.webdriver.common.alert import Alert
from util.selenium_tools import wait_for_element_exists, wait_for_elements_exist, wait_for_element_exists_and_contains_expected_text, wait_for_element_exists_and_has_non_empty_content, wait_for_an_element_with_partial_link_text_exists, verify_element_label
from util.election_testing import console_log, random_email_addresses_generator, populate_credential_and_password_for_voters_from_sent_emails, populate_random_votes_for_voters, repopulate_vote_confirmations_for_voters_from_sent_emails, wait_a_bit, build_css_selector_to_find_buttons_in_page_content_by_value, find_button_in_page_content_by_value, initialize_browser, election_page_url_to_election_id, verify_election_consistency, create_election_data_snapshot, delete_election_data_snapshot, log_in_as_administrator, log_out, administrator_starts_creation_of_election, administrator_edits_election_questions, administrator_sets_election_voters, administrator_validates_creation_of_election
import settings


class BeleniosElectionTestBase(unittest.TestCase):
    """
    A base class that is meant to be derived, to implement a real test of an election.

    Properties:
    - server
    - browser
    - fake_sent_emails_manager: An instance of util.fake_sent_emails_manager.FakeSentEmailsManager
    - voters_email_addresses: A list of email addresses (strings). This is all users who are invited to vote
    - voters_email_addresses_who_have_lost_their_password: A list of email addresses (strings). This is all users who have asked for a new password.
    - voters_email_addresses_who_have_voted: A dictionary, indexed by email address (string), where each element value is True
    - voters_data: A dictionary, indexed by email address (string), where each element is a dictionary of fields for the voter who is identified by this email address. This is data about all users who have voted.
    - election_page_url: The election page URL (string). Example: "http://localhost:8001/elections/H5ecRG3wHZ21cp/"
    - election_id: The election ID (string). Example: "H5ecRG3wHZ21cp"
    """


    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)

        self.server = None
        self.browser = None
        self.fake_sent_emails_manager = None
        self.voters_email_addresses = []
        self.voters_email_addresses_who_have_lost_their_password = []
        self.voters_email_addresses_who_have_voted = dict()
        self.voters_data = dict()
        self.election_page_url = None
        self.election_id = None


    def update_voters_data(self, some_voters_data):
        """
        :param some_voters: a list of voter data
        """
        for voter in some_voters_data:
            self.voters_data[voter["email_address"]] = voter


    def compute_number_of_votes_per_answer(self, voters_data=None):
        if not voters_data:
            voters_data = self.voters_data
        votes_for_answers = {'answer1': 0, 'answer2': 0}
        for k, v in voters_data.items():
            answer1 = v['votes']['question1']['answer1']
            answer2 = v['votes']['question1']['answer2']
            if answer1:
                votes_for_answers['answer1'] += 1
            if answer2:
                votes_for_answers['answer2'] += 1
        return votes_for_answers


    def administrator_creates_election(self):
        # # Setting up a new election (action of the administrator)

        browser = self.browser

        # Alice has been given administrator rights on an online voting app called Belenios. She goes
        # to check out its homepage and logs in
        log_in_as_administrator(browser)

        # She starts creation of the election:
        # - She clicks on the "Prepare a new election" link
        # (- She keeps default values on the form: Credential management is automatic (not manual), and Authentication method is Password, not CAS)
        # - She clicks on the "Proceed" button (this redirects to the "Preparation of election" page)
        # - She changes values of fields name and description of the election
        # - She clicks on the "Save changes button" (the one that is next to the election description field)
        administrator_starts_creation_of_election(browser)

        # She edits election's questions:
        # - She clicks on the "Edit questions" link, to write her own questions
        # - She arrives on the Questions page. She checks that the page title is correct
        # - She removes answer 3
        # - She clicks on the "Save changes" button (this redirects to the "Preparation of election" page)
        administrator_edits_election_questions(browser)

        # She sets election's voters:
        # - She clicks on the "Edit voters" link, to then type the list of voters
        # - She types N e-mail addresses (the list of invited voters)
        # - She clicks on the "Add" button to submit changes
        # - She clicks on "Return to draft page" link
        self.voters_email_addresses = random_email_addresses_generator(settings.NUMBER_OF_INVITED_VOTERS)
        administrator_sets_election_voters(browser, self.voters_email_addresses)

        # She clicks on button "Generate on server"
        generate_on_server_button_label = "Generate on server"
        generate_on_server_button_css_selector = build_css_selector_to_find_buttons_in_page_content_by_value(generate_on_server_button_label)
        generate_on_server_button_element = wait_for_element_exists(browser, generate_on_server_button_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        generate_on_server_button_element.click()

        wait_a_bit()

        # (Server sends emails to voters.) She checks that server does not show any error that would happen when trying to send these emails (this can happen if sendmail is not configured)
        confirmation_sentence_expected_text = "Credentials have been generated and mailed!"
        confirmation_sentence_css_selector = "#main p"
        wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, settings.EXPLICIT_WAIT_TIMEOUT)

        # Now we do a sanity check that server has really tried to send emails. For this, we look for email addresses in the temporary file where our fake sendmail executable redirects its inputs to.

        """
        An email sent by Belenios (using sendmail or using the fake sendmail) to a voter looks like this:

Content-type: text/plain; charset="UTF-8"
Content-transfer-encoding: quoted-printable
From: Belenios public server <noreply@example.org>
To: "820E7G83JBY0F4Z3DY2Y@mailinator.com"
 <820E7G83JBY0F4Z3DY2Y@mailinator.com>
Subject: Your credential for election My test election for Scenario 1
MIME-Version: 1.0
X-Mailer: OcamlNet (ocamlnet.sourceforge.net)
Date: Wed, 31 Oct 2018 15:22:27 +0100

You are listed as a voter for the election

  My test election for Scenario 1

You will find below your credential.  To cast a vote, you will also
need a password, sent in a separate email.  Be careful, passwords and
credentials look similar but play different roles.  You will be asked
to enter your credential before entering the voting booth.  Login and
passwords are required once your ballot is ready to be cast.

Credential: yQVDQaKSAQVjdZq
Page of the election: http://localhost:8001/elections/AFFNDEPnpy21bw/

Note that you are allowed to vote several times.  Only the last vote
counts.

----------

Vous =C3=AAtes enregistr=C3=A9(e) en tant qu=27=C3=A9lecteur(trice) pour=20=
l=27=C3=A9lection

  My test election for Scenario 1

Veuillez trouver ci-dessous votre code de vote. Pour soumettre un
bulletin, vous aurez =C3=A9galement besoin d=27un mot de passe, envoy=C3=A9=
 dans
un e-mail s=C3=A9par=C3=A9. Soyez attentif(ve), le mot de passe et le cod=
e de
vote se ressemblent mais jouent des r=C3=B4les diff=C3=A9rents. Le syst=C3=
=A8me vous
demandera votre code de vote d=C3=A8s l=27entr=C3=A9e dans l=27isoloir=20=
virtuel. Le
nom d=27utilisateur et le mot de passe sont n=C3=A9cessaires lorsque votr=
e
bulletin est pr=C3=AAt =C3=A0 =C3=AAtre soumis.

Code de vote=C2=A0: yQVDQaKSAQVjdZq
Page de l=27=C3=A9lection=C2=A0: http://localhost:8001/elections/AFFNDEPn=
py21bw/

Notez que vous pouvez voter plusieurs fois. Seul le dernier vote est
pris en compte.

--=20
        """

        email_address_to_look_for = self.voters_email_addresses[0]
        text_to_look_for = 'To: "' + email_address_to_look_for + '"'
        email_address_found = self.fake_sent_emails_manager.find_in_sent_emails(text_to_look_for)
        assert email_address_found, "Text '" + email_address_to_look_for + "'' not found in fake sendmail log file"


        # She clicks on the "Proceed" link
        proceed_link_css_selector = "#generic_proceed_link"
        proceed_link_element = wait_for_element_exists(browser, proceed_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        proceed_link_element.click()

        wait_a_bit()

        # In "Authentication" section, she clicks on the "Generate and mail missing passwords" button
        generate_and_mail_missing_passwords_button_label = "Generate and mail missing passwords"
        generate_and_mail_missing_passwords_button_element = wait_for_element_exists(browser, build_css_selector_to_find_buttons_in_page_content_by_value(generate_and_mail_missing_passwords_button_label), settings.EXPLICIT_WAIT_TIMEOUT)
        generate_and_mail_missing_passwords_button_element.click()

        wait_a_bit()

        # She checks that the page contains expected confirmation text, instead of an error (TODO: explain in which case an error can happen, and check that it does not show)
        confirmation_sentence_expected_text = "Passwords have been generated and mailed!"
        confirmation_sentence_css_selector = "#main p"
        wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, settings.EXPLICIT_WAIT_TIMEOUT)

        # She clicks on the "Proceed" link (this redirects to the "Preparation of election" page)
        proceed_link_css_selector = "#generic_proceed_link"
        proceed_link_element = wait_for_element_exists(browser, proceed_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        proceed_link_element.click()

        wait_a_bit()

        self.election_page_url = administrator_validates_creation_of_election(browser)
        console_log("election_page_url:", self.election_page_url)
        self.election_id = election_page_url_to_election_id(self.election_page_url)
        console_log("election_id:", self.election_id)

        log_out(browser)


    def administrator_regenerates_passwords_for_some_voters(self):
        # Alice has been contacted by some voters who say they lost their password. She wants to re-generate their passwords and have the platform send them by email. For this, she logs in as administrator.
        browser = self.browser
        log_in_as_administrator(browser)


        # She remembers the list of voters who contacted her and said they lost their password. For this, we pick randomly NUMBER_OF_REGENERATED_PASSWORD_VOTERS voters from all the voters.
        self.voters_email_addresses_who_have_lost_their_password = random.sample(self.voters_email_addresses, settings.NUMBER_OF_REGENERATED_PASSWORD_VOTERS)

        # She selects the election that she wants to edit
        browser = self.browser
        election_to_edit_css_selector = "#election_admin_" + str(self.election_id)
        election_to_edit_elements = wait_for_elements_exist(browser, election_to_edit_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        assert len(election_to_edit_elements) > 0
        election_to_edit_elements[0].click()

        wait_a_bit()

        # She arrives to the election administration page. For each voter of the NUMBER_OF_REGENERATED_PASSWORD_VOTERS selected voters:
        for email_address in self.voters_email_addresses_who_have_lost_their_password:
            # She clicks on the "Regenerate and mail a password" link
            regenerate_and_mail_a_password_link_css_selector = "#election_regenpwd"
            regenerate_and_mail_a_password_link_element = wait_for_element_exists(browser, regenerate_and_mail_a_password_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
            regenerate_and_mail_a_password_link_element.click()

            wait_a_bit()

            # She types the e-mail address of the voter in the "Username" field
            username_field_css_selector = "#main input[type=text]"
            username_field_element = wait_for_element_exists(browser, username_field_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
            username_field_element.send_keys(email_address)

            wait_a_bit()

            # She clicks on the "Submit" button
            submit_button_label = "Submit"
            submit_button_element = find_button_in_page_content_by_value(browser, submit_button_label)
            submit_button_element.click()

            wait_a_bit()

            # She checks that the page shows a confirmation message similar to "A new password has been mailed to RMR4MY4XV5GUDNOR6XNH@mailinator.com"
            confirmation_sentence_expected_text = "A new password has been mailed to"
            confirmation_sentence_css_selector = "#main p"
            wait_for_element_exists_and_contains_expected_text(browser, confirmation_sentence_css_selector, confirmation_sentence_expected_text, settings.EXPLICIT_WAIT_TIMEOUT)

            # She clicks on the "Proceed" link
            proceed_link_css_selector = "#generic_proceed_link"
            proceed_link_element = wait_for_element_exists(browser, proceed_link_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
            proceed_link_element.click()

            wait_a_bit()

            # She arrives back to the election administration page

        """
        Now we do a sanity check that server has really tried to send these emails. For this, we look for email addresses in the temporary file where our fake sendmail executable redirects its inputs to. There should be 3 occurences of "To : xxx@xxx" for users who have lost their password, with respective subjects:
        - "Your credential for election My test election for Scenario 1"
        - "Your password for election My test election for Scenario 1"
        - "Your password for election My test election for Scenario 1"

        And there should be only 2 occurences for other users, with respective subjects:
        - "Your credential for election My test election for Scenario 1"
        - "Your password for election My test election for Scenario 1"
        """

        for email_address in self.voters_email_addresses_who_have_lost_their_password:
            text_to_look_for = 'To: "' + email_address + '"'
            assert self.fake_sent_emails_manager.count_occurences_in_sent_emails(text_to_look_for) is 3

        voters_email_addresses_who_have_not_lost_their_password = set(self.voters_email_addresses) - set(self.voters_email_addresses_who_have_lost_their_password)

        for email_address in voters_email_addresses_who_have_not_lost_their_password:
            text_to_look_for = 'To: "' + email_address + '"'
            assert self.fake_sent_emails_manager.count_occurences_in_sent_emails(text_to_look_for) is 2

        log_out(browser)


    def one_voter_votes(self, voter, direct=False):
        browser = self.browser

        if direct:
            browser.get(settings.SERVER_URL + "/vote.html#" + urlencode({"uuid": self.election_id}))

        else:
            # Bob has received 2 emails containing an invitation to vote and all necessary credentials (election page URL, username, password). He goes to the election page URL.
            browser.get(voter["election_page_url"])

            wait_a_bit()

            # He clicks on the "Start" button
            start_button_expected_label = "Start"
            start_button_css_selector = "#main button"
            start_button_element = wait_for_element_exists_and_contains_expected_text(browser, start_button_css_selector, start_button_expected_label, settings.EXPLICIT_WAIT_TIMEOUT)
            start_button_element.click()

        wait_a_bit()

        # A loading screen appears, then another screen appears. He clicks on the "Here" button
        here_button_expected_label = "here"
        here_button_css_selector = "#input_code button"
        here_button_element = wait_for_element_exists_and_contains_expected_text(browser, here_button_css_selector, here_button_expected_label, settings.EXPLICIT_WAIT_TIMEOUT)
        here_button_element.click()

        wait_a_bit()

        # A modal opens (it is an HTML modal created using Window.prompt()), with an input field. He types his credential.
        credential_prompt = Alert(browser)
        credential_prompt.send_keys(voter["credential"])
        credential_prompt.accept()

        wait_a_bit()

        # A new screen appears, which has a title "Step 2/6: Answer to questions", and a content:
        # "Question 1?"
        # "Question #1 of 1 — select between 1 and 2 answer(s)"
        # [ ] "Answer 1"
        # [ ] "Answer 2"
        # [Next]
        # (where "[ ]" is a checkbox, and [Next] is a button)

        # He fills his votes to each answer of the question
        answers_css_selector = "#question_div input[type=checkbox]"
        answers_elements = wait_for_elements_exist(browser, answers_css_selector, settings.EXPLICIT_WAIT_TIMEOUT) # or we could use find_element_by_xpath("//div[@id='question_div']/input[@type='checkbox'][2]")

        assert len(answers_elements) is 2
        question1_answer1_element = answers_elements[0]
        question1_answer2_element = answers_elements[1]
        voter_vote_to_question_1_answer_1 = voter["votes"]["question1"]["answer1"]
        voter_vote_to_question_1_answer_2 = voter["votes"]["question1"]["answer2"]
        voter_vote_to_question_1_answer_1_is_checked = question1_answer1_element.get_attribute('checked')
        voter_vote_to_question_1_answer_2_is_checked = question1_answer2_element.get_attribute('checked')
        assert voter_vote_to_question_1_answer_1_is_checked is None
        assert voter_vote_to_question_1_answer_2_is_checked is None
        if voter_vote_to_question_1_answer_1:
            question1_answer1_element.click()
        if voter_vote_to_question_1_answer_2:
            question1_answer2_element.click()

        wait_a_bit()

        # He clicks on the "Next" button
        next_button_expected_label = "Next"
        next_button_css_selector = "#question_div button"
        next_button_element = wait_for_element_exists_and_contains_expected_text(browser, next_button_css_selector, next_button_expected_label, settings.EXPLICIT_WAIT_TIMEOUT)
        next_button_element.click()

        wait_a_bit()

        """
        A new screen appears, showing:

        Step 3/6: Review and encrypt
        Question 1?
        - Answer 1

        Your ballot has been successfully encrypted, but has not been cast yet!

        Your smart ballot tracker is sLRilXoAYcodIrjWrOqPrVXLNlRyCJAqFeeHZ4WCajU

        We invite you to save it in order to check later that it is taken into account.

        [Continue]
        [Restart]
        """

        # He remembers the smart ballot tracker that is displayed.
        smart_ballot_tracker_css_selector = "#ballot_tracker"
        smart_ballot_tracker_element = wait_for_element_exists_and_has_non_empty_content(browser, smart_ballot_tracker_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        smart_ballot_tracker_value = smart_ballot_tracker_element.get_attribute('innerText')
        assert len(smart_ballot_tracker_value) > 5

        voter["smart_ballot_tracker"] = smart_ballot_tracker_value

        # He clicks on the "Continue" button
        next_button_expected_label = "Continue"
        next_button_css_selector = "#div_submit input[type=submit][value='" + next_button_expected_label + "']"
        next_button_element = browser.find_element_by_css_selector(next_button_css_selector)
        next_button_element.click()

        wait_a_bit()

        # He types his voter username and password, and submits the form
        username_field_css_selector = "#main input[name=username]"
        username_field_element = wait_for_element_exists(browser, username_field_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        username_field_element.send_keys(voter["username"])

        password_field_css_selector = "#main input[name=password]"
        password_field_element = browser.find_element_by_css_selector(password_field_css_selector)
        password_field_element.send_keys(voter["password"])

        wait_a_bit()

        password_field_element.submit()

        wait_a_bit()

        # He checks that the smart ballot tracker value that appears on screen is the same as the one he noted
        smart_ballot_tracker_verification_css_selector = "#ballot_tracker"
        smart_ballot_tracker_verification_element = wait_for_element_exists_and_has_non_empty_content(browser, smart_ballot_tracker_verification_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        smart_ballot_tracker_verification_value = smart_ballot_tracker_verification_element.get_attribute('innerText')
        assert len(smart_ballot_tracker_verification_value) > 5

        assert smart_ballot_tracker_verification_value == voter["smart_ballot_tracker"]

        # He clicks on the "I cast my vote" button
        submit_button_css_selector = "#main input[type=submit]"
        submit_button_element = browser.find_element_by_css_selector(submit_button_css_selector)
        submit_button_element.click()



    def some_voters_cast_their_vote(self, voters):
        """
        :param voters: list of dict. Each element contains information about a voter (their e-mail address, the planned answers to each question they will cast)
        """
        browser = self.browser
        voters_count = len(voters)
        for index, voter in enumerate(voters):
            console_log("#### Current voter casting their vote in current batch: " + str(index + 1) + "/" + str(voters_count))

            self.one_voter_votes(voter)
            wait_a_bit()

            """
            Next screen looks like this:
            Your ballot for My test election for Scenario 1 has been accepted. Your smart ballot tracker is ISXe/rCNCVa9XcVeFgKglbpgo5SoZs4svT6dPbR5b6M. You can check its presence in the {ballot box} anytime during the election. A confirmation e-mail has been sent to you.

            {Go back to election}

            Where {xxx} is a link
            """

            # He clicks on the "ballot box" link
            ballot_box_link_label = "ballot box"
            ballot_box_link_element = wait_for_an_element_with_partial_link_text_exists(browser, ballot_box_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
            ballot_box_link_element.click()

            wait_a_bit()

            # He checks that his smart ballot tracker appears in the list
            all_smart_ballot_trackers_css_selector = "#main ul li a"
            all_smart_ballot_trackers_elements = wait_for_elements_exist(browser, all_smart_ballot_trackers_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
            assert len(all_smart_ballot_trackers_elements)
            matches = [element for element in all_smart_ballot_trackers_elements if element.get_attribute('innerText') == voter["smart_ballot_tracker"]]
            assert len(matches) is 1


            self.voters_email_addresses_who_have_voted[voter["email_address"]] = True

            # In a following pass, he checks his mailbox to find a new email with confirmation of his vote, and verifies the value of the smart ballot tracker written in this email is the same as the one he noted. This verification is done in a separated pass because of an optimization, so that we only re-read and re-populate the sendmail_fake text file once for all users.

            # He closes the window (there is no log-out link, because user is not logged in: credentials are not remembered)
            # It is not really mandatory for the test to close the window. Re-opening a browser takes much more time, compared to just navigating to another URL. So actually to save execution time, we choose to close the window only sometimes, randomly.
            if random.randint(0, 10) <= 3:
                browser.quit()
                self.browser = initialize_browser()
                browser = self.browser

        # Start another pass, where we re-read and re-populate the sendmail_fake text file once for all users.
        voters = repopulate_vote_confirmations_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters, settings.ELECTION_TITLE)
        for voter in voters:
            # He checks his mailbox to find a new email with confirmation of his vote, and verifies the value of the smart ballot tracker written in this email is the same as the one he noted.
            assert voter["smart_ballot_tracker"] == voter["smart_ballot_tracker_in_vote_confirmation_email"], "Ballot tracker read in vote confirmation email (" + voter["smart_ballot_tracker"] + ") is not the same as the one read on the vote confirmation page (" + voter["smart_ballot_tracker_in_vote_confirmation_email"] + ")"


    def one_voter_casts_after_the_election_is_closed(self, voter):
        browser = self.browser
        console_log("#### Current voter casting their vote after the election is closed")
        self.one_voter_votes(voter, direct=True)
        wait_a_bit()

        """
        Next screen looks like this:
        Your ballot for Test vote after close is rejected, because the election is closed.

        {Go back to election}

        Where {xxx} is a link
        """

        # He checks that "the election is closed" is present
        wait_for_element_exists_and_contains_expected_text(browser, "#main p", "the election is closed", settings.EXPLICIT_WAIT_TIMEOUT)


    def all_voters_vote(self):
        """
        This function selects a random set of `NUMBER_OF_VOTING_VOTERS` voters, and casts their vote.
        Note: If you rather want to cast votes and check consistency for every batch of votes, see function `all_voters_vote_in_sequences()`.
        """
        voters_who_will_vote_now = random.sample(self.voters_email_addresses, settings.NUMBER_OF_VOTING_VOTERS)
        voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters_who_will_vote_now, settings.ELECTION_TITLE)
        voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
        self.update_voters_data(voters_who_will_vote_now_data)
        self.some_voters_cast_their_vote(voters_who_will_vote_now_data)


    def all_voters_vote_in_sequences(self, verify_every_x_votes=5):
        """
        This function is a wrapper of some_voters_vote_in_sequences(), for readability.
        It selects a random set of `NUMBER_OF_VOTING_VOTERS` voters, and casts their vote, in batches of `verify_every_x_votes`, and checks vote data consistency after every batch of votes (using `belenios_tool verify-diff` and a snapshot of election data copied in previous batch).
        Note: If you rather want to cast votes without checking consistency, see function `all_voters_vote()`.
        """
        voters_who_will_vote_now = random.sample(self.voters_email_addresses, settings.NUMBER_OF_VOTING_VOTERS)
        self.some_voters_vote_in_sequences(voters_who_will_vote_now, start_index=0, end_index=settings.NUMBER_OF_VOTING_VOTERS, verify_every_x_votes=verify_every_x_votes)

    def some_voters_vote_in_sequences(self, voters=None, start_index=0, end_index=None, verify_every_x_votes=5):
        """
        Iterates over `voters` from index `start_index` to `end_index`, cast their vote, and checks vote data consistency for every batch of `verify_every_x_votes` votes (using `belenios_tool verify-diff` and a snapshot of election data copied in previous batch).
        """
        if start_index < 0:
            raise Exception("start_index cannot be below 0")
        current_start_index = start_index
        if end_index is None:
            end_index = settings.NUMBER_OF_VOTING_VOTERS
        elif end_index > settings.NUMBER_OF_VOTING_VOTERS:
            raise Exception("end_index cannot exceeed NUMBER_OF_VOTING_VOTERS")

        if voters is None:
            voters = self.voters_email_addresses
        voters_who_will_vote_now = voters[start_index:end_index]
        voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters_who_will_vote_now, settings.ELECTION_TITLE)
        voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
        self.update_voters_data(voters_who_will_vote_now_data)
        snapshot_folder = None

        while current_start_index < end_index:
            increment = verify_every_x_votes # could be randomized
            current_end_index = current_start_index + increment
            if current_end_index > end_index:
                current_end_index = end_index

            if current_start_index > 0:
                console_log("#### Starting substep: create_election_data_snapshot")
                snapshot_folder = create_election_data_snapshot(self.election_id)
                console_log("#### Substep complete: create_election_data_snapshot")

            try:
                console_log("#### A batch of " + str(current_end_index - current_start_index) + " voters, indexed " + str(current_start_index) + " to " + str(current_end_index - 1) + " are now going to vote")
                self.some_voters_cast_their_vote(voters_who_will_vote_now_data[current_start_index:current_end_index])
                console_log("#### A batch of " + str(current_end_index - current_start_index) + " voters, indexed " + str(current_start_index) + " to " + str(current_end_index - 1) + " have now voted")

                if current_start_index > 0:
                    console_log("#### Starting substep: verify_election_consistency using `belenios_tool verify-diff` (for a batch of votes)")
                    verify_election_consistency(self.election_id, snapshot_folder)
                    console_log("#### Substep complete: verify_election_consistency using `belenios_tool verify-diff` (for a batch of votes)")
            finally:
                if current_start_index > 0:
                    console_log("#### Starting substep: delete_election_data_snapshot")
                    delete_election_data_snapshot(snapshot_folder)
                    console_log("#### Substep complete: delete_election_data_snapshot")

            current_start_index += increment


    def some_voters_revote(self):
        voters_list_we_pick_from = self.voters_email_addresses_who_have_voted.keys()
        voters_who_will_vote_now = random.sample(voters_list_we_pick_from, settings.NUMBER_OF_REVOTING_VOTERS)
        voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters_who_will_vote_now, settings.ELECTION_TITLE)
        voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
        self.update_voters_data(voters_who_will_vote_now_data)
        self.some_voters_cast_their_vote(voters_who_will_vote_now_data)


    def one_voter_revotes_after_the_election_is_closed(self):
        voters_list_we_pick_from = self.voters_email_addresses_who_have_voted.keys()
        voters_who_will_vote_now = random.sample(voters_list_we_pick_from, 1)
        voters_who_will_vote_now_data = populate_credential_and_password_for_voters_from_sent_emails(self.fake_sent_emails_manager, voters_who_will_vote_now, settings.ELECTION_TITLE)
        voters_who_will_vote_now_data = populate_random_votes_for_voters(voters_who_will_vote_now_data)
        self.update_voters_data(voters_who_will_vote_now_data)
        self.one_voter_casts_after_the_election_is_closed(voters_who_will_vote_now_data[0])


    def administrator_verifies_vote_results(self):
        """
        Initial browser (required) state: on the vote results page
        Final browser state: on the accepted ballots page

        She checks consistency of the vote result:
        - 1) She checks that the number of accepted ballots is the same as the number of voters who voted
        - 2) For each available answer in the question, she checks that the total number of votes in favor of Answer X displayed in result page is the same as the sum of votes for Answer X in all votes of voters who voted that have been randomly generated in advance
        - 3) She checks that each ballot content corresponds to content that of this vote that has been randomly generated in advance


        This screen looks like this:

        This is the development version!
        By using this site, you accept our <personal data policy>. <Accept>
        <en> <fr> <de> <ro> <it>

        This election has been tallied.

            Question 1?
            Answer 1    6
            Answer 2    8

        Number of accepted ballots: 10
        You can also download the <result with cryptographic proofs>.

        <See accepted ballots>

        Where <...> is a link
        """

        browser = self.browser

        # - 1) She checks that the number of accepted ballots is the same as the number of voters who voted

        main_css_selector = "#main"
        main_expected_content = "Number of accepted ballots:"
        main_element = wait_for_element_exists_and_contains_expected_text(browser, main_css_selector, main_expected_content, settings.EXPLICIT_WAIT_TIMEOUT)
        main_text_content = main_element.get_attribute('innerText')

        number_of_accepted_ballots = None
        match = re.search(r'Number of accepted ballots:\s*(\d+)\s', main_text_content, re.MULTILINE | re.DOTALL)
        if match:
            number_of_accepted_ballots = match.group(1)
            number_of_accepted_ballots = number_of_accepted_ballots.strip()
        else:
            raise Exception("Number of accepted ballots not found in election tally page: " + main_text_content)
        assert str(number_of_accepted_ballots) == str(settings.NUMBER_OF_VOTING_VOTERS), "Number of accepted ballots (" + str(number_of_accepted_ballots) + ") is not the same as number of voters (" + str(settings.NUMBER_OF_VOTING_VOTERS) + ")"


        # - 2) For each available answer in the question, she checks that the total number of votes in favor of Answer X displayed in result page is the same as the sum of votes for Answer X in all votes of voters who voted that have been randomly generated in advance

        number_of_votes_per_answer = self.compute_number_of_votes_per_answer()
        question_id = 1
        for answer_id in range(1, 3):
            base_selector = "#main li:nth-child(" + str(question_id) + ") tr:nth-child(" + str(answer_id) + ")"
            answer_label_css_selector = base_selector + " td:nth-child(1)"
            answer_total_css_selector = base_selector + " td:nth-child(2)"
            answer_expected_label = "Answer " + str(answer_id)
            answer_element = browser.find_element_by_css_selector(answer_label_css_selector)
            verify_element_label(answer_element, answer_expected_label)
            answer_total_real_value_element = browser.find_element_by_css_selector(answer_total_css_selector)
            answer_total_real_value = answer_total_real_value_element.get_attribute('innerText').strip()
            answer_total_expected_value = str(number_of_votes_per_answer['answer' + str(answer_id)])
            assert answer_total_real_value == answer_total_expected_value, "Number of votes for Answer " + str(answer_id) + " displayed on vote result page  (" + answer_total_real_value + ") does not match expected value (" + answer_total_expected_value


        # - 3) She checks that each smart ballot tracker in the ballot box page corresponds to the smart ballot tracker of one of our voters, and that there is only one of these, and that the number of smart ballot trackers in this page is the same as the number of voters who voted

        all_ballots_link_label = "See accepted ballots"
        all_ballots_link_element = wait_for_an_element_with_partial_link_text_exists(browser, all_ballots_link_label, settings.EXPLICIT_WAIT_TIMEOUT)
        all_ballots_link_element.click()

        all_smart_ballot_trackers_css_selector = "#main ul li a"
        all_smart_ballot_trackers_elements = wait_for_elements_exist(browser, all_smart_ballot_trackers_css_selector, settings.EXPLICIT_WAIT_TIMEOUT)
        assert len(self.voters_email_addresses_who_have_voted) == settings.NUMBER_OF_VOTING_VOTERS
        assert len(all_smart_ballot_trackers_elements) == settings.NUMBER_OF_VOTING_VOTERS
        for voter_email_address in self.voters_email_addresses_who_have_voted:
            voter = self.voters_data[voter_email_address]
            matches = [element for element in all_smart_ballot_trackers_elements if element.get_attribute('innerText') == voter["smart_ballot_tracker"]]
            assert len(matches) is 1
