#!/usr/bin/python
# coding: utf-8

from os.path import abspath, dirname, join
from enum import Enum, unique

SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "demo/run-server.sh"
SERVER_URL = "http://127.0.0.1:8001"
START_SERVER = False
ELECTION_ID = None
DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY = "_run/spool"
FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "tests/selenium/tools/sendmail_fake.sh"
SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = "/tmp/sendmail_fake"
FAKE_SENT_EMAILS_FILE_RELATIVE_URL = "mail/mails.txt"
USE_HEADLESS_BROWSER = True # Set this to True if you run this test in Continuous Integration (it has no graphical display)
WAIT_TIME_BETWEEN_EACH_STEP = 0 # In seconds (float). Time we wait between each action that we tell Selenium driver to do in the browser. Set to 0 if you don't need to have the time to visually follow progress of actions in the browser
EXPLICIT_WAIT_TIMEOUT = 30 # In seconds. Maximum duration Selenium driver will wait for appearance of a specific DOM element expected in the page (for example when transitioning from a page to another). This referes to Selenium's "Explicit Wait" concept

NUMBER_OF_INVITED_VOTERS = 20 # This is N in description of Scenario 1. N is between 6 (quick test) and 1000 (load testing)
NUMBER_OF_VOTING_VOTERS = 10 # This is K in description of Scenario 1. K is between 6 (quick test) and 1000 (load testing). K <= N. (Some invited voters don't vote, this is abstention, and its value is N - K)
NUMBER_OF_MONKEY_VOTING_VOTERS = 4 # In test `test_scenario_2_with_monkeys.py`, this is the number of voters who will act as smart monkeys (who complete their vote). This set of users are part of the bigger set NUMBER_OF_VOTING_VOTERS, so NUMBER_OF_MONKEY_VOTING_VOTERS <= NUMBER_OF_VOTING_VOTERS. If NUMBER_OF_MONKEY_VOTING_VOTERS > 0, then its value must make result the of `(NUMBER_OF_VOTING_VOTERS - NUMBER_OF_MONKEY_VOTING_VOTERS) / 2` be an integer.
NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART = 3 # In test `test_scenario_2_with_monkeys.py`, this is the number of non-monkey voters who will vote at first. Then NUMBER_OF_MONKEY_VOTING_VOTERS monkeys vote. Then (NUMBER_OF_VOTING_VOTERS - NUMBER_OF_VOTING_VOTERS_IN_FIRST_PART - NUMBER_OF_MONKEY_VOTING_VOTERS) vote.
NUMBER_OF_REVOTING_VOTERS = 5 # This is L in description of Scenario 1. L <= K
NUMBER_OF_REGENERATED_PASSWORD_VOTERS = 4 # This is M in description of Scenario 1. M <= K
ELECTION_TITLE = "My test election for Scenario 1"
ELECTION_DESCRIPTION = "This is the description of my test election for Scenario 1"
LOGIN_MODE = "local"
ADMINISTRATOR_USERNAME = "user1" # This value comes from file `demo/password_db.csv`, first row, first column
ADMINISTRATOR_PASSWORD = "phiexoey" # This value comes from file `demo/password_db.csv`, first row, 4th column
INITIATOR_CONTACT = "Election initiator <election.initiator@mytestelection.com>"

GIT_REPOSITORY_ABSOLUTE_PATH = dirname(dirname(dirname(abspath(__file__))))
GENERATED_FILES_DESTINATION_FOLDER = join(GIT_REPOSITORY_ABSOLUTE_PATH, "_testdata")


@unique
class CLEAN_UP_POLICIES(Enum):
    REMOVE_DATABASE = "REMOVE_DATABASE"
    REMOVE_ELECTION = "REMOVE_ELECTION"
    DO_NOTHING = "DO_NOTHING"


@unique
class BOOTH_VERSIONS(Enum):
    CLASSIC_BOOTH = "CLASSIC_BOOTH"
    RESPONSIVE_BOOTH = "RESPONSIVE_BOOTH"


CLEAN_UP_POLICY = CLEAN_UP_POLICIES.REMOVE_DATABASE


# These variables are used by Scenario 2 only
BROWSER_DOWNLOAD_FOLDER = "/tmp"
ADMINISTRATOR_EMAIL_ADDRESS = "alice_aka_election_administrator@example.org"
CREDENTIAL_AUTHORITY_EMAIL_ADDRESS = "cecily_aka_election_credential_authority@example.org"
NUMBER_OF_TRUSTEES = 5
TRUSTEES_EMAIL_ADDRESSES = ["tom_aka_trustee_1@example.org", "taylor_aka_trustee_2@example.org", "tania_aka_trustee_3@example.org", "tiffany_aka_trustee_4@example.org", "theresa_aka_trustee_5@example.org"]
TRUSTEES_THRESHOLD_VALUE = 3
BOOTH_VERSION = BOOTH_VERSIONS.CLASSIC_BOOTH
