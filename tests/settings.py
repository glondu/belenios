#!/usr/bin/python
# coding: utf-8

from os.path import abspath, dirname

SERVER_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "demo/run-server.sh"
SERVER_URL = "http://localhost:8001"
DATABASE_FOLDER_PATH_RELATIVE_TO_GIT_REPOSITORY = "_run/spool"
FAKE_SENDMAIL_EXECUTABLE_FILE_PATH_RELATIVE_TO_GIT_REPOSITORY = "tests/tools/sendmail_fake.sh"
SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH = "/tmp/sendmail_fake"
USE_HEADLESS_BROWSER = True # Set this to True if you run this test in Continuous Integration (it has no graphical display)
WAIT_TIME_BETWEEN_EACH_STEP = 0 # In seconds (float). Time we wait between each action that we tell Selenium driver to do in the browser. Set to 0 if you don't need to have the time to visually follow progress of actions in the browser
EXPLICIT_WAIT_TIMEOUT = 30 # In seconds. Maximum duration Selenium driver will wait for appearance of a specific DOM element expected in the page (for example when transitioning from a page to another). This referes to Selenium's "Explicit Wait" concept

NUMBER_OF_INVITED_VOTERS = 20 # This is N in description of Scenario 1. N is between 6 (quick test) and 1000 (load testing)
NUMBER_OF_VOTING_VOTERS = 10 # This is K in description of Scenario 1. K is between 6 (quick test) and 1000 (load testing). K <= N. (Some invited voters don't vote, this is abstention, and its value is N - K)
NUMBER_OF_REVOTING_VOTERS = 5 # This is L in description of Scenario 1. L <= K
NUMBER_OF_REGENERATED_PASSWORD_VOTERS = 4 # This is M in description of Scenario 1. M <= K
ELECTION_TITLE = "My test election for Scenario 1"
ELECTION_DESCRIPTION = "This is the description of my test election for Scenario 1"
ADMINISTRATOR_USERNAME = "user1" # This value comes from file `demo/password_db.csv`, first row, first column
ADMINISTRATOR_PASSWORD = "phiexoey" # This value comes from file `demo/password_db.csv`, first row, 4th column

GIT_REPOSITORY_ABSOLUTE_PATH = dirname(dirname(abspath(__file__)))


# These variables are used by Scenario 2 only
BROWSER_DOWNLOAD_FOLDER = "/tmp"
ADMINISTRATOR_EMAIL_ADDRESS = "alice_aka_election_administrator@mailinator.com"
CREDENTIAL_AUTHORITY_EMAIL_ADDRESS = "cecily_aka_election_credential_authority@mailinator.com"
TRUSTEES_EMAIL_ADDRESSES = ["tom_aka_trustee_1@mailinator.com", "taylor_aka_trustee_2@mailinator.com"]
