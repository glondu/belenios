# Documentation related to automated tests of Belenios

Automated tests are stored in the `tests` directory.

Technologies used to run these tests are:

- `python3`: Python 3. We us it in a virtual environment
- `pip`: Python's package manager. We use it to install Python packages such as `selenium` (`pip` installs packages mentioned in `requirements.txt`)
- `selenium`: Selenium's Python API documentation: https://selenium-python.readthedocs.io/)
- `firefox`: The browser we use to run tests with Selenium. We can use standard Firefox, or `firefox-esr`, depending what is available on the system and which Firefox version is compatible with Selenium at the moment
- `geckodriver`: A Firefox driver for Selenium
- `unittest`: Python's standard test framework

These automated tests start the Belenios demo server (`demo/run-server.sh`), with the `BELENIOS_SENDMAIL` environment variable defined as the path to a fake `sendmail` executable (similar to a mock, provided in `tests/tools/sendmail_fake.sh`). This way, Belenios server does not return an error when trying to send emails in the test environment (that has no `sendmail` installed nor configured), and the fake `sendmail` executable makes it possible to verify what emails have been sent and read their content, simply by reading the log file where it redirects all its input (we use `/tmp/sendmail_fake` as location for this log file).

Note: For example, during election creation procedure, a step sends emails to voters. If at this moment, a `sendmail` binary is not properly installed and configured (or replaced by a mock), the web page displays the following error message: `Netchannels.Command_failure(WEXITED 126)`

When these automated tests start running, and when they end, they clean up Belenios database: Belenios database consists in directories and files under the `_run/spool` directory, for each election. So these are deleted during test setup. Belenios demo server stores initial admin users logins and passwords in `demo/password_db.csv`. This file is not deleted during test setup, and its contents are used to log in the adminstrator during the test and have this administrator create an election. 

Automated tests can be executed manually, or via Continuous Integration. Next sub-sections explain how to execute them in each of these 2 contexts.


## Executing automated test suites manually on your local machine

You can execute a test suite by running its python script from your Python virtual environment, given you have already installed its `pip` requirements, `firefox` and `geckodriver`. Automated tests need that Belenios server has already been compiled. So if you have not done any of these, this is the sequence of commands that you can execute:

```
export GECKODRIVER_VERSION=0.18.0
wget --no-verbose -O /tmp/geckodriver.tar.gz https://github.com/mozilla/geckodriver/releases/download/v$GECKODRIVER_VERSION/geckodriver-v$GECKODRIVER_VERSION-linux64.tar.gz && rm -rf /opt/geckodriver && sudo tar -C /opt -zxf /tmp/geckodriver.tar.gz && rm /tmp/geckodriver.tar.gz && sudo mv /opt/geckodriver /opt/geckodriver-$GECKODRIVER_VERSION && sudo chmod 755 /opt/geckodriver-$GECKODRIVER_VERSION && sudo ln -fs /opt/geckodriver-$GECKODRIVER_VERSION /usr/bin/geckodriver

sudo apt-get install -y -qq python3 python3-venv firefox

BELENIOS_DEBUG=1 make all
make archive

python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python ./tests/test_scenario_1.py
```

Note: Depending on the version of `firefox` that you use, you may need to adjust the version of `geckodriver` to download and install. Please refer to this compatibility table: https://firefox-source-docs.mozilla.org/testing/geckodriver/geckodriver/Support.html

Note: The `make all` command exits in error if `ocamlbuild` finds compiled files (including `.so` shared libraries) other than the ones it expects. So, make sure that you run this command before creation of the Python virtual environment and installation of `pip` requirements. If you want to re-compile Belenios after this, you can simply delete the `venv` folder.

Note: We cannot run the Belenios server from a folder that is not the git repository root, otherwise it exits immediately in an error: `This script should be run from the root of the (built) source tree!`.


## Executing automated tests using Continuous Integration

For this, we run a Docker image (built from `Dockerfile_test_scenario_environment`) that preinstalls compatible versions of `firefox-esr`, `geckodriver`, `python`, and python virtual environment.

File `.gitlab-ci.yml` defines a task `build_and_run_automated_test_scenario_1_with_preinstalled_image` that uses this docker image, compiles belenios, creates a python virtual environment, steps into it, installs pip required packages, and then executes the automated test suite.

Note: You can execute this Continuous Integration task as if you were running GitlabCI, by executing the following command:

```
gitlab-runner exec docker build_and_run_automated_test_scenario_1_with_preinstalled_image
```

Note: The Docker image has been built and pushed to Docker Hub using the following commands:

```
docker build -t glondu/beleniosbase:20181206-1 -f Dockerfile_base_environment .
docker build -t glondu/beleniosbase-tests:20181206-1 -f Dockerfile_test_scenario_environment .
sudo docker push glondu/beleniosbase-tests:20181206-1
```

We use `YYYYMMDD-N` for docker-tagging, where `YYYYMMDD` is the build date, and `N` is a sequence number.


## Customizing configuration variables of Test Scenario 1

Test `test_scenario_1.py` executes using some default configuration that can be customized, by setting some environment variables when executing the script.

The list of configuration variables is:

- `RANDOM_SEED`: An integer used as seed for the random number generator of Python. By default, a random seed is used. Seed used is displayed at every run of the test, so you can re-run the test using the same random seed by setting this configuration variable to the value displayed.
- `USE_HEADLESS_BROWSER`: Set this to non-zero (True) if you run this test in Continuous Integration (it has no graphical display). Set this to 0 (False) if you want to see the browser open its graphical user interface and visually track progress ot the test. By default, the falue is True.
- `WAIT_TIME_BETWEEN_EACH_STEP`: In seconds (float). Time we wait between each action that we tell Selenium driver to do in the browser. Set to 0 if you don't need to have the time to visually follow progress of actions in the browser
- `EXPLICIT_WAIT_TIMEOUT`: In seconds. Maximum duration Selenium driver will wait for appearance of a specific DOM element expected in the page (for example when transitioning from a page to another). This referes to Selenium's "Explicit Wait" concept
- `SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH`
- `NUMBER_OF_INVITED_VOTERS`: This is N in description of Scenario 1. N is between 6 (quick test) and 1000 (load testing)
- `NUMBER_OF_VOTING_VOTERS`: This is K in description of Scenario 1. K is between 6 (quick test) and 1000 (load testing). K <= N. (Some invited voters don't vote, this is abstention, and its value is N - K)
- `NUMBER_OF_REVOTING_VOTERS`: This is L in description of Scenario 1. L <= K
- `NUMBER_OF_REGENERATED_PASSWORD_VOTERS`: This is M in description of Scenario 1. M <= K
- `SENT_EMAILS_TEXT_FILE_ABSOLUTE_PATH`: By default, this is "/tmp/sendmail_fake"
- `ADMINISTRATOR_USERNAME`: By default, this value comes from file `demo/password_db.csv`, first row, first column
- `ADMINISTRATOR_PASSWORD`: By default, this value comes from file `demo/password_db.csv`, first row, 4th column

Here is an example of how you can set configuration variables and execute the test in your terminal:

```
RANDOM_SEED=222 WAIT_TIME_BETWEEN_EACH_STEP=1.2 USE_HEADLESS_BROWSER=0 NUMBER_OF_INVITED_VOTERS=4 python3 ./tests/test_scenario_1.py
```

## Prepared database

You can construct a prepared database (contents of folder `_run/spool`) with several elections and invited voters, and optionnaly with some voters who have already submitted their ballot. Building such a database, loading it into Belenios and executing automated tests on it make it possible to evaluate any difference in behaviour or in duration between an "empty" Belenios server and a "crowded" one.

File `.gitlab-ci.yml` defines a task `build_and_run_automated_test_scenarios_with_preinstalled_image_and_prepared_database` that downloads a prepared database and places it into `_run/spool` before executing tests. This task is only executed once in a while (at every new release of Belenios), not at every commit.

Here is how you can build your own prepared database (for example if you want to add new elections to the prepared database used in this task):

- Execute the Python/Selenium script which creates an election, invites some voters, generates their ballots, and stores them into a CSV file: See section `Execute the script that creates the election and generates ballot files and the aggregated votes CSV file`
- Execute the JMeter script which, for each row of the previously generated CSV file, submits a voter's ballot to the election: See section `Load testing: Executing scenario of vote with already prepared ballots, using JMeter`

You can now execute automated tests locally and analyse their results. If you want to update or replace the prepared database used in the Continuous Integration / Continuous Delivery pipeline, download its compressed file, unzip its contents and merge them into your local `_run/spool` folder. Then compress the whole folder and upload its compressed version to a public URL and replace the URL in `.gitlab-ci.yml` by your own.

Good to know: If you version the prepared database using a git repository like gitlab or github, the VCS platform automatically generates a compressed file associated to latest commit on master branch and to commits which have tags. So you don't need to compress and upload the file yourself, just copy its URL and paste it into `.gitlab-ci.yml`.

## Monkey testing and fuzz testing

You can execute test `test_fuzz.py` the following way:

```
START_SERVER=1 LOGIN_MODE="local" USE_HEADLESS_BROWSER=0 WAIT_TIME_BETWEEN_EACH_STEP=0.02 python ./tests/test_fuzz.py 
```

For environment variable `WAIT_TIME_BETWEEN_EACH_STEP`, do not set a value below 0.02 seconds, otherwise hypothesis test becomes flaky.

