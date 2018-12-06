# Documentation related to automated tests of Belenios

Automated tests are stored in the `tests` directory.

Technologies used to run these tests are: Python 3 in a virtual environment, `pip` (to install Python packages such as `selenium`), `selenium`, `firefox`, `geckodriver` (a firefox driver for selenium), `unittest`.

These automated tests start the Belenios demo server (`demo/run-server.sh`), with the `BELENIOS_SENDMAIL` environment variable defined as the path to a fake `sendmail` executable (similar to a mock, provided in `tests/tools/sendmail_fake.sh`). This way, Belenios server does not return an error when trying to send emails in the test environment (that has no `sendmail` installed nor configured), and the fake `sendmail` executable makes it possible to verify what emails have been sent and read their content, simply by reading the log file where it redirects all its input (we use `/tmp/sendmail_fake` as location for this log file).

Note: For example, during election creation procedure, a step sends emails to voters. If at this moment, a `sendmail` binary is not properly installed and configured (or replaced by a mock), the web page displays the following error message: `Netchannels.Command_failure(WEXITED 126)`

When these automated tests start running, and when they end, they clean up Belenios database: Belenios database consists in directories and files under the `_run/spool` directory, for each election. So these are deleted during test setup. Belenios demo server stores initial admin users logins and passwords in `demo/password_db.csv`. This file is not deleted during test setup, and its contents are used to log in the adminstrator during the test and have this administrator create an election. 

Automated tests can be executed manually, or via Continuous Integration.

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

File `.gitlab-ci.yml` defines a task `build_and_run_automated_test_scenario_1_with_preinstalled_image` that uses this docker image, compiles belenios, creates a python virtual environment, step into it, install pip required packages, and then executes the automated test suite.

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
