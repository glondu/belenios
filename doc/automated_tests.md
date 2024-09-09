# Documentation related to automated tests of Belenios

Automated tests are stored in the `tests` directory.

Technologies used to run these tests are:

- [WebDriver](https://www.w3.org/TR/webdriver1/)
- `firefox`: The browser we use to run tests with WebDriver. We can
  use standard Firefox, or `firefox-esr`, depending what is available
  on the system and compatible at the moment.
- `geckodriver`: A Firefox WebDriver implementation. Its role is to
  translate commands received via WebDriver API into concrete actions
  in the Firefox browser.

These automated tests start the Belenios demo server
(`demo/run-server.sh`), with the `BELENIOS_SENDMAIL` environment
variable defined as the path to a fake `sendmail` executable (similar
to a mock, provided in `tests/sendmail_fake.sh`). This way, Belenios
server does not return an error when trying to send emails in the test
environment (that has no `sendmail` installed nor configured), and the
fake `sendmail` executable makes it possible to verify what emails
have been sent and read their content, simply by reading the log file
where it redirects all its input (we use `/tmp/sendmail_fake` as
location for this log file).

Automated tests can be executed manually, or via Continuous
Integration. Next sub-sections explain how to execute them in each of
these 2 contexts.


## Executing automated test suites manually on your local machine

You need to install `firefox` and `geckodriver` first. Automated tests
need that Belenios server has already been compiled. So if you have
not done any of these, this is the sequence of commands that you can
execute:

```
export GECKODRIVER_VERSION=0.18.0
wget --no-verbose -O /tmp/geckodriver.tar.gz https://github.com/mozilla/geckodriver/releases/download/v$GECKODRIVER_VERSION/geckodriver-v$GECKODRIVER_VERSION-linux64.tar.gz && rm -rf /opt/geckodriver && sudo tar -C /opt -zxf /tmp/geckodriver.tar.gz && rm /tmp/geckodriver.tar.gz && sudo mv /opt/geckodriver /opt/geckodriver-$GECKODRIVER_VERSION && sudo chmod 755 /opt/geckodriver-$GECKODRIVER_VERSION && sudo ln -fs /opt/geckodriver-$GECKODRIVER_VERSION /usr/bin/geckodriver

sudo apt-get install -y -qq firefox

source ./env.sh
make build-debug-server
make build-debug-tool

mkdir -p /tmp/tests
HOME=/tmp/tests dune exec -- tests/webdriver/main.exe run "BELENIOS_SENDMAIL=tests/sendmail_fake.sh exec demo/run-server.sh" run "LC_ALL=C.UTF-8 exec geckodriver --port=4444" scenario demo one 1 basic email
```

Note: Depending on the version of `firefox` that you use, you may need
to adjust the version of `geckodriver` to download and install. Please
refer to this [compatibility
table](https://firefox-source-docs.mozilla.org/testing/geckodriver/geckodriver/Support.html).


## Executing automated tests using Continuous Integration

For this, we run a Docker image (built from
`contrib/docker/gitlab-ci.Dockerfile`) that preinstalls compatible
versions of `firefox-esr`, `geckodriver`.

File `.gitlab-ci.yml` defines a task
`build_and_run_automated_test_scenarios_with_preinstalled_image` that
uses this docker image, compiles belenios, and then executes the
automated test suite.

Note: You can execute this Continuous Integration task as if you were
running GitlabCI, by executing the following command:

```
gitlab-runner exec docker build_and_run_automated_test_scenarios_with_preinstalled_image
```

Note: The Docker image has been built and pushed to Docker Hub using
the following commands:

```
docker-compose -f contrib/docker/gitlab-ci.docker-compose.yml build
docker tag beleniosbase glondu/beleniosbase:YYYYMMDD-N
docker tag beleniosbase-tests glondu/beleniosbase-tests:YYYYMMDD-N
docker push glondu/beleniosbase-tests:YYYYMMDD-N
docker push glondu/beleniosbase:YYYYMMDD-N
```

We use `YYYYMMDD-N` for docker-tagging, where `YYYYMMDD` is the build
date, and `N` is a sequence number.
