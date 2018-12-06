# Docker image used as FROM has been generated from ./Dockerfile_base_environment
FROM glondu/beleniosbase:20181206-2

USER root

# Install firefox-esr (The firefox package is not present in apt repositories of this linux image, so we use firefox-esr)
RUN apt-get install -y -qq firefox-esr

# Install geckodriver (It is needed by selenium to run firefox)
ENV GECKODRIVER_VERSION 0.18.0
RUN wget --no-verbose -O /tmp/geckodriver.tar.gz https://github.com/mozilla/geckodriver/releases/download/v$GECKODRIVER_VERSION/geckodriver-v$GECKODRIVER_VERSION-linux64.tar.gz && rm -rf /opt/geckodriver && tar -C /opt -zxf /tmp/geckodriver.tar.gz && rm /tmp/geckodriver.tar.gz && mv /opt/geckodriver /opt/geckodriver-$GECKODRIVER_VERSION && chmod 755 /opt/geckodriver-$GECKODRIVER_VERSION && ln -fs /opt/geckodriver-$GECKODRIVER_VERSION /usr/bin/geckodriver

# Install packages required to run the test scenario
RUN apt-get install -y -qq python3 python3-venv

USER belenios
