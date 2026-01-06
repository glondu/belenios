# syntax=docker/dockerfile:1

# Base environment, for running opam-bootstrap.sh
FROM debian:13 AS beleniosbase
RUN apt-get update -qq && apt-get upgrade -qq && apt-get install -qq bubblewrap build-essential libgmp-dev libsodium-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates zip unzip libncurses-dev zlib1g-dev libgd-securityimage-perl cracklib-runtime git jq npm rsync
RUN useradd --create-home belenios
COPY opam-bootstrap.sh /home/belenios
COPY vendor/opam-overlay /home/belenios/vendor/opam-overlay
USER belenios
WORKDIR /home/belenios
RUN BELENIOS_OPAM_INIT_ARGS=--disable-sandboxing ./opam-bootstrap.sh

# Testing environment, for running firefox/webdriver tests
FROM beleniosbase AS beleniosbase-tests
USER root
RUN apt-get install -y -qq firefox-esr
ENV GECKODRIVER_VERSION=0.36.0
RUN wget --no-verbose -O /tmp/geckodriver.tar.gz https://github.com/mozilla/geckodriver/releases/download/v$GECKODRIVER_VERSION/geckodriver-v$GECKODRIVER_VERSION-linux64.tar.gz && rm -rf /opt/geckodriver && tar -C /opt -zxf /tmp/geckodriver.tar.gz && rm /tmp/geckodriver.tar.gz && mv /opt/geckodriver /opt/geckodriver-$GECKODRIVER_VERSION && chmod 755 /opt/geckodriver-$GECKODRIVER_VERSION && ln -fs /opt/geckodriver-$GECKODRIVER_VERSION /usr/bin/geckodriver
USER belenios
