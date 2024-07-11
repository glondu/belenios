FROM debian:testing
RUN apt-get update -qq && apt-get upgrade -qq && apt-get install -qq build-essential mmdebstrap bubblewrap devscripts squashfs-tools-ng zstd git
RUN useradd --create-home belenios
COPY --chown=belenios:belenios . /tmp/belenios
USER belenios
WORKDIR /tmp/belenios
RUN contrib/unshare/setup-build-dir.sh bookworm 1 /tmp /tmp/build
WORKDIR /tmp/build
RUN git config --global user.name "Belenios Builder"
RUN git config --global user.email "belenios.builder@example.org"
