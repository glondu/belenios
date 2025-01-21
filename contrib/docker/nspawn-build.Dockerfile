FROM debian:unstable
RUN apt-get update -qq && apt-get upgrade -qq && apt-get install -qq build-essential mmdebstrap bubblewrap devscripts squashfs-tools-ng zstd git sbuild
RUN useradd --create-home belenios
COPY --chown=belenios:belenios . /tmp/belenios
WORKDIR /tmp/belenios
RUN cp contrib/debian/ocaml-backports-keyring.asc /etc/apt/trusted.gpg.d
RUN contrib/debian/install-deps.sh
USER belenios
RUN contrib/debian/setup-build-dir.sh /tmp/build
WORKDIR /tmp/build
RUN git config --global user.name "Belenios Builder"
RUN git config --global user.email "belenios.builder@example.org"
