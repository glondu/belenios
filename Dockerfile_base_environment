FROM debian:9
RUN apt-get update -qq && apt-get upgrade -qq && apt-get install -qq bubblewrap build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates zip unzip aspcud libncurses-dev uuid-runtime zlib1g-dev libgd-securityimage-perl cracklib-runtime git
RUN useradd --create-home belenios
COPY .opamrc-nosandbox /home/belenios/.opamrc
COPY opam-bootstrap.sh /home/belenios
USER belenios
WORKDIR /home/belenios
RUN ./opam-bootstrap.sh
