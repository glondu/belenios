# Belenios demo server with Docker

The goal of the following instructions is to quickly evaluate
Belenios. The recommended way to deploy Belenios in production is to
use [systemd-nspawn](../nspawn/README.md).

Use the following command to build the image:

    git archive HEAD | docker build -f contrib/docker/demo.Dockerfile -t belenios-demo -

Use the following command to run the container:

    docker run --network=host belenios-demo

The server will be accessible at http://127.0.0.1:8001/

Sent mails will be accessible at http://127.0.0.1:8001/static/mail.txt
