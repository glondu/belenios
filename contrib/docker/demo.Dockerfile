FROM glondu/beleniosbase:20260106-1
# alternatively, run
#   docker build -f contrib/docker/gitlab-ci.Dockerfile -t beleniosbase --target beleniosbase ."
# and replace with the line below
#   FROM beleniosbase

ADD --chown=belenios . /home/belenios/
RUN . /home/belenios/.belenios/env.sh \
    && make build-debug-server

ENV BELENIOS_SENDMAIL=tests/sendmail_fake_to_static.sh
CMD ./demo/run-server.sh --debug
