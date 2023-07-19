Deploying Belenios using systemd-nspawn
=======================================


Introduction
------------

This file documents a technique with a (relatively) small runtime
footprint on the deployment server. With this technique, the
"development" and the "deployment" environments are distinguished and
can be on different machines. The benefit is that the development
environment takes more than 3.5 GB while the deployment environment
takes a few hundreds MB. Several instances can be deployed on the same
machine. It is expected that these instances listen on localhost, and
that a reverse-proxy on the host exposes them on different vhosts or
directories of some vhost, with TLS. Configuring the reverse-proxy is
documented [here](../reverse-proxy.md).

This technique requires Linux and root privileges. If you don't trust
something, it is suggested to use a virtual machine.


Building the development environment
------------------------------------

This requires `debootstrap` and `systemd-nspawn` (provided by the
`systemd-container` package in Debian).

You must also enable and start `systemd-resolved` on the host, with
the following commands:

   systemctl enable systemd-resolved
   systemctl start systemd-resolved

The `belenios-stage1.sh` script (which must be run as root) takes a
directory as argument and bootstraps in it a Debian distribution with
the appropriate prerequisites, and runs `opam-bootstrap.sh` inside it.

One can log into the resulting environment with the command:

    systemd-nspawn --directory=/path/to/development/environment --user=belenios

To work on Belenios inside this environment, one must source the
`/home/belenios/opam-env.sh` file. One can then clone Belenios sources
(they are expected in `/home/belenios/belenios`), and build them using
`make build-release-server` (or `make build-debug-server`).


Building the deployment environment
-----------------------------------

The deployment environment must be built from inside the development
environment created above. It is a squashfs image of a root file
system.

The image is built with the `belenios-stage3.sh` script, which creates
`/target/rootfs.squashfs`.

To run the script from the host, use the following command:

    systemd-nspawn --directory=/path/to/development/environment /home/belenios/belenios/doc/nspawn/belenios-stage3.sh


Deploying
---------

We assume the deployment server runs Linux, systemd and has
`systemd-nspawn` installed. We also assume that `systemd-resolved` is
running, and an [MTA](../mta.md) is installed and listening on
localhost.

When deploying on a server for the first time, run:

 * `mkdir /srv/belenios-containers`
 * `cp belenios-nspawn /srv/belenios-containers`
 * `cp belenios-container@.service /etc/systemd/system`

You might want to update `belenios-nspawn` and
`belenios-container@.service` as Belenios evolves.

To deploy an instance named `main`:

 * create a directory `/srv/belenios-containers/main`
 * copy there `rootfs.squashfs`
 * create a `belenios` sub-directory belonging to user 1000 and
   group 1000
 * create there sub-directories `etc` and `var`
 * create `etc/ocsigenserver.conf.in` file (you can take the one
   in Belenios sources as example)

Beware, the `belenios` directory and its contents must belong to user
and group 1000, which correspond to user and group `belenios` inside
the deployment environment, but may appear as different names on the
host.

You can then run the instance with:

    systemctl start belenios-container@main.service

You should now be able to browse to your instance!

Of course, you can run other `systemctl` commands such as `status`,
`enable`, etc.


Troubleshooting
---------------


### Cannot connect to Belenios web server

You can open a shell inside the container with (replace `main` with
the name of your instance):

    machinectl shell belenios-main

The systemd unit running the web server (inside the container) is
called `belenios.service` so you can, for example, run:

    systemctl status belenios

to see if the unit is running and debug it if needed. You can also use
`journalctl` or read Belenios-specific logs in `/var/belenios/log`.


### Mails are not delivered

Be sure the MTA on the host works properly. Its logs can be helpful in
debugging mail delivery problems.

Usually, the `return-path` setting in `ocsigenserver.conf.in` must be
set to a valid e-mail address.
