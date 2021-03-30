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
out of the scope of this document.

This technique requires Linux and root privileges. If you don't trust
something, it is suggested to use a virtual machine.


Building the development environment
------------------------------------

This requires `debootstrap` and `systemd-nspawn` (provided by the
`systemd-container` package in Debian).

The `belenios-stage1.sh` script (which must be run as root) takes a
directory as argument and bootstraps in it a Debian distribution with
the appropriate prerequisites, and runs `opam-bootstrap.sh` inside it.

One can log into the resulting environment with the command:

    systemd-nspawn --directory=/path/to/directory --user=belenios

To work on Belenios inside this environment, one must source the
`/home/belenios/opam-env.sh` file. One can then clone Belenios sources
(they are expected in `/home/belenios/belenios`), and build them using
`make build-release-server` (or `make build-debug-server`).


Building the deployment environment
-----------------------------------

The deployment environment must be built from inside the development
environment created above. It consists in a squashfs image of a root
file system, and a directory which is mounted at `/var/belenios`
inside the system.

The image is built with the `belenios-stage3.sh` script, which creates
`/target/rootfs.squashfs`.

The directory can be created empty with just an
`ocsigenserver.conf.in` file (which should be based on the one that
can be found in Belenios sources). Beware, this directory and its
contents must be owned by the `belenios` user.


Deploying
---------

We assume the deployment server runs Linux, systemd and has
`systemd-nspawn` installed. We also assume that `systemd-resolved` is
running, and an MTA is installed and listening on localhost.

When deploying on a server for the first time, run:

 * `mkdir /srv/belenios-containers`
 * `cp belenios-nspawn /srv/belenios-containers`
 * `cp belenios-container@.service /etc/systemd/system`

You might want to update `belenios-nspawn` and
`belenios-container@.service` as Belenios evolves.

To deploy an instance named `main`, create a directory
`/srv/belenios-containers/main`, and copy there `rootfs.squashfs` and
the `belenios` directory (beware of the owner). You can then run the
instance with:

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
