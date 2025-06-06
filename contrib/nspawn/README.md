Deploying Belenios using systemd-nspawn
=======================================


Introduction
------------

This file documents a technique with a (relatively) small runtime
footprint on the deployment server. With this technique, the
"development" and the "deployment" environments are distinguished and
can be on different machines. The benefit is that the development
environment takes several gigabytes while the deployment environment
takes less than 100 MB. Several instances can be deployed on the same
machine. It is expected that these instances listen on localhost, and
that a reverse-proxy on the host exposes them on different vhosts or
directories of some vhost, with TLS. Configuring the reverse-proxy is
documented [here](../../doc/reverse-proxy.md).

This technique requires a Debian-based Linux (trixie/sid at the time
of writing) and working unprivileged user namespaces. On other (Linux)
systems, a privileged Docker container can also be used. If you don't
trust something, it is suggested to use a virtual machine.

The whole procedure requires (at least) the following Debian packages
installed on the development host:
- `mmdebstrap`
- `uidmap`
- `bubblewrap`
- `devscripts`
- `squashfs-tools-ng`
- `zstd`
- `git`
- `sbuild`


Building the image
------------------

The whole process takes approx. 15 min, depending on the host and
Internet connection.

### On Debian-based systems

Set `TMPDIR` to a directory with sufficient space (>= 10 GB).

Set `TARGET` to a directory where artifacts will be put.

Run:

    contrib/debian/setup-build-dir.sh "$TARGET"

This will set up `$TARGET` with a `Makefile`. Then, go to this
directory and run:

    make

### On non-Debian-based systems (using Docker)

Make sure your `/var/lib/docker` has sufficient space (>= 10 GB).

Build the building environment:

    docker build -f contrib/docker/nspawn-build.Dockerfile -t belenios-nspawn-image .

Prepare the directory on the host where images will be built:

    mkdir -p _docker
    docker run --rm belenios-nspawn-image tar -C /tmp -c build | tar -C _docker -x

Build the image:

    docker run --privileged --tty --rm --volume=$PWD:/tmp/belenios --volume=$PWD/_docker/build:/tmp/build belenios-nspawn-image make


Deploying
---------

We assume the deployment server runs Linux, systemd and has
`systemd-nspawn` installed. We also assume that `systemd-resolved` is
running, and an [MTA](../../doc/mta.md) is installed and listening on
localhost.

When deploying on a server for the first time, run:

 * `mkdir /srv/belenios-containers`
 * `cp belenios-nspawn /srv/belenios-containers`
 * `cp belenios-container@.service /etc/systemd/system`

You might want to update `belenios-nspawn` and
`belenios-container@.service` as Belenios evolves.

Let `SQUASHFS` be the `.squashfs` image, built in the previous
section.

To deploy an instance named `main`:

 * create a directory `/srv/belenios-containers/main`
 * copy there `$SQUASHFS`
 * make there a symlink `rootfs.squashfs` pointing to `$SQUASHFS`
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
called `belenios-server.service` so you can, for example, run:

    systemctl status belenios-server

to see if the unit is running and debug it if needed. You can also use
`journalctl` or read Belenios-specific logs in `/var/belenios/log`.


### Mails are not delivered

Be sure the MTA on the host works properly. Its logs can be helpful in
debugging mail delivery problems.

Usually, the `return-path` setting in `ocsigenserver.conf.in` must be
set to a valid e-mail address.
