Belenios compilation instructions
=================================

The easy way
------------

Belenios is written in OCaml and has some dependencies towards
third-party OCaml libraries. The easiest and most portable way to
compile Belenios from source is to use
[OPAM](http://opam.ocamlpro.com/), which is a package manager for
OCaml projects.

The non-OCaml prerequisites are:

 * a POSIX system with a C compiler
 * on Linux, [Bubblewrap](https://github.com/projectatomic/bubblewrap)
 * [GMP](http://gmplib.org/)
 * [PCRE](http://www.pcre.org/)
 * [pkg-config](http://www.freedesktop.org/wiki/Software/pkg-config/)
 * [m4](https://www.gnu.org/software/m4/)
 * [SQLite3](https://www.sqlite.org/)
 * [OpenSSL](https://www.openssl.org/)
 * [Wget](https://www.gnu.org/software/wget/) or [curl](http://curl.haxx.se/)
 * [Zip](http://www.info-zip.org/Zip.html)
 * [Unzip](http://www.info-zip.org/UnZip.html)
 * [ncurses](http://invisible-island.net/ncurses/)
 * [uuidgen](https://www.kernel.org/pub/linux/utils/util-linux/)
 * [GD-SecurityImage](https://metacpan.org/release/GD-SecurityImage)
 * [cracklib](https://github.com/cracklib/cracklib)
 * [jq](https://github.com/stedolan/jq)

These libraries and tools are pretty common, and might be directly part
of your operating system. On [Debian](http://www.debian.org/) and its
derivatives, they can be installed with the following command:

    sudo apt install bubblewrap build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates zip unzip libncurses-dev uuid-runtime zlib1g-dev libgd-securityimage-perl cracklib-runtime jq

If you are unfamiliar with OCaml or OPAM, we provide an
`opam-bootstrap.sh` shell script that creates a whole, hopefully
self-contained, OCaml+OPAM install, and then installs all the
dependencies of Belenios, everything into a single directory. You can
choose the directory by setting the `BELENIOS_SYSROOT` environment
variable, or it will take `~/.belenios` by default. Just run:

    ./opam-bootstrap.sh

On a modern desktop system, this needs approximately 20 minutes and 2.6
gigabytes of disk space.

If everything goes successfully, follow the given instructions to
update your shell environment, then run:

    make build-release-server

and you can skip the next two sections and go directly to the
_Documentation_ section.

You can also compile a debug version by using:

    make build-debug-server

Note that this version may introduce vulnerabilities and should not be
used in production!

To make sure everything went well, you can run tests:

    make check

If you are familiar with OCaml, please read the `opam-bootstrap.sh`
shell script, or the following two sections to compile Belenios with
your existing OCaml installation.

Command-line tool
-----------------

To compile the command-line tool, you will need:

 * [OCaml](https://ocaml.org/)
 * [Dune](https://dune.build/)
 * [Zarith](https://github.com/ocaml/Zarith)
 * [Uuidm](http://erratique.ch/software/uuidm)
 * [Cryptokit](https://github.com/xavierleroy/cryptokit)
 * [Atdgen](https://github.com/ahrefs/atd)
 * [Yojson](https://github.com/ocaml-community/yojson)
 * [Cmdliner](http://erratique.ch/software/cmdliner)

With OPAM, these dependencies can be installed with the following
command:

    opam install dune atdgen zarith cryptokit uuidm cmdliner

Once all the dependencies have been installed, the command-line tool
can be compiled with:

    make

It produces a single executable, `belenios-tool`, in the
`_build/install/default/bin` directory. You can install it in your
`PATH` (which we will assume in the guides), or refer to it with a
full path.

Web server
----------

The web server has the following additional dependencies:

 * [Calendar](http://calendar.forge.ocamlcore.org/)
 * [Eliom](http://ocsigen.org/eliom/)
 * [Csv](https://github.com/Chris00/ocaml-csv)

With OPAM, you can install them with:

    opam install calendar eliom csv

Once all the dependencies have been installed, the Eliom module can be
compiled with:

    make build-release-server

It will produce a full installation of Belenios, its libraries and its
server, in the `_run/usr` directory. See `demo/ocsigenserver.conf.in`
for an ocsigenserver configuration template, and the _Server
administrator's guide_ for more information on how to use it.

Documentation
-------------

You will need LaTeX to compile the specification.

On Debian-based systems, you can install the dependencies needed to
compile the documentation with:

    sudo apt install texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra lmodern texlive-science

Once all the dependencies have been installed, the documentation can
be compiled with:

    make doc

Compiling on Windows using Cygwin
---------------------------------

Windows is not yet a fully supported platform, but you can compile at
least the command-line tool on Windows + 32-bit
[Cygwin](http://cygwin.com/index.html). You might need the following
packages:

 * curl
 * dos2unix
 * flexdll
 * gcc-core
 * gcc-g++
 * git
 * gmp
 * libgmp-devel
 * libncursesw-devel
 * libpcre-devel
 * libsqlite3-devel
 * m4
 * make
 * ocaml
 * ocaml-base
 * ocaml-camlp4
 * ocaml-compiler-libs
 * openssh
 * patch
 * pkg-config
 * zlib-devel

With these packages installed, you should be able to install OPAM by
following its [installation instructions from
sources](http://opam.ocaml.org/doc/Install.html#FromSources).
Once OPAM is installed, follow the instructions in the _Command-line
tool_ section above.

Troubleshooting
---------------

### Bootstrap fails if dune is already installed

The script `opam-bootstrap.sh` fails when a not suitable version of
dune is already installed in your `$PATH`. This is due to [a bug in
opam](https://github.com/ocaml/opam/issues/3987). If you face this
issue, either uninstall dune before running `opam-bootstrap.sh`, or
manage to get opam running by other means, and directly use it to
install the dependencies of Belenios.

### Missing sources

The instructions outlined in this document and in the
`opam-bootstrap.sh` script imply downloading files from third-party
servers. Sometimes, these servers can be down. For example, you can
get:

    =-=-= Installing ocamlnet.3.7.3 =-=-=
    ocamlnet.3.7.3 Downloading http://download.camlcity.org/download/ocamlnet-3.7.3.tar.gz
    [ERROR] http://download.camlcity.org/download/ocamlnet-3.7.3.tar.gz is not available

    ===== ERROR while installing ocamlnet.3.7.3 =====
    Could not get the source for ocamlnet.3.7.3.

This can be worked around with the following steps:

 * source the generated `env.sh` file (you must adapt it if you use an
   incompatible shell such as tcsh);
 * download the file from an alternate source (for example
   [Debian source packages](http://www.debian.org/distrib/packages));
 * run `opam pin <package-name> <path-to-file-download-above>` (in the
   example above, `<package-name>` would be `ocamlnet`);
 * resume the installation by running again the `opam install` command
   found in `opam-bootstrap.sh`;
 * follow the instructions given at the end of `opam-bootstrap.sh`.

### Errors while compiling ocsigenserver

If ocsigenserver fails to install because of a SSL-related error:

 * edit `opam-bootstrap.sh` by adding ` ssl=0.5.2` to the `opam
   install` call;
 * run `./opam-bootstrap.sh`.

### Errors while compiling Belenios itself

If you succeeded installing all dependencies, but you get errors while
compiling Belenios, maybe you installed an incompatible version of a
dependency. The `opam-bootstrap.sh` script is tuned to install only
compatible versions; you can have a look at it to get these versions.
