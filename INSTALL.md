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
 * [aspcud](http://www.cs.uni-potsdam.de/wv/aspcud/) (optional)
 * [ncurses](http://invisible-island.net/ncurses/)
 * [uuidgen](https://www.kernel.org/pub/linux/utils/util-linux/)
 * [GD-SecurityImage](https://metacpan.org/release/GD-SecurityImage)
 * [cracklib](http://sourceforge.net/projects/cracklib)

These libraries and tools are pretty common, and might be directly part
of your operating system. On [Debian](http://www.debian.org/) and its
derivatives, they can be installed with the following command:

    sudo apt install bubblewrap build-essential libgmp-dev libpcre3-dev pkg-config m4 libssl-dev libsqlite3-dev wget ca-certificates zip unzip aspcud libncurses-dev uuid-runtime zlib1g-dev libgd-securityimage-perl cracklib-runtime

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

    make all

and you can skip the next two sections and go directly to the
_Documentation_ section.

You can also compile a debug version by using the `BELENIOS_DEBUG`
environment variable:

    BELENIOS_DEBUG=1 make all

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

 * [OCaml](http://caml.inria.fr/)
 * [Findlib](http://projects.camlcity.org/projects/findlib.html)
 * [Zarith](https://forge.ocamlcore.org/projects/zarith/)
 * [Uuidm](http://erratique.ch/software/uuidm)
 * [Cryptokit](https://forge.ocamlcore.org/projects/cryptokit/)
 * [Atdgen](http://mjambon.com/atdgen)
 * [Yojson](http://mjambon.com/yojson.html)
 * [Cmdliner](http://erratique.ch/software/cmdliner)

With OPAM, these dependencies can be installed with the following
command:

    opam install atdgen zarith cryptokit uuidm cmdliner

Once all the dependencies have been installed, the command-line tool
can be compiled with:

    make

It produces a single executable, `belenios-tool`, in the `_build/`
directory. You can install it in your `PATH` (which we will assume in
the guides), or refer to it with a full path.

Web server
----------

The web server has the following additional dependencies:

 * [Calendar](http://calendar.forge.ocamlcore.org/)
 * [Eliom](http://ocsigen.org/eliom/)
 * [Csv](https://forge.ocamlcore.org/projects/csv/)

With OPAM, you can install them with:

    opam install calendar eliom csv

Once all the dependencies have been installed, the Eliom module can be
compiled with:

    make all

It will produce a single Eliom module, `server.cma`, in the
`_build/src/web` directory. See `demo/ocsigenserver.conf.in` for an
ocsigenserver configuration template, and the _Server administrator's
guide_ for more information on how to use it.

Documentation
-------------

To generate HTML files from `.md` ones, you will need:

 * [Markdown](http://daringfireball.net/projects/markdown/)

Additionnaly, you will need LaTeX to compile the specification.

On Debian-based systems, you can install the dependencies needed to
compile the documentation with:

    sudo apt install markdown texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra lmodern

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

An alternative could be to install aspcud before running
`opam-bootstrap.sh`.

### Errors while compiling Belenios itself

If you succeeded installing all dependencies, but you get errors while
compiling Belenios, maybe you installed an incompatible version of a
dependency. The `opam-bootstrap.sh` script is tuned to install only
compatible versions; you can have a look at it to get these versions.
