Belenios compilation instructions
=================================

The easy way
------------

Belenios is written in OCaml and has some dependencies towards
third-party OCaml libraries. The easiest and most portable way to
compile Belenios from source is to use
[OPAM](http://opam.ocamlpro.com/), which is a package manager for
OCaml projects.

The requirements are a POSIX system, a C compiler,
[GMP](http://gmplib.org/) and [PCRE](http://www.pcre.org/). These
libraries are quite common, and probably available from your system
package manager if you have one.

In Belenios sources, there is a `opam-bootstrap.sh` shell script that
downloads and installs OCaml, OPAM and all the dependencies of
Belenios into a single directory. You can chose the directory by
setting the `OPAMROOT` environment variable, or it will take `~/.opam`
by default. Just run:

    ./opam-boostap.sh

On a modern desktop system, this needs approximately 10 minutes and 1
gigabyte of disk space.

If everything goes successfully, follow the given instructions to
update your shell environment, then run:

    make all

and you can skip the next two sections.

Command-line tool
-----------------

To compile the command-line tool, you will need:

 * [OCaml](http://caml.inria.fr/)
 * [Findlib](http://projects.camlcity.org/projects/findlib.html)
 * [Zarith](https://forge.ocamlcore.org/projects/zarith/)
 * [Calendar](http://calendar.forge.ocamlcore.org/)
 * [Uuidm](http://erratique.ch/software/uuidm)
 * [Cryptokit](https://forge.ocamlcore.org/projects/cryptokit/)
 * [Atdgen](http://mjambon.com/atdgen)
 * [Yojson](http://mjambon.com/yojson.html)

With OPAM, these dependencies can be installed with the following
command:

    opam install atdgen zarith cryptokit uuidm calendar

Once all the dependencies have been installed, the command-line tool
can be compiled with:

    make

It produces a single executable, `belenios-tool`, in the `_build/`
directory. You can install it in your `PATH` (which we will assume in
the guides), or refer to it with a full path.

Web server
----------

The web server has the following additional dependencies:

 * [Eliom](http://ocsigen.org/eliom/), version 3
 * [Csv](https://forge.ocamlcore.org/projects/csv/)

With OPAM, you can install them with:

    opam install eliom csv

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

    sudo apt-get install markdown texlive

Once all the dependencies have been installed, the documentation can
be compiled with:

    make doc

Compilation using only official Debian packages
-----------------------------------------------

At the time of writing (07 Jan 2014), you need the development version
of Debian (or Ubuntu) to be able to compile Belenios using only
official Debian packages. On Ubuntu, you need to enable the "Universe"
repository. Instead of using OPAM, the dependencies of Belenios can
then be installed with:

    sudo apt-get install libatdgen-ocaml-dev libzarith-ocaml-dev libcryptokit-ocaml-dev libuuidm-ocaml-dev libcalendar-ocaml-dev
    sudo apt-get install ocsigenserver eliom libcsv-ocaml-dev

Troubleshooting
---------------

OCamlDuce is an optional transitive dependency of Belenios, but
Belenios does not use it. If OCamlDuce was installed outside of OPAM
(e.g. via your system package manager), you may face issues. You can
work around them by uninstalling OCamlDuce and restarting the
installation procedure.
