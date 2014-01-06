Belenios compilation instructions
=================================

The easiest and most portable way to compile Belenios from source is
to use [OPAM](http://opam.ocamlpro.com/), which is a package manager
for OCaml projects. You can find installation instructions
[there](http://opam.ocaml.org/doc/Quick_Install.html).

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

If you use [OPAM](http://opam.ocamlpro.com/), these dependencies can
be installed with the following command:

    opam install atdgen zarith cryptokit uuidm calendar

On [Debian](http://www.debian.org)-based systems, they can be
installed with the following command:

    sudo apt-get install libatdgen-ocaml-dev libzarith-ocaml-dev libcryptokit-ocaml-dev libuuidm-ocaml-dev libcalendar-ocaml-dev

On [Ubuntu](http://www.ubuntu.com), you need to enable the "Universe"
repository. This can be done with the following command-line (run it
before running the command above):

    sudo add-apt-repository "deb http://archive.ubuntu.com/ubuntu $(lsb_release -sc) universe"

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

On Debian-based systems, you can install it with:

    sudo apt-get install ocsigenserver eliom libcsv-ocaml-dev

But keep in mind that Belenios needs a very recent version of these
packages (in particular, eliom version 3 which is not in Debian stable
at the time of writing), so the ones available on your system might be
too old. If you are in this case, and want to run the server, we
advise you to use OPAM.

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
