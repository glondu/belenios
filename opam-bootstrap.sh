#!/bin/sh

set -e

BELENIOS_SRC="${BELENIOS_SRC:-$PWD}"

# Check that OCamlDuce is not installed
if which ocamlduce >/dev/null; then
    echo "Please uninstall OCamlDuce first, or remove it from your PATH."
    exit 1
fi

echo
echo "=-=-= Download and check tarballs =-=-="
echo

# Look for wget or curl
if which wget >/dev/null; then
    echo "wget was found and will be used"
elif which curl >/dev/null; then
    wget () { curl "$1" > "${1##*/}"; }
    echo "curl was found and will be used"
fi

export BELENIOS_SYSROOT="${BELENIOS_SYSROOT:-$HOME/.belenios}"
export OPAMROOT="$BELENIOS_SYSROOT/opam"

if [ -e "$BELENIOS_SYSROOT" ]; then
    echo "$BELENIOS_SYSROOT already exists."
    echo "Please remove it or set BELENIOS_SYSROOT to a non-existent directory first."
    exit 1
fi

mkdir -p "$BELENIOS_SYSROOT/bootstrap/src"

cd "$BELENIOS_SYSROOT/bootstrap/src"
wget http://caml.inria.fr/pub/distrib/ocaml-4.02/ocaml-4.02.3.tar.gz
wget https://github.com/ocaml/opam/releases/download/1.2.2/opam-full-1.2.2.tar.gz

if which sha256sum >/dev/null; then
sha256sum --check <<EOF
928fb5f64f4e141980ba567ff57b62d8dc7b951b58be9590ffb1be2172887a72  ocaml-4.02.3.tar.gz
15e617179251041f4bf3910257bbb8398db987d863dd3cfc288bdd958de58f00  opam-full-1.2.2.tar.gz
EOF
else
    echo "WARNING: sha256sum was not found, checking tarballs is impossible!"
fi

echo
echo "=-=-= Compilation and installation of OCaml =-=-="
echo
cd "$BELENIOS_SYSROOT/bootstrap/src"
tar -xzf ocaml-4.02.3.tar.gz
cd ocaml-4.02.3
./configure -prefix "$BELENIOS_SYSROOT/bootstrap"
make world
if ! grep -q ARCH=none config/Makefile; then
  make opt
  make opt.opt
fi
make install
export PATH="$BELENIOS_SYSROOT/bootstrap/bin:$PATH"

echo
echo "=-=-= Compilation and installation of OPAM =-=-="
echo
cd "$BELENIOS_SYSROOT/bootstrap/src"
tar -xzf opam-full-1.2.2.tar.gz
cd opam-full-1.2.2
./configure -prefix "$BELENIOS_SYSROOT/bootstrap"
make lib-ext
make
make install

echo
echo "=-=-= Generation of env.sh =-=-="
echo
cat > $BELENIOS_SRC/env.sh <<EOF
PATH="$BELENIOS_SYSROOT/bootstrap/bin:\$PATH"; export PATH;
OPAMROOT=$OPAMROOT; export OPAMROOT;
eval \`opam config env\`
EOF

echo
echo "=-=-= Initialization of OPAM root =-=-="
echo
opam init --no-setup
eval `opam config env`
opam switch 4.02.3
eval `opam config env`

echo
echo "=-=-= Installation of Belenios build-dependencies =-=-="
echo
opam install --yes atdgen zarith cryptokit uuidm calendar cmdliner sqlite3 eliom=4.2.0 csv

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "Belenios build-dependencies have been successfully compiled and installed"
echo "to $BELENIOS_SYSROOT. The directory"
echo "  $BELENIOS_SYSROOT/bootstrap/src"
echo "can be safely removed now."
echo
echo "Next, you need to run the following commands or add them to your ~/.bashrc"
echo "or equivalent:"
echo "  source $BELENIOS_SRC/env.sh"
echo "Note that if you use a Bourne-incompatible shell (e.g. tcsh), you'll have"
echo "to adapt env.sh to your shell."
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
