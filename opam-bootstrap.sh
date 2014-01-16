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
wget http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.gz
wget http://www.ocamlpro.com/pub/opam-full-1.1.0.tar.gz

if which sha256sum >/dev/null; then
sha256sum --check <<EOF
ea1751deff454f5c738d10d8a0ad135afee0852d391cf95766b726c0faf7cfdb  ocaml-4.01.0.tar.gz
c0ab5e85b6cd26e533a40686e08aea173387d15bead817026f5b08f264642583  opam-full-1.1.0.tar.gz
EOF
else
    echo "WARNING: sha256sum was not found, checking tarballs is impossible!"
fi

echo
echo "=-=-= Compilation and installation of OCaml =-=-="
echo
cd "$BELENIOS_SYSROOT/bootstrap/src"
tar -xzf ocaml-4.01.0.tar.gz
cd ocaml-4.01.0
./configure -prefix "$BELENIOS_SYSROOT/bootstrap"
make world
make opt
make opt.opt
make install
export PATH="$BELENIOS_SYSROOT/bootstrap/bin:$PATH"

echo
echo "=-=-= Compilation and installation of OPAM =-=-="
echo
cd "$BELENIOS_SYSROOT/bootstrap/src"
tar -xzf opam-full-1.1.0.tar.gz
cd opam-full-1.1.0
./configure -prefix "$BELENIOS_SYSROOT/bootstrap"
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
eval `opam config env --sh`

echo
echo "=-=-= Installation of Belenios build-dependencies =-=-="
echo
opam install --yes atdgen zarith cryptokit uuidm calendar eliom csv

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
