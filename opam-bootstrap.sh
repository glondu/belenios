#!/bin/sh

set -e

# Check that OCamlDuce is not installed
if which ocamlduce >/dev/null; then
    echo "Please uninstall OCamlDuce first, or remove it from your PATH."
    exit 1
fi

export OPAMROOT="${OPAMROOT:-$HOME/.opam}"

if [ -e "$OPAMROOT" ]; then
    echo "$OPAMROOT already exists."
    echo "Please set OPAMROOT to a non-existent directory first."
    exit 1
fi

mkdir -p "$OPAMROOT/bootstrap/src"

# Download and check tarballs
cd "$OPAMROOT/bootstrap/src"
wget http://caml.inria.fr/pub/distrib/ocaml-4.01/ocaml-4.01.0.tar.gz
wget http://www.ocamlpro.com/pub/opam-full-1.1.0.tar.gz
sha256sum --check <<EOF
ea1751deff454f5c738d10d8a0ad135afee0852d391cf95766b726c0faf7cfdb  ocaml-4.01.0.tar.gz
c0ab5e85b6cd26e533a40686e08aea173387d15bead817026f5b08f264642583  opam-full-1.1.0.tar.gz
EOF

# Install OCaml from source
cd "$OPAMROOT/bootstrap/src"
tar -xzf ocaml-4.01.0.tar.gz
cd ocaml-4.01.0
./configure -prefix "$OPAMROOT/bootstrap"
make world
make opt
make opt.opt
make install
export PATH="$OPAMROOT/bootstrap/bin:$PATH"

# Install OPAM from source
cd "$OPAMROOT/bootstrap/src"
tar -xzf opam-full-1.1.0.tar.gz
cd opam-full-1.1.0
./configure -prefix "$OPAMROOT/bootstrap"
make
make install

# Installation of Belenios build-dependencies
opam init --no-setup
eval `opam config env`
opam install --yes atdgen zarith cryptokit uuidm calendar eliom csv

echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo
echo "Belenios build-dependencies have been successfully compiled and installed"
echo "to $OPAMROOT. The directory"
echo "  $OPAMROOT/bootstrap/src"
echo "can be safely removed now."
echo
echo "Next, you need to run the following commands or add them to your ~/.bashrc"
echo "or equivalent:"
echo "  export PATH=$OPAMROOT/bootstrap/bin:\$PATH"
echo "  export OPAMROOT=$OPAMROOT"
echo "  eval \`opam config env\`"
echo
echo "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
echo