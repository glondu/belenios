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
wget https://github.com/ocaml/opam/releases/download/2.0.0/opam-full-2.0.0.tar.gz

if which sha256sum >/dev/null; then
sha256sum --check <<EOF
9dad4fcb4f53878c9daa6285d8456ccc671e21bfa71544d1f926fb8a63bfed25  opam-full-2.0.0.tar.gz
EOF
else
    echo "WARNING: sha256sum was not found, checking tarballs is impossible!"
fi

export PATH="$BELENIOS_SYSROOT/bootstrap/bin:$PATH"

echo
echo "=-=-= Compilation and installation of OPAM =-=-="
echo
cd "$BELENIOS_SYSROOT/bootstrap/src"
tar -xzf opam-full-2.0.0.tar.gz
cd opam-full-2.0.0
make cold CONFIGURE_ARGS="--prefix $BELENIOS_SYSROOT/bootstrap"
make cold-install LIBINSTALL_DIR="$BELENIOS_SYSROOT/bootstrap/lib/ocaml"

echo
echo "=-=-= Generation of env.sh =-=-="
echo
cat > $BELENIOS_SRC/env.sh <<EOF
PATH="$BELENIOS_SYSROOT/bootstrap/bin:\$PATH"; export PATH;
OPAMROOT=$OPAMROOT; export OPAMROOT;
eval \$(opam env)
EOF

echo
echo "=-=-= Initialization of OPAM root =-=-="
echo
opam init --bare --no-setup
opam switch create 4.06.1
eval $(opam env)

echo
echo "=-=-= Installation of Belenios build-dependencies =-=-="
echo
opam install --yes dune=1.6.3 atdgen zarith cryptokit uuidm calendar cmdliner sqlite3 ssl eliom=6.3.0 csv

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
