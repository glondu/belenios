#!/bin/sh

set -e

OPAM_VERSION=2.1.5
BELENIOS_SRC="${BELENIOS_SRC:-$PWD}"

export BELENIOS_SYSROOT="${BELENIOS_SYSROOT:-$HOME/.belenios}"
export OPAMROOT="$BELENIOS_SYSROOT/opam"
export XDG_CACHE_HOME="$BELENIOS_SYSROOT/cache"

if [ -e "$BELENIOS_SYSROOT" ]; then
    echo "$BELENIOS_SYSROOT already exists."
    echo "Please remove it or set BELENIOS_SYSROOT to a non-existent directory first."
    exit 1
fi

mkdir -p "$BELENIOS_SYSROOT"
cd "$BELENIOS_SYSROOT"

echo
echo "=-=-= Cloning OPAM repository =-=-="
echo
mkdir opam-repository
cd opam-repository
git init
git remote add origin https://github.com/ocaml/opam-repository.git
git fetch --depth=1 origin 0f0319b6fdf6c811c00ecf56bb035dc471f21090:opam
git checkout opam

if [ -z "$BELENIOS_USE_SYSTEM_OPAM" ]; then

    # Download and install opam

    # Check that Dune is not installed
    # cf. https://github.com/ocaml/opam/issues/3987
    if command -v dune >/dev/null; then
        echo "Please uninstall Dune first, or remove it from your PATH."
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

    mkdir -p "$BELENIOS_SYSROOT/bootstrap/src"

    cd "$BELENIOS_SYSROOT/bootstrap/src"
    wget https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-full-$OPAM_VERSION.tar.gz

    if which sha256sum >/dev/null; then
        sha256sum --check <<EOF
09f8d9e410b2f5723c2bfedbf7970e3b305f5017895fcd91759f05e753ddcea5  opam-full-$OPAM_VERSION.tar.gz
EOF
    else
        echo "WARNING: sha256sum was not found, checking tarballs is impossible!"
        exit 2
    fi

    export PATH="$BELENIOS_SYSROOT/bootstrap/bin:$PATH"

    echo
    echo "=-=-= Compilation and installation of OPAM =-=-="
    echo
    cd "$BELENIOS_SYSROOT/bootstrap/src"
    tar -xzf opam-full-$OPAM_VERSION.tar.gz
    cd opam-full-$OPAM_VERSION
    make cold CONFIGURE_ARGS="--prefix $BELENIOS_SYSROOT/bootstrap"
    make cold-install LIBINSTALL_DIR="$BELENIOS_SYSROOT/bootstrap/lib/ocaml"

    cat > $BELENIOS_SYSROOT/env.sh <<EOF
PATH="$BELENIOS_SYSROOT/bootstrap/bin:\$PATH"; export PATH;
EOF

fi

echo
echo "=-=-= Generation of env.sh =-=-="
echo
cat >> $BELENIOS_SYSROOT/env.sh <<EOF
OPAMROOT=$OPAMROOT; export OPAMROOT;
XDG_CACHE_HOME=$XDG_CACHE_HOME; export XDG_CACHE_HOME;
eval \$(opam env)
EOF
ln -sf $BELENIOS_SYSROOT/env.sh $BELENIOS_SRC/env.sh

echo
echo "=-=-= Initialization of OPAM root =-=-="
echo
opam init $BELENIOS_OPAM_INIT_ARGS --bare --no-setup -k git "$BELENIOS_SYSROOT/opam-repository"
opam switch create 4.13.1 ocaml-base-compiler.4.13.1
eval $(opam env)
opam repository add belenios-overlay $BELENIOS_SRC/ext/opam-overlay

echo
echo "=-=-= Installation of Belenios build-dependencies =-=-="
echo
opam install --yes base64 hex dune atdgen zarith cryptokit calendar cmdliner sqlite3 csv ocsipersist-sqlite eliom=10.0.0 gettext-camomile ocamlnet

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
