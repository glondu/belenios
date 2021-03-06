#!/bin/sh

set -e

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
    wget https://github.com/ocaml/opam/releases/download/2.0.8/opam-full-2.0.8.tar.gz
    wget https://github.com/mirage/ocaml-magic-mime/releases/download/v1.1.3/magic-mime-v1.1.3.tbz

    if which sha256sum >/dev/null; then
        sha256sum --check <<EOF
7b9d29233d9633ef50ba766df2e39112b15cd05c1c6fedf80bcb548debcdd9bd  opam-full-2.0.8.tar.gz
7fb36ce619ca479ac44ef923c3bf19eda4c98a4428dbf7f3f7c714b516d212f7  magic-mime-v1.1.3.tbz
EOF
    else
        echo "WARNING: sha256sum was not found, checking tarballs is impossible!"
    fi

    export PATH="$BELENIOS_SYSROOT/bootstrap/bin:$PATH"

    echo
    echo "=-=-= Compilation and installation of OPAM =-=-="
    echo
    cd "$BELENIOS_SYSROOT/bootstrap/src"
    tar -xzf opam-full-2.0.8.tar.gz
    cd opam-full-2.0.8
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
cd "$BELENIOS_SYSROOT"
git clone https://github.com/ocaml/opam-repository.git
cd opam-repository
git reset --hard 94a137c4585c442147ef4dd0f8c8c7756ebcdec8
opam init $BELENIOS_OPAM_INIT_ARGS --bare --no-setup -k git "$BELENIOS_SYSROOT/opam-repository"
opam switch create 4.11.2
eval $(opam env)

# FIXME: temporary, until one of:
#   https://github.com/ocsigen/ocsigenserver/issues/201
#   https://github.com/mirage/ocaml-magic-mime/issues/21
# is fixed
cd "$BELENIOS_SYSROOT/bootstrap/src"
tar -xf magic-mime-v1.1.3.tbz
cd magic-mime-v1.1.3
sed -i 's@application/javascript.*@application/javascript\tjs mjs@' mime.types
opam pin --yes magic-mime $PWD

echo
echo "=-=-= Installation of Belenios build-dependencies =-=-="
echo
opam install --yes dune atdgen zarith cryptokit calendar cmdliner sqlite3 csv eliom=8.4.8 gettext-camomile ocamlnet

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
