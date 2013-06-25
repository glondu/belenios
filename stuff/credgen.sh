#!/bin/sh
set -e
ocamlbuild -quiet src/bin/credgen.native
exec _build/src/bin/credgen.native "$@"
