#!/bin/sh
ocamlbuild src/bin/credgen.native
exec _build/src/bin/credgen.native "$@"
