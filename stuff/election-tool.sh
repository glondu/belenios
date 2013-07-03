#!/bin/sh
set -e
ocamlbuild -quiet src/bin/election-tool.native
exec _build/src/bin/election-tool.native "$@"
