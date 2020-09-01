#!/bin/sh

set -e

echo "\"use strict\";(function(g){var belenios={};"
sed "s/(function(){return this}())/(g)/g" "$1"
echo
echo "}(this));"
