#!/bin/sh

make build-debug-server

for LANG in $(cat po/voter/LINGUAS); do
    if [ ! -f frontend/translations/$LANG.json ]; then
        echo '{}' > frontend/translations/$LANG.json
    fi
    dune exec --build-dir=_build-debug -- src/scripts/translate_stubs/main.exe frontend/translations/en.json _build-debug/default/po/voter/$LANG.mo < frontend/translations/$LANG.json | jq --indent 4 | sponge frontend/translations/$LANG.json
done
