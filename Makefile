DUNE_DEBUG_ARGS := --build-dir=_build-debug

ifeq "$(BELENIOS_DEBUG)" "1"
    JAVASCRIPT_MODE := development
    JAVASCRIPT_EXTENSION := 
else
    JAVASCRIPT_MODE := production
    JAVASCRIPT_EXTENSION := .min
endif

REACT_JS_FOLDER := react/umd
REACT_JS_FILENAME := react.$(JAVASCRIPT_MODE)$(JAVASCRIPT_EXTENSION).js
REACT_JS_URL := $(REACT_JS_FOLDER)/$(REACT_JS_FILENAME)
REACT_DOM_JS_FOLDER := react-dom/umd
REACT_DOM_JS_FILENAME := react-dom.$(JAVASCRIPT_MODE)$(JAVASCRIPT_EXTENSION).js
REACT_DOM_JS_URL := $(REACT_DOM_JS_FOLDER)/$(REACT_DOM_JS_FILENAME)
I18NEXT_JS_FOLDER := i18next/dist/umd
I18NEXT_JS_FILENAME := i18next$(JAVASCRIPT_EXTENSION).js
I18NEXT_JS_URL := $(I18NEXT_JS_FOLDER)/$(I18NEXT_JS_FILENAME)
I18NEXT_HTTP_BACKEND_JS_FOLDER := i18next-http-backend
I18NEXT_HTTP_BACKEND_JS_FILENAME := i18nextHttpBackend$(JAVASCRIPT_EXTENSION).js
I18NEXT_HTTP_BACKEND_JS_URL := $(I18NEXT_HTTP_BACKEND_JS_FOLDER)/$(I18NEXT_HTTP_BACKEND_JS_FILENAME)
REACT_I18NEXT_JS_FOLDER := react-i18next/dist/umd
REACT_I18NEXT_JS_FILENAME := react-i18next$(JAVASCRIPT_EXTENSION).js
REACT_I18NEXT_JS_URL := $(REACT_I18NEXT_JS_FOLDER)/$(REACT_I18NEXT_JS_FILENAME)
INITIAL_EXTERNAL_JS_FOLDER := node_modules
PUBLIC_EXTERNAL_JS_FOLDER := _run/usr/share/belenios-server/node_modules
PUBLIC_INTERNAL_JS_FOLDER := _run/usr/share/belenios-server/internal_modules
INITIAL_TRANSLATIONS_FOLDER := frontend_translations
PUBLIC_TRANSLATIONS_FOLDER := _run/usr/share/belenios-server/translations

minimal:
	dune build -p belenios-platform,belenios-platform-native,belenios,belenios-tool

custom-javascript: $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_DOM_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_HTTP_BACKEND_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_I18NEXT_JS_URL) src/booth/js/i18n_init.js src/booth/js/shortcuts.js src/booth/js/like_button.js src/booth/js/translated_like_button.js src/booth/js/app.js
	mkdir -p $(PUBLIC_EXTERNAL_JS_FOLDER)/$(REACT_JS_FOLDER)
	cp -r $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_JS_URL) $(PUBLIC_EXTERNAL_JS_FOLDER)/$(REACT_JS_URL)
	mkdir -p $(PUBLIC_EXTERNAL_JS_FOLDER)/$(REACT_DOM_JS_FOLDER)
	cp -r $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_DOM_JS_URL) $(PUBLIC_EXTERNAL_JS_FOLDER)/$(REACT_DOM_JS_URL)
	mkdir -p $(PUBLIC_EXTERNAL_JS_FOLDER)/$(I18NEXT_JS_FOLDER)
	cp -r $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_JS_URL) $(PUBLIC_EXTERNAL_JS_FOLDER)/$(I18NEXT_JS_URL)
	mkdir -p $(PUBLIC_EXTERNAL_JS_FOLDER)/$(I18NEXT_HTTP_BACKEND_JS_FOLDER)
	cp -r $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_HTTP_BACKEND_JS_URL) $(PUBLIC_EXTERNAL_JS_FOLDER)/$(I18NEXT_HTTP_BACKEND_JS_URL)
	mkdir -p $(PUBLIC_EXTERNAL_JS_FOLDER)/$(REACT_I18NEXT_JS_FOLDER)
	cp -r $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_I18NEXT_JS_URL) $(PUBLIC_EXTERNAL_JS_FOLDER)/$(REACT_I18NEXT_JS_URL)
	mkdir -p $(PUBLIC_INTERNAL_JS_FOLDER)/booth/js
	cp -r src/booth/js/i18n_init.js src/booth/js/shortcuts.js src/booth/js/like_button.js src/booth/js/translated_like_button.js src/booth/js/app.js $(PUBLIC_INTERNAL_JS_FOLDER)/booth/js/
	cp -r $(INITIAL_TRANSLATIONS_FOLDER) $(PUBLIC_TRANSLATIONS_FOLDER)

build-debug-server:
	BELENIOS_DEBUG=1 dune build $(DUNE_DEBUG_ARGS)
	rm -rf _run/usr
	dune install $(DUNE_DEBUG_ARGS) --destdir=_run --prefix=/usr 2>/dev/null
	BELENIOS_DEBUG=1 $(MAKE) custom-javascript
	git archive --prefix=belenios-debug/ HEAD | gzip -9n > _run/usr/share/belenios-server/belenios.tar.gz

build-release-server:
	$(MAKE) clean
	BELENIOS_DEBUG= dune build --release
	rm -rf _run/usr
	dune install --destdir=_run --prefix=/usr 2>/dev/null
	BELENIOS_DEBUG= $(MAKE) custom-javascript
	git archive --prefix="belenios-$(shell git describe --tags)/" HEAD | gzip -9n > _run/usr/share/belenios-server/belenios.tar.gz

build-debug-tool:
	BELENIOS_DEBUG=1 dune build $(DUNE_DEBUG_ARGS) -p belenios-platform,belenios-platform-native,belenios,belenios-tool
	rm -rf _run/tool-debug
	dune install $(DUNE_DEBUG_ARGS) --destdir=_run/tool-debug --prefix=/ belenios-platform belenios-platform-native belenios belenios-tool 2>/dev/null

check:
	$(MAKE) build-debug-tool
	$(MAKE) -C tests/tool check

clean:
	dune clean
	dune clean $(DUNE_DEBUG_ARGS)
	$(MAKE) -C po clean
	$(MAKE) -C tests/tool clean

.PHONY: doc
doc:
	$(MAKE) doc/specification.pdf

doc/specification.pdf: doc/specification.tex
	cd doc && for u in 1 2 3; do pdflatex specification.tex; done

release:
	@if [ `git status --porcelain | grep -v '^?? ' | wc -l ` -eq 0 ]; then \
	  COMMIT_ID=`git describe --tags`; \
	  VERSION=`cat VERSION`; \
	  mkdir -p _releases; \
	  if [ "$$(printf $$COMMIT_ID | head -c$$(printf $$VERSION | wc -c))" = "$$VERSION" ]; then \
	    git archive --prefix=belenios-$$COMMIT_ID/ $$COMMIT_ID | gzip -9n > _releases/belenios-$$COMMIT_ID.tar.gz; \
	  else \
	    echo "VERSION is not up-to-date!"; exit 1; \
	  fi; \
	else \
	  echo "The tree is not clean!"; exit 1; \
	fi
