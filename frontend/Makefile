ifeq "$(BELENIOS_DEBUG)" "1"
    JAVASCRIPT_MODE := development
    JAVASCRIPT_EXTENSION := 
    VOTE_PAGE := vote_development.html
else
    JAVASCRIPT_MODE := production
    JAVASCRIPT_EXTENSION := .min
    VOTE_PAGE := vote.html
endif

ifeq "$(DESTDIR)" ""
	DESTDIR := ./build
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
PUBLIC_EXTERNAL_JS_FOLDER := $(DESTDIR)/node_modules
PUBLIC_INTERNAL_JS_FOLDER := $(DESTDIR)
INITIAL_TRANSLATIONS_FOLDER := translations
PUBLIC_TRANSLATIONS_FOLDER := $(DESTDIR)/translations

# The `DESTDIR` variable is used as in [https://www.gnu.org/prep/standards/html_node/DESTDIR.html]. Use value `../_run/usr/share/belenios-server` to install Belenios.
install: $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_DOM_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_HTTP_BACKEND_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_I18NEXT_JS_URL) booth/*.mjs booth/*.js booth/*.css booth/$(VOTE_PAGE) booth/components
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
	mkdir -p $(PUBLIC_INTERNAL_JS_FOLDER)/booth
	cp -r booth/*.mjs booth/*.js booth/*.css booth/components $(PUBLIC_INTERNAL_JS_FOLDER)/booth/
	cp -r booth/$(VOTE_PAGE) $(PUBLIC_INTERNAL_JS_FOLDER)/booth/vote.html
	mkdir -p $(PUBLIC_TRANSLATIONS_FOLDER)
	cp -r $(INITIAL_TRANSLATIONS_FOLDER)/* $(PUBLIC_TRANSLATIONS_FOLDER)/

$(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_DOM_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(I18NEXT_HTTP_BACKEND_JS_URL) $(INITIAL_EXTERNAL_JS_FOLDER)/$(REACT_I18NEXT_JS_URL): package-lock.json

package-lock.json: package.json
	npm install

.PHONY: install