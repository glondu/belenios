ifeq "$(BELENIOS_DEBUG)" "1"
    BUNDLE := bundle.development.js
    VOTE_PAGE := vote.html
else
    BUNDLE := bundle.production.js
    VOTE_PAGE := vote.html
endif

ifeq "$(DESTDIR)" ""
	DESTDIR := ./build
endif

PUBLIC_INTERNAL_JS_FOLDER := $(DESTDIR)
INITIAL_TRANSLATIONS_FOLDER := translations
PUBLIC_TRANSLATIONS_FOLDER := $(DESTDIR)/translations

# The `DESTDIR` variable is used as in [https://www.gnu.org/prep/standards/html_node/DESTDIR.html]. Use value `../_run/usr/share/belenios-server` to install Belenios.
install: booth/bundle.js booth/*.css booth/$(VOTE_PAGE) booth/components/*.css
	mkdir -p $(PUBLIC_INTERNAL_JS_FOLDER)/booth
	cp -r booth/bundle.js booth/*.css $(PUBLIC_INTERNAL_JS_FOLDER)/booth/
	mkdir -p $(PUBLIC_INTERNAL_JS_FOLDER)/booth/components
	cp -r booth/components/*.css $(PUBLIC_INTERNAL_JS_FOLDER)/booth/components/
	cp -r booth/$(VOTE_PAGE) $(PUBLIC_INTERNAL_JS_FOLDER)/booth/vote.html
	mkdir -p $(PUBLIC_TRANSLATIONS_FOLDER)
	cp -r $(INITIAL_TRANSLATIONS_FOLDER)/* $(PUBLIC_TRANSLATIONS_FOLDER)/

booth/dist/bundle.production.js: package-lock.json booth/*.mjs booth/components/*.mjs
	( cd booth && npx webpack --config webpack.config.js --mode production ) && mv booth/dist/bundle.js $@

booth/dist/bundle.development.js: package-lock.json booth/*.mjs booth/components/*.mjs
	( cd booth && npx webpack --config webpack.config.js --mode development ) && mv booth/dist/bundle.js $@

booth/bundle.js: booth/dist/$(BUNDLE)
	cp $< $@

package-lock.json: package.json
	npm install

.PHONY: install clean

clean:
	rm -rf package-lock.json node_modules booth/dist booth/bundle.js
