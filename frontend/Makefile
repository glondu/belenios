CSS_DEPS := $(wildcard ../ext/css/*.css ../src/web/static/*.css booth/*.css booth/components/*.css)

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

PUBLIC_INTERNAL_JS_FOLDER := $(DESTDIR)/frontend
INITIAL_TRANSLATIONS_FOLDER := translations
PUBLIC_TRANSLATIONS_FOLDER := $(DESTDIR)/frontend/translations

# The `DESTDIR` variable is used as in [https://www.gnu.org/prep/standards/html_node/DESTDIR.html]. Use value `../_run/usr/share/belenios-server` to install Belenios.
install: site.bundle.css booth/bundle.js booth/app.bundle.css booth/$(VOTE_PAGE)
	mkdir -p $(PUBLIC_INTERNAL_JS_FOLDER)/booth
	cp *.bundle.css $(DESTDIR)
	cp -r booth/bundle.js booth/app.bundle.css $(PUBLIC_INTERNAL_JS_FOLDER)/booth/
	cp -r booth/$(VOTE_PAGE) $(PUBLIC_INTERNAL_JS_FOLDER)/booth/vote.html
	mkdir -p $(PUBLIC_TRANSLATIONS_FOLDER)
	cp -r $(INITIAL_TRANSLATIONS_FOLDER)/* $(PUBLIC_TRANSLATIONS_FOLDER)/

format:
	npx prettier ./booth --write

booth/dist/bundle.production.js: package-lock.json booth/*.js booth/components/*.js
	( cd booth && npx webpack --config webpack.config.js --mode production ) && mv booth/dist/bundle.js $@

booth/dist/bundle.development.js: package-lock.json booth/*.js booth/components/*.js
	( cd booth && npx webpack --config webpack.config.js --mode development ) && mv booth/dist/bundle.js $@

booth/bundle.js: booth/dist/$(BUNDLE)
	cp $< $@

site.bundle.css: package-lock.json $(CSS_DEPS)
	NODE_PATH=$(CURDIR)/node_modules nodejs ./bundle-css.js ../src/web/static/responsive_site.css > $@

booth/app.bundle.css: package-lock.json $(CSS_DEPS)
	NODE_PATH=$(CURDIR)/node_modules nodejs ./bundle-css.js booth/app.css > $@

package-lock.json: package.json
	npm install

.PHONY: install clean

clean:
	rm -rf package-lock.json node_modules booth/dist booth/bundle.js *.bundle.css booth/*.bundle.css
