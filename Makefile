BELENIOS_BUILD := $(shell ./src/platform/version/get_build.sh)
DUNE_DEBUG_ARGS :=
BELENIOS_SRC := _run/usr/share/belenios-server/belenios.tar.gz
DOCUMENTATION := doc/specification.pdf doc/instructions-en.html doc/instructions-fr.html

export BELENIOS_BUILD

minimal:
	dune build -p belenios-platform,belenios-platform-native,belenios-lib,belenios-tool

$(BELENIOS_SRC):
	contrib/make-tarball.sh $@

build-debug-server:
	@echo "Building Belenios server $(BELENIOS_BUILD) in debug mode..."
	BELENIOS_DEBUG=1 dune build $(DUNE_DEBUG_ARGS)
	BELENIOS_DEBUG=1 dune exec $(DUNE_DEBUG_ARGS) -- \
	  src/scripts/checki18next/checki18next.exe --dir frontend/translations \
	  < src/scripts/checki18next/reference.json
	rm -rf _run/usr
	dune install $(DUNE_DEBUG_ARGS) --display=quiet --destdir=_run --prefix=/usr
	BELENIOS_DEBUG=1 $(MAKE) DESTDIR=../_run/usr/share/belenios-server -C frontend
	rm -f $(BELENIOS_SRC) && $(MAKE) $(BELENIOS_SRC)

build-release-server:
	$(MAKE) clean
	@echo "Building Belenios server $(BELENIOS_BUILD) in release mode..."
	BELENIOS_DEBUG= dune build --release
	BELENIOS_DEBUG= dune exec $(DUNE_DEBUG_ARGS) -- \
	  src/scripts/checki18next/checki18next.exe --dir frontend/translations \
	  < src/scripts/checki18next/reference.json
	rm -rf _run/usr
	dune install --display=quiet --destdir=_run --prefix=/usr
	BELENIOS_DEBUG= $(MAKE) DESTDIR=../_run/usr/share/belenios-server -C frontend
	rm -f $(BELENIOS_SRC) && $(MAKE) $(BELENIOS_SRC)

build-i18next-reference:
	BELENIOS_DEBUG=1 dune exec $(DUNE_DEBUG_ARGS) -- \
	  src/scripts/checki18next/checki18next.exe --dir frontend/translations --make-reference \
	  < frontend/translations/en.json > src/scripts/checki18next/reference.json

build-debug-tool:
	BELENIOS_DEBUG=1 dune build $(DUNE_DEBUG_ARGS) -p belenios-platform,belenios-platform-native,belenios-lib,belenios-tool
	rm -rf _run/tool-debug
	dune install $(DUNE_DEBUG_ARGS) --display=quiet --destdir=_run/tool-debug --prefix=/ belenios-platform belenios-platform-native belenios-lib belenios-tool

check:
	$(MAKE) build-debug-tool
	$(MAKE) -C tests/tool check

clean:
	dune clean
	dune clean $(DUNE_DEBUG_ARGS)
	$(MAKE) -C po clean
	$(MAKE) -C tests/tool clean
	$(MAKE) -C frontend clean
	rm -rf geckodriver.log venv .hypothesis _tests
	find tests -name __pycache__ -print0 | xargs -0 rm -rf

.PHONY: doc
doc:
	$(MAKE) $(DOCUMENTATION)

install-doc: doc
ifeq ($(DESTDIR),)
	$(error Please define DESTDIR)
else
	cp $(DOCUMENTATION) $(DESTDIR)
endif

doc/specification.pdf: doc/specification.tex doc/spec_version.tex
	cd doc && rubber --pdf specification.tex

%.html: %.md
	pandoc -o $@ $<

release:
	@if [ `git status --porcelain | grep -v '^?? ' | wc -l ` -eq 0 ]; then \
	  VERSION=`cat VERSION`; \
	  mkdir -p _releases; \
	  if [ "$$(printf $(BELENIOS_BUILD) | head -c$$(printf $$VERSION | wc -c))" = "$$VERSION" ]; then \
	    git archive --prefix=belenios-$(BELENIOS_BUILD)/ $(BELENIOS_BUILD) | gzip -9n > _releases/belenios-$(BELENIOS_BUILD).tar.gz; \
	  else \
	    echo "VERSION is not up-to-date!"; exit 1; \
	  fi; \
	else \
	  echo "The tree is not clean!"; exit 1; \
	fi
