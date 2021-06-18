DUNE_DEBUG_ARGS := --build-dir=_build-debug

minimal:
	dune build -p belenios-platform,belenios-platform-native,belenios-lib,belenios-tool

build-debug-server:
	BELENIOS_DEBUG=1 dune build $(DUNE_DEBUG_ARGS)
	rm -rf _run/usr
	dune install $(DUNE_DEBUG_ARGS) --destdir=_run --prefix=/usr 2>/dev/null
	BELENIOS_DEBUG=1 $(MAKE) DESTDIR=../_run/usr/share/belenios-server/frontend -C frontend
	git archive --prefix=belenios-debug/ HEAD | gzip -9n > _run/usr/share/belenios-server/belenios.tar.gz

build-release-server:
	$(MAKE) clean
	BELENIOS_DEBUG= dune build --release
	rm -rf _run/usr
	dune install --destdir=_run --prefix=/usr 2>/dev/null
	BELENIOS_DEBUG= $(MAKE) DESTDIR=../_run/usr/share/belenios-server/frontend -C frontend
	git archive --prefix="belenios-$(shell git describe --tags)/" HEAD | gzip -9n > _run/usr/share/belenios-server/belenios.tar.gz

build-debug-tool:
	BELENIOS_DEBUG=1 dune build $(DUNE_DEBUG_ARGS) -p belenios-platform,belenios-platform-native,belenios-lib,belenios-tool
	rm -rf _run/tool-debug
	dune install $(DUNE_DEBUG_ARGS) --destdir=_run/tool-debug --prefix=/ belenios-platform belenios-platform-native belenios-lib belenios-tool 2>/dev/null

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
