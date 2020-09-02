minimal:
	dune build -p belenios-platform,belenios-platform-native,belenios,belenios-tool

build-debug-server:
	dune clean
	BELENIOS_DEBUG=1 dune build
	rm -rf _run/usr
	dune install --destdir=_run --prefix=/usr 2>/dev/null
	git archive --prefix=belenios-debug/ HEAD | gzip -9n > _run/usr/share/belenios-server/belenios.tar.gz

build-release-server:
	dune clean
	BELENIOS_DEBUG= dune build --release
	rm -rf _run/usr
	dune install --destdir=_run --prefix=/usr 2>/dev/null
	git archive --prefix="belenios-$(shell git describe --tags)/" HEAD | gzip -9n > _run/usr/share/belenios-server/belenios.tar.gz

build-debug-tool:
	dune clean
	BELENIOS_DEBUG=1 dune build
	cp _build/install/default/bin/belenios-tool _build/

check:
	$(MAKE) build-debug-tool
	mkdir -p demo/data
	demo/demo.sh
	demo/demo-threshold.sh
	demo/demo-nh.sh

clean:
	dune clean

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
