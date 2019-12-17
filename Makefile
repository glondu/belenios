ALL_TARGETS := all.otarget
ALL_TARGETS += $(if $(shell sh -c "command -v ocamlopt"),all-native.otarget)

minimal:
	rm -f _build/BUILD
	ocamlbuild minimal.otarget

all:
	rm -f _build/BUILD
	ocamlbuild $(ALL_TARGETS)

check: minimal
	mkdir -p demo/data
	demo/demo.sh
	demo/demo-threshold.sh
	demo/demo-nh.sh

clean:
	-ocamlbuild -clean
	rm -rf _build
	rm -f *~

tree: _build/tree.html

_build/tree.html: _build/_digests
	mkdir -p _build
	tree -o $@ -H '..'  -I '_build|_run|*~'

.PHONY: doc
doc:
	ocamlbuild doc.otarget
	$(MAKE) doc/specification.pdf

doc/specification.pdf: doc/specification.tex
	cd doc && for u in 1 2 3; do pdflatex specification.tex; done

archive:
	@if [ `git status --porcelain | grep -v '^?? ' | wc -l ` -eq 0 ]; then \
	  COMMIT_ID=`git describe --tags`; \
	  VERSION=`cat VERSION`; \
	  if [ "$$(printf $$COMMIT_ID | head -c$$(printf $$VERSION | wc -c))" = "$$VERSION" ]; then \
	    git archive --prefix=belenios-$$COMMIT_ID/ $$COMMIT_ID | gzip -9n > ../belenios-$$COMMIT_ID.tar.gz; \
	    ln -sf belenios-$$COMMIT_ID.tar.gz ../belenios.tar.gz; \
	    ls -l ../belenios.tar.gz; \
	  else \
	    echo "VERSION is not up-to-date!"; exit 1; \
	  fi; \
	else \
	  echo "The tree is not clean!"; exit 1; \
	fi
