minimal:
	ocamlbuild minimal.otarget

all:
	ocamlbuild all.otarget

check: all
	demo/demo.sh

clean:
	ocamlbuild -clean
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
	test `git status --porcelain | wc -l ` -eq 0 && { \
	COMMIT_ID=`git describe --tags`; \
	git archive --prefix=belenios-$$COMMIT_ID/ $$COMMIT_ID | gzip -9n > ../belenios-$$COMMIT_ID.tar.gz; \
	ln -sf belenios-$$COMMIT_ID.tar.gz ../belenios.tar.gz; }
