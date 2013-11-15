minimal:
	ocamlbuild minimal.otarget

all:
	ocamlbuild all.otarget

check: all
	ocamlbuild demo/demo.byte
	@_build/demo/demo.byte

clean:
	ocamlbuild -clean
	rm -f *~

tree: _build/tree.html

_build/tree.html: _build/_digests
	mkdir -p _build
	tree -o $@ -H '..'  -I '_build|_run|*~'

doc:
	ocamlbuild doc.otarget
	$(MAKE) doc/specification.pdf

doc/specification.pdf: doc/specification.tex
	cd doc && for u in 1 2; do pdflatex specification.tex; done
