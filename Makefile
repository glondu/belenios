all:
	ocamlbuild all.otarget

check: all
	ocamlbuild tests/legacy/sandbox.byte
	@_build/tests/legacy/sandbox.byte

clean:
	ocamlbuild -clean
	rm -f *~

tree: _build/tree.html

_build/tree.html:
	tree -o $@ -H '..'  -I '_build|_run|*~'
