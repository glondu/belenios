all:
	ocamlbuild all.otarget

check: all
	ocamlbuild tests/legacy/sandbox.byte
	@_build/tests/legacy/sandbox.byte

clean:
	ocamlbuild -clean
	rm -f *~
