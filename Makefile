all:
	ocamlbuild all.otarget

check: all
	ocamlbuild tests/sandbox.byte
	@_build/tests/sandbox.byte

clean:
	ocamlbuild -clean
	rm -f *~
