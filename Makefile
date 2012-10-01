all:
	ocamlbuild all.otarget

clean:
	ocamlbuild -clean
	rm -f *~
