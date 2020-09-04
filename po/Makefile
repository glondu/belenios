LINGUAS := $(shell cat LINGUAS)

POFILES=$(addsuffix .po,$(LINGUAS))
MOFILES=$(addsuffix .mo,$(LINGUAS))
POTFILE=messages.pot

all: $(MOFILES)

clean:
	rm -f *.mo *.bak

$(POTFILE): POTFILES
	ocaml-gettext --action extract --extract-pot $@ $^

%.mo: %.po
	ocaml-gettext --action compile --compile-output $@ $^

%.po: $(POTFILE)
	ocaml-gettext --action merge --merge-pot $^ $@
