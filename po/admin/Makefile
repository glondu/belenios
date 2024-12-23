LINGUAS := $(shell cat LINGUAS)

POFILES=$(addsuffix .po,$(LINGUAS))
POTFILE=messages.pot

all: dune $(POFILES)

dune: LINGUAS
	../gen-dune.sh admin < $^ > $@
	dune fmt

clean:
	rm -f *.bak

$(POTFILE): POTFILES
	cd ../.. && ocaml-gettext --action extract --extract-pot po/admin/$@ $(shell cat $^)

%.po: $(POTFILE)
	ocaml-gettext --action merge --merge-pot $^ $@
