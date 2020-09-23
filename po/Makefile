LINGUAS := $(shell cat LINGUAS)

POFILES=$(addsuffix .po,$(LINGUAS))
POTFILE=messages.pot

all: $(POFILES)

clean:
	rm -f *.bak

$(POTFILE): POTFILES
	cd .. && ocaml-gettext --action extract --extract-pot po/$@ $(shell cat $^)

%.po: $(POTFILE)
	ocaml-gettext --action merge --merge-pot $^ $@
