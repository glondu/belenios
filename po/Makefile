LINGUAS := $(shell cat LINGUAS)

POFILES=$(addsuffix .po,$(LINGUAS))
POTFILE=messages.pot

all: $(POFILES)

clean:
	rm -f *.bak

$(POTFILE): POTFILES
	ocaml-gettext --action extract --extract-pot $@ $^

%.po: $(POTFILE)
	ocaml-gettext --action merge --merge-pot $^ $@
