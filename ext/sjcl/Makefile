SOURCES := $(addprefix core/,$(shell cat core/INDEX))

sjcl.js: sjcl-full.js
	yui-compressor -o $@ $<

sjcl-full.js: $(SOURCES)
	cat $^ > $@
