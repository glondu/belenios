include Makefile.config

CHROOT := chroot.tar.zst
VERSION := $(shell cd $(BELENIOS_SOURCES) && ./src/platform/version/get_build.sh)
DSC := belenios-server_$(VERSION)-1.dsc
DEB := belenios-server_$(VERSION)-1_$(ARCH).deb
SQUASHFS := belenios_$(VERSION)_$(ARCH).squashfs

all: $(SQUASHFS)

deb: $(DEB)

clean:
	dcmd rm -f chroot.tar.zst *.changes *.dsc *.build *.squashfs

$(CHROOT):
	cd $(BELENIOS_SOURCES) && contrib/debian/make-chroot.sh $(CURDIR)/$@

$(DSC):
	cd $(BELENIOS_SOURCES) && contrib/debian/make-dsc.sh $(CURDIR)

$(DEB): $(DSC) $(CHROOT)
	unset LANG; \
	sbuild --no-run-lintian --dist=stable --chroot-mode=unshare --chroot=$(CURDIR)/$(CHROOT) $(DSC)

$(SQUASHFS): $(DEB) $(CHROOT)
	cd $(BELENIOS_SOURCES) && contrib/debian/make-squashfs.sh $(CURDIR)/$(DEB) $(CURDIR)/$@
