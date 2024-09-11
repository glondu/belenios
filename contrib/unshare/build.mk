include Makefile.config

CHANGES := belenios-opam_$(TOOLCHAIN_VERSION)_$(ARCH).changes
ROOTFS := rootfs_$(TOOLCHAIN_VERSION)_$(ARCH).tar.zst
VERSION := $(shell cd "$(BELENIOS_SOURCES)" && ./src/platform/version/get_build.sh)
SQUASHFS := belenios_$(VERSION)_$(ARCH).squashfs

all: $(SQUASHFS)

$(SQUASHFS): $(ROOTFS) $(CHANGES)
	cd "$(BELENIOS_SOURCES)" && \
	TMPDIR="$(BIGTMP)" \
	contrib/unshare/make-squashfs.sh "$(SUITE)" \
	  "$(CURDIR)/$(CHANGES)" "$(CURDIR)/$(ROOTFS)" \
	  "$(CURDIR)/$(SQUASHFS)"

$(ROOTFS): $(CHANGES)
	cd "$(BELENIOS_SOURCES)" && \
	TMPDIR="$(BIGTMP)" \
	contrib/unshare/make-rootfs-dev.sh "$(SUITE)" \
	  "$(CURDIR)/$(CHANGES)" "$(CURDIR)/$(ROOTFS)"

$(CHANGES): stamp
	cd "$(BELENIOS_SOURCES)" && \
	TMPDIR="$(BIGTMP)" \
	contrib/unshare/make-deb-belenios-opam.sh "$(SUITE)" \
	  "" "$(TOOLCHAIN_VERSION)" "" "$(CURDIR)"

stamp:
	touch $@
