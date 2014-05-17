# Makefile for wcheck-mode

BASE := wcheck-mode
DESC := General interface for text checkers
VERSION := $(shell date +%Y.%-m.%-d)
MAIN := $(BASE).el
PKG := $(BASE)-pkg.el
FILES := $(MAIN) $(PKG) COPYING README
NAME := $(BASE)-$(VERSION)

elpa: $(PKG)
tar: $(NAME).tar
sign: $(NAME).tar.sig
elc: $(BASE).elc

$(NAME).tar: $(FILES)
	tar --create --file $@ --transform 's,^,$(NAME)/,' $(FILES)

$(NAME).tar.sig: $(NAME).tar
	gpg --yes --detach-sign $<

$(PKG):
	@printf "(define-package \"%s\" \"%s\"\n  \"%s\")\n" \
		"$(BASE)" "$(VERSION)" "$(DESC)" >$@
	@cat $@

README: README.md
	cp -f -- $< $@

$(BASE).elc: %.elc: %.el
	emacs -Q --batch -f batch-byte-compile $<

tag:
	git tag -s $(VERSION) -m 'Version $(VERSION)' HEAD

clean:
	rm -f -- $(BASE)*.tar* README $(BASE).elc

.PHONY: elpa tar sign tag clean elc $(PKG)
