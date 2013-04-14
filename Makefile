# Makefile for wcheck-mode

BASE := wcheck-mode
DESC := General interface for text checkers
VERSION := $(shell date +%Y.%-m.%-d)
MAIN := $(BASE).el
PKG := $(BASE)-pkg.el
FILES := $(MAIN) $(PKG) README COPYING
NAME := $(BASE)-$(VERSION)

elpa: $(NAME).tar
sign: $(NAME).tar.sig

$(NAME).tar: $(FILES)
	tar --create --file $@ --transform 's,^,$(NAME)/,' $(FILES)

$(NAME).tar.sig: $(NAME).tar
	gpg --yes --detach-sign $<

$(PKG):
	@printf "(define-package \"%s\" \"%s\"\n  \"%s\")\n" \
		"$(BASE)" "$(VERSION)" "$(DESC)" >$@
	@cat $@

README: README.org
	emacs -Q --batch --file $< --funcall org-export-as-ascii
	mv -f -- $@.txt $@

tag:
	git tag -s $(VERSION) -m 'Version $(VERSION)' HEAD

clean:
	rm -f -- $(PKG) *.sig *.tar README

.PHONY: elpa sign tag clean
