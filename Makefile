# Makefile for wcheck-mode

BASE := wcheck-mode
DESC := General interface for text checkers
VERSION := $(shell date +%Y.%-m.%-d)
MAIN := $(BASE).el
PKG := $(BASE)-pkg.el
FILES := $(MAIN) $(PKG) COPYING README
NAME := $(BASE)-$(VERSION)
ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

elpa: $(NAME).tar
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

$(ELC): %.elc: %.el
	emacs -Q --batch -f batch-byte-compile $<

tag:
	git tag -s $(VERSION) -m 'Version $(VERSION)' HEAD

clean:
	rm -f -- $(PKG) $(BASE)*.tar* README $(ELC)

.PHONY: elpa sign tag clean elc
