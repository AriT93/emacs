# Generated automatically from Makefile.in by configure.
# where the w3 lisp files should go
srcdir  = .
prefix  = /usr
datadir = ${prefix}/share
lispdir = $(prefix)/share/emacs/site-lisp
infodir = ${prefix}/info

DOTEMACS=/home/abturet/.emacs
SUBDIRS =lisp texi etc

INSTALL   = /usr/bin/install -c
RM        = rm -f

.PHONY: $(SUBDIRS) dotemacs

all:	w3 info

w3:
	cd lisp && $(MAKE) $@

fast: info
	cd lisp && $(MAKE) $@

html info dvi:
	cd texi && $(MAKE) $@

install: all
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done

distclean: clean
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done
	$(RM) config.* Makefile

clean:
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) $@ ); done

dotemacs:
	@if (grep ";;; Emacs/W3 Configuration" $(DOTEMACS) 2>&1) >/dev/null; then \
		echo Emacs/W3 setup already exists in $(DOTEMACS);	\
	else								\
		test -d `dirname $(DOTEMACS)` || mkdir -p `dirname $(DOTEMACS)`; \
		(echo >> $(DOTEMACS)); \
		(echo ";;; Emacs/W3 Configuration" >> $(DOTEMACS)); \
		(echo "(setq load-path (cons \"$(lispdir)\" load-path))" >> $(DOTEMACS)); \
		(echo "(condition-case () (require 'w3-auto \"w3-auto\") (error nil))" >> $(DOTEMACS)); \
		echo "Added Emacs/W3 setup to $(DOTEMACS)"; \
	fi

Makefile: $(srcdir)/Makefile.in ./config.status
	( cd . ; CONFIG_FILES=Makefile ./config.status )

$(srcdir)/configure: $(srcdir)/configure.in $(srcdir)/aclocal.m4
	cd $(srcdir) && autoreconf

./config.status: $(srcdir)/configure
	cd . && ./config.status --recheck

###############################################################################
# Distribution building, the joy of my life
###############################################################################
SUPPORTFILES =  HOWTO TODO README INSTALL README.NT README.VMS BUGS
DISTFILES    =  configure.in aclocal.m4 Makefile.in install-sh	\
		configure descrip.mms ChangeLog	build.bat 	\
		$(SUPPORTFILES)
MAJOR = 4
MINOR = 0
VERSION = $(MAJOR).$(MINOR).$(shell cat .version)
DIRNAME = w3-$(VERSION)

newversion:
	if [ ! -f .version ]; then \
		echo 1 > .version; \
	else \
		expr 0`cat .version` + 1 > .version; \
	fi
	cvs commit -m "" .version

version: newversion clean $(DISTFILES)
	echo "Building version $(VERSION)"
	./build-dist --version=$(VERSION)

realversion: $(DISTFILES)
	mkdir /tmp/$(DIRNAME)
	cp $(DISTFILES) /tmp/$(DIRNAME)
	for x in $(SUBDIRS); do ( cd $$x && $(MAKE) version VERSION=$(VERSION) DIRNAME=/tmp/$(DIRNAME) ); done
	tar cf - contrib | ( cd /tmp/$(DIRNAME) ; tar xf - )
	( cd /tmp ; tar cf - $(DIRNAME) | gzip -c -9 > /tmp/$(DIRNAME).tar.gz )
	rm -fr /tmp/$(DIRNAME)

###############################################################################
# Package building, yet another joy
###############################################################################
AUTHOR_VERSION = $(VERSION)
MAINTAINER = William Perry <wmperry@cs.indiana.edu>
PACKAGE = w3
PKG_TYPE = regular
REQUIRES = w3 mail-lib xemacs-base
CATEGORY = comm

_pkg.el: Makefile
	@echo Creating _pkg.el
	@echo ";;;###autoload" > _pkg.el
	@echo "(package-provide '$(PACKAGE)" >> pkg.el
	@echo "		:version $(VERSION)" >> pkg.el
