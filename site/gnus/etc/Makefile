datadir = ${prefix}/share
infodir = ${prefix}/info
prefix = /usr
srcdir = .
subdir = etc
top_srcdir = ..
lispdir = $(datadir)/emacs/site-lisp
etcdir = $(lispdir)/../etc


EMACS=emacs
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
SHELL = /bin/sh

all: 

install: 
	$(SHELL) $(top_srcdir)/mkinstalldirs $(etcdir)
	cd $(srcdir) \
	&& for p in gnus-tut.txt; do \
	  echo " $(INSTALL_DATA) $$p $(etcdir)/$$p"; \
	  $(INSTALL_DATA) $$p $(etcdir)/$$p; \
	done
	$(SHELL) $(top_srcdir)/mkinstalldirs $(etcdir)/gnus
	cd $(srcdir) \
	&& for p in gnus/*.xpm gnus/*.pbm gnus/*.xbm gnus/x-splash; do \
	  echo " $(INSTALL_DATA) $$p $(etcdir)/$$p"; \
	  $(INSTALL_DATA) $$p $(etcdir)/$$p; \
	done
	$(SHELL) $(top_srcdir)/mkinstalldirs $(etcdir)/smilies
	cd $(srcdir) \
	&& for p in smilies/*.pbm smilies/*.xpm; do \
	  echo " $(INSTALL_DATA) $$p $(etcdir)/$$p"; \
	  $(INSTALL_DATA) $$p $(etcdir)/$$p; \
	done

uninstall: 
	rm -f $(etcdir)/gnus-tut.txt
	cd $(srcdir) \
	&& for p in gnus/*.xpm gnus/*.pbm gnus/*.xbm gnus/x-splash; do \
	  rm -f "$(etcdir)/$$p"; \
	done
	rmdir $(etcdir)/gnus 2> /dev/null || true
	cd $(srcdir) \
	&& for p in smilies/*.pbm smilies/*.xpm; do \
	  rm -f "$(etcdir)/$$p"; \
	done
	rmdir $(etcdir)/smilies 2> /dev/null || true

Makefile: $(srcdir)/Makefile.in ../config.status
	cd .. \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

distclean:
	rm -f *~ Makefile

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
