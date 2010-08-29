###
### Makefile for the BHL mode
###
### Author:  Bastien Guerry <bzg AT altern DOT org>
### Revised: 23/07/2003
### Created: 11/16/2002
### URL: http://www.nongnu.org/bhl/

### (0) Set the following variables as you like.
### (1) ~$ make		
### (2) ~$ make info
### (3) ~$ make install
### (4) ~$ make install-info	
### (4) ~$ make pdf

################################################################
##
## EDIT THE FOLLOWINGS
##

prefix=/usr/share
infodir=$(prefix)/info

##
## Texinfo Manual
##

manual = bhl.texi	#English manual

##
## Compile engines
##

EMACS = emacs
#EMACS = xemacs
#EMACS = mule

##
## A directory where bhl.el[c] will be installed.
##

elispdir = $(prefix)/share/emacs/site-lisp/
#elispdir  = $(prefix)/lib/$(EMACS)/site-lisp
#elispdir = $(prefix)/lib/emacs

##
## A directory where bhl2* will be installed.
##
bindir = $(prefix)/bin/

## Conversion tools

TEXI2PDF = texi2pdf
TEXI2HTML = makeinfo --html --no-split
#TEXI2HTML = texi2html

################################################################
##
## DO NOT EDIT THE FOLLOWINGS
##

CP = cp
RM = rm -f
MKDIR = mkdir -p
TEMPFILE = tmp.el
PWD =

INSTALLINFO = `which install-info`

INSTALLINFO_ARGS  = bhl.info* $(infodir)/dir

################################################################

all: el

info: infofile

install: install-el install-scripts

install-info: inst-info

pdf: make-pdf

html: make-html

clean: 

################################################################

el: bhl.el
	@echo "Compiling EL files of BHL ... "
	@echo "PLEASE IGNORE WARNINGS IF DISPLAYED. TAKE IT EASY!"
	$(EMACS) -batch -q -no-site-file --eval "(byte-compile-file \"bhl.el\")"
	@echo "Compiling EL files of BHL ... done."
	@echo "You can now install the BHL mode (make install)."

install-el: bhl.elc
	-@if [ ! -d $(elispdir) ]; then \
		$(MKDIR) $(elispdir); \
	fi
	$(CP) bhl.el* $(elispdir)

################################################################

install-scripts: 
	-@if [ ! -d $(bindir) ]; then \
		$(MKDIR) $(bindir); \
	fi
	$(CP) contrib/bhl2* $(bindir)

################################################################

infofile:
#	$(EMACS) -batch -q -no-site-file -l texinfmt \
#		-f batch-texinfo-format $(manual)
#	Uncomment the two lines above and uncomment the next line 
#	if you prefer to use makeinfo instead of texinfmt.
	makeinfo --no-split $(manual)
	@echo "You can now install the info file (make install-info)."

inst-info:
	-@if [ ! -d $(infodir) ]; then \
		$(MKDIR) $(infodir); \
	fi; \
	cp bhl.info* $(infodir)
	-@if [ -x "$(INSTALLINFO)" ]; then \
		$(INSTALLINFO) $(INSTALLINFO_ARGS); \
	else \
	echo "Don't forget to edit $(infodir)/dir so that it includes the info pages for the BHL mode."; \
	fi

################################################################

make-pdf:	
	@echo "Creating PDF manual..."
	$(TEXI2PDF) $(manual)
	@echo "... done."

################################################################

make-html:
	@echo "Creating HTML manual..."
	$(TEXI2HTML) $(manual)
	@echo "... done."

################################################################

clean:
	$(RM) *cps *elc *fn *aux *cp *ky *log *pg *tp *toc *vr *~ $(TEMPFILE)

##
## End of Makefile
##
