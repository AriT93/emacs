############################################################################
# Makefile to install Gnus under Windows NT using nmake.
# Adrian Aichner, aichner@ecf.teradyne.com, Teradyne GmbH, 1999-07-14.
#
# Please specify path to the Gnus sources here, if $(MAKEDIR) is
# inappropriate:
#
GNUSDIR=$(MAKEDIR)
#
# Please specify the path where you want Gnus installed:
#
# INSTALLDIR=c:\XEmacs\site-packages\pgnus-0.98
#
# Please specify the path to the XEmacs executable here:
#
# XEMACS=c:\XEmacs\XEmacs-21.1.8\i386-pc-win32\xemacs.exe
#
# Specify wheter you want to use XEmacs mail-lib package with UIDL support.
#
USE_XEMACS_MAIL_LIB=0
#
############################################################################
# Do not change anything below this line.
MANDIR=$(GNUSDIR)\texi
LISPDIR=$(GNUSDIR)\lisp
INFODIR=$(INSTALLDIR)\info
FLAGS=-batch -q -no-site-file
VARDEFS=XEMACS="$(XEMACS)" INFODIR="$(INFODIR)" MANDIR="$(MANDIR)" FLAGS="$(FLAGS)"

!if !exist("$(XEMACS)")
!error Please set XEMACS to point to XEmacs executable, "$(XEMACS)" does not exist.
!endif

!if "$(INSTALLDIR)" == ""
!error Please specify INSTALLDIR.
!endif

all: lick info

lick:
#	protect paths containing whitespace
	cd "$(LISPDIR)"
!if $(USE_XEMACS_MAIL_LIB) != 0
!if exist("$(LISPDIR)\pop3.el")
	rename pop3.el pop3-not-used.el
!endif
!if exist("$(LISPDIR)\pop3.elc")
	rename pop3.elc pop3-not-used.elc
!endif
	@echo Gnus pop3.el not used, verify you have XEmacs mail-lib package.
!else
	@echo Consider using XEmacs mail-lib package by setting USE_XEMACS_MAIL_LIB.
!endif
	"$(XEMACS)" $(FLAGS) -l ./dgnushack.el -f dgnushack-compile

install-without-info: lick
	-rmdir /s /q "$(INSTALLDIR)"
	xcopy /i "$(LISPDIR)\*.el" "$(INSTALLDIR)\lisp"
	xcopy /i "$(LISPDIR)\*.elc" "$(INSTALLDIR)\lisp"
	xcopy /i /s "$(GNUSDIR)\etc" "$(INSTALLDIR)\etc"

install: install-without-info info
	cd "$(MANDIR)"
#	protect paths containing whitespace
	$(MAKE) /$(MAKEFLAGS) /f xemacs.mak $(VARDEFS) install

info:
#	protect paths containing whitespace
	cd "$(MANDIR)"
	$(MAKE) /$(MAKEFLAGS) /f xemacs.mak $(VARDEFS) all

clean:
	del *.orig *.rej

elclean:
	del "$(LISPDIR)\*.elc"

distclean:
	$(MAKE) /$(MAKEFLAGS) /f xemacs.mak $(VARDEFS) clean
	del *~
#	protect paths containing whitespace
	cd "$(LISPDIR)"
	del "*.orig" "*.rej" "*.elc" "*~"
#	protect paths containing whitespace
	cd "$(MANDIR)"
	$(MAKE) /$(MAKEFLAGS) /f xemacs.mak $(VARDEFS) distclean
############################################################################
# Subsidiary makefile to install Gnus under Windows NT using nmake.
# Adrian Aichner, aichner@ecf.teradyne.com, Teradyne GmbH, 1999-07-14.
############################################################################
# Do not change anything below this line.
# No spaces are allowed due to inference rule limitation:
MAKEINFO="$(XEMACS)" $(FLAGS) -l texinfmt -f batch-texinfo-format

!if "$(XEMACS)" == ""
!message Please use $(MAKEDIR)\..\xemacs.mak instead.
!error
!endif

!if ["$(XEMACS)" $(FLAGS) -eval \
"(condition-case nil (require (quote texinfo)) (t (kill-emacs 1)))"]
!message Install `texinfo' in XEmacs from Options->Manage Packages->List & Install.
!message Without it Gnus info documentation cannot be built by XEmacs!
!error Cannot build `info' without `texinfo'.
!endif

all: gnus message emacs-mime

gnus: gnus.texi
	$(MAKEINFO) "$**"

message: message.texi
	$(MAKEINFO) "$**"

emacs-mime: emacs-mime.texi
	$(MAKEINFO) "$**"

clean:
	del gnus.*.bak *.ky *.cp *.fn *.cps *.kys *.log *.aux *.dvi *.vr \
	*.tp *.toc *.pg gnus.latexi *.aux *.[cgk]idx \
	gnus.ilg gnus.ind gnus.[cgk]ind gnus.idx \
	gnus.tmptexi *.tmplatexi gnus.tmplatexi1 texput.log *.orig *.rej \
	gnus.latexi*~* xface.tex picons.tex smiley.tex *.latexi

makeinfo: all

veryclean:
	$(MAKE) /$(MAKEFLAGS) /f xemacs.mak $(VARDEFS) clean
	del /f gnus.dvi gnus.ps texi2latex.elc

distclean:
	$(MAKE) /$(MAKEFLAGS) /f xemacs.mak $(VARDEFS) clean
	del /f *.orig *.rej *.elc *~ gnus gnus-?? Makefile
	del /f message
	del /f emacs-mime

install: all
	-mkdir "$(INFODIR)"
	xcopy /i /s "$(MANDIR)\dir" "$(INFODIR)"
	xcopy /i /s "$(MANDIR)\gnus" "$(INFODIR)"
	xcopy /i /s "$(MANDIR)\gnus-??" "$(INFODIR)"
	xcopy /i /s "$(MANDIR)\message" "$(INFODIR)"
	xcopy /i /s "$(MANDIR)\emacs-mime" "$(INFODIR)"
