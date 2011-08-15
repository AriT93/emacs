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
