TEXI = xslt-process.texi
IMAGES = $(wildcard *.png)
GENERATED = $(subst ../etc/,,$(wildcard ../etc/*.xpm))

INFO = $(TEXI:.texi=.info)
DVI = $(TEXI:.texi=.dvi)
PS = $(TEXI:.texi=.ps)
PDF = $(TEXI:.texi=.pdf)
PDFIMAGES = $(IMAGES:.png=.pdf) $(GENERATED:.xpm=.pdf)
EPSIMAGES = $(IMAGES:.png=.eps) $(GENERATED:.xpm=.eps)
PNGIMAGES = $(GENERATED:.xpm=.png)
#JPGIMAGES = $(IMAGES:.png=.jpg)
HTML = $(TEXI:.texi=.html)

DEPEND = sizes.sed xslt-process.texi version.texi

.SUFFIXES: .in .texi .png .info .dvi .ps .pdf .eps .jpg .html

.texi.info:
	makeinfo $<

.texi.dvi:
	texi2dvi $<
	texi2dvi $<

.dvi.ps:
	dvips -o $@ $<

.texi.html:
	../bin/texi2html -nosec_nav -nomenu $<
# Remove the CENTER specification for images. Also fix a bug in how
# texi2html deals with multitables that include images.
	sed -e 's%<CENTER>\(<IMG[^>]*>\)</CENTER>%\1%g' \
	  -e 's%<TR><TD></TD>%<TR><TD>%g' \
	  <$@ >/tmp/new-$@
	mv /tmp/new-$@ $@

.texi.pdf:
	texi2pdf $<

all: info dvi ps pdf html

info: $(DEPEND) $(INFO)
dvi: $(DEPEND) $(EPSIMAGES) $(DVI)
ps: $(DEPEND) $(EPSIMAGES) $(PS)
pdf: $(DEPEND) $(PDFIMAGES) $(PDF)
html: $(DEPEND) $(PNGIMAGES) $(HTML)

$(PNGIMAGES):
	(cd ../etc/; make)

version.texi: xslt-process.texi.in
	@echo "@set version $(VERSION)" >$@
	@echo "@set update-month `date +'%B %Y'`" >> $@

# Generate a texinfo file containing width definitions for the
# images. This is used by main texinfo file when including the image
# files to resize them. The scale is defined below
sizes.sed: $(IMAGES)
	(scale=0.75; \
	 for f in $(IMAGES); do \
	  width=`identify $$f | awk '{print $$3}' | awk -F x '{print $$1}'`; \
	  height=`identify $$f | awk '{print $$3}' | awk -F x '{print $$2}'`; \
	  scalew=`echo $$scale '*' $$width | bc | awk '{printf "%d", $$0}'`; \
	  scaleh=`echo $$scale '*' $$height | bc | awk '{printf "%d", $$0}'`; \
	  var=`echo $$f | sed 's/.png//g'`; \
	  echo "s/@image{$$var}/@image{$$var,$${scalew}pt,$${scaleh}pt}/g"; \
	 done) > $@

xslt-process.texi: xslt-process.texi.in sizes.sed
	rm -f $@
	sed -f sizes.sed xslt-process.texi.in > $@
	chmod a-w $@

.png.pdf:
	@(size=`identify $< | awk '{print $$2}'`; \
	  echo convert -page $$size $< $@; \
	  convert -page $$size $< $@)

.png.jpg:
	@(size=`identify $< | awk '{print $$2}'`; \
	  echo convert -page $$size $< $@; \
	  convert -page $$size $< $@)

.png.eps:
	@(size=`identify $< | awk '{print $$2}'`; \
	  echo convert -page $$size $< $@; \
	  convert -page $$size $< $@)

clean:
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr version.texi \
	    $(PDFIMAGES) $(EPSIMAGES) *.dvi sizes.sed xslt-process.texi \
	    *.cps

distclean: clean
	rm -f *.dvi *.html *.pdf *.ps *.jpg *.eps *.info *.info-*
