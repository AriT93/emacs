IMAGES = $(wildcard *.xpm)

PNGIMAGES = $(addprefix ../doc/, $(IMAGES:.xpm=.png))

.SUFFIXES: .xpm .png

../doc/%.png: %.xpm
	convert $< $@

all: $(PNGIMAGES)

clean:

distclean:
	rm -f $(PNGIMAGES)
