SVGS=\
	tankode-logo-colour.svg \
	tankode-logo-bw.svg \
	tankode-logo-greyscale.svg \
	black.html \
	white.html \
	dark.html \
	light.html

all: $(SVGS)


tankode-logo-colour.svg: tankode-logo.svg Makefile
	./bin/colorize black  grey4  blue2  blue4  grey7  grey1 orange6 > tankode-logo-colour.svg

tankode-logo-bw.svg: tankode-logo.svg Makefile
	./bin/colorize ffffff 000000 000000 000000 ffffff 000000 000000 > tankode-logo-bw.svg

tankode-logo-greyscale.svg: tankode-logo.svg Makefile
	./bin/colorize ffffff 3f3f3f 5f5f5f 3f3f3f bfbfbf 1f1f1f 3f3f3f > tankode-logo-greyscale.svg

dark.html:
	./bin/html-colorize black > dark.html

light.html:
	./bin/html-colorize white > light.html

black.html:
	./bin/html-colorize 000000 > black.html

white.html:
	./bin/html-colorize ffffff > white.html

view: all
	feh --magick-timeout 1 tankode-logo-colour.svg -Bwhite

clean:
	rm -f $(SVGS)
