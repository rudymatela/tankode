# top-level Makefile for tankode

TANKODES=haskell/eg/{sitting-duck,chaser,escaper,left-turner,right-turner,zigzagger}

all: display logic haskell

.PHONY: display
display:
	make -C display

.PHONY: logic
logic:
	make -C logic

.PHONY: haskell
haskell:
	make -C haskell

test: all
	make -Clogic test

.PHONY: doc
doc:
	markdown doc/tankode-protocol.md > doc/tankode-protocol.html
	markdown README.md > README.html

run: display logic haskell
	./bin/tankode $(TANKODES)

run-charge: display logic haskell
	./bin/tankode $(TANKODES) --draw-charge

clean:
	make -Cdisplay  clean
	make -Clogic    clean
	make -Chaskell  clean
	make -Cc/raw    clean
	make -Cdoc/logo clean
	rm -f logic/palette.html README.html doc/tankode-protocol.html

palette: logic
	make -Clogic bin/html-palette
	./logic/bin/html-palette > logic/palette.html
