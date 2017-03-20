# top-level Makefile for tankode

TANKODES=haskell/eg/{sitting-duck,chaserII,chaserIII,escaper,left-turner,right-turner,zigzagger}
TANKODES2=c/raw/eg/{sitting-duck,left-turner,right-turner,chaser,chaserII} bash/{sitting-duck,left-turner}
TANKODES3=c/raw/eg/{sitting-duck,left-turner} haskell/eg/sitting-duck

all: runner haskell c-raw

.PHONY: runner
runner:
	make -C runner

.PHONY: haskell
haskell:
	make -C haskell

.PHONY: c-raw
c-raw:
	make -C c/raw

test: all
	make -C runner test

.PHONY: doc
doc:
	markdown doc/tankode-protocol.md > doc/tankode-protocol.html
	markdown README.md > README.html

run: all
	./bin/tankode $(TANKODES)

run2: all
	./bin/tankode $(TANKODES2)

run3: all
	./bin/tankode $(TANKODES3)

run-charge: all
	./bin/tankode $(TANKODES) --draw-charge

clean:
	make -Crunner   clean
	make -Chaskell  clean
	make -Cc/raw    clean
	make -Cdoc/logo clean
	rm -f runner/palette.html README.html doc/tankode-protocol.html

palette: runner
	make -Crunner bin/html-palette
	./runner/bin/html-palette > runner/palette.html
