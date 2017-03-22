# top-level Makefile for tankode

TANKODES=haskell/eg/{sitting-duck,chaser,escaper,left-turner,right-turner,zigzagger}
TANKODES1=haskell/eg/{sitting-duck,chaserII,chaserIII,escaper,left-turner,right-turner,zigzagger}
TANKODES2=c/raw/eg/{sitting-duck,left-turner,right-turner,chaser,chaserII} bash/{sitting-duck,left-turner}
TANKODES3=c/raw/eg/{sitting-duck,left-turner} haskell/eg/sitting-duck
TANKODESM=haskell/eg/{sitting-duck,chaser,left-turner,escaper}

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
	./bin/tankode $(ARGS) $(TANKODES)

run1: all
	./bin/tankode $(ARGS) $(TANKODES1)

run2: all
	./bin/tankode $(ARGS) $(TANKODES2)

run3: all
	./bin/tankode $(ARGS) $(TANKODES3)

run-small:
	primusrun ./bin/tankode $(TANKODESM) -s8x4 --window-size=600x300 --close-window --seed 1 -t6

gif: all
	primusrun ./bin/tankode $(TANKODESM) -s8x4 --window-size=600x300 --close-window --seed 1 -t6 --dump-frames
	#convert -delay 3 -loop 0 *.pnm tankode.gif
	#rm *.pnm

run-charge: all
	./bin/tankode $(ARGS) $(TANKODES) --draw-charge

demo: all
	./bin/tankode --no-draw-scan -n24 $(TANKODES)

demo-duck: all
	./bin/tankode --no-draw-scan -n1  -s4x8 haskell/eg/{sitting-duck,sitting-duck}

demo-left: all
	./bin/tankode --no-draw-scan -n12 -s4x8 haskell/eg/{sitting-duck,left-turner}

demo-chaser: all
	./bin/tankode --no-draw-scan -n12 -s4x8 haskell/eg/{sitting-duck,sitting-duck,chaser}

demo-chaserII: all
	./bin/tankode --no-draw-scan -n12 -s4x8 haskell/eg/{sitting-duck,zigzagger,chaserII}

clean:
	make -Crunner   clean
	make -Chaskell  clean
	make -Cc/raw    clean
	make -Cdoc/logo clean
	rm -f runner/palette.html README.html doc/tankode-protocol.html

palette: runner
	make -Crunner bin/html-palette
	./runner/bin/html-palette > runner/palette.html
