# top-level Makefile for tankode

TANKODES=haskell/eg/raw/{sitting-duck,chaser,escaper,left-turner,right-turner,zigzagger}

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

run-display: display
	./display/bin/tankode-display

run-logic: logic
	./logic/bin/tankode-logic $(TANKODES) # +RTS -p -RTS

run: display logic haskell
	./bin/tankode $(TANKODES)
	make kill

bench: display logic haskell
	cat .runtimes-`hostname`
	/usr/bin/time -f%e ./logic/bin/tankode-logic -t1 $(TANKODES) > /dev/null
	make kill

save-bench: display logic haskell
	/usr/bin/time -f%e ./logic/bin/tankode-logic -t1 $(TANKODES) > /dev/null 2> .runtimes-`hostname`
	make kill

prof:
	make -Clogic clean
	make -Clogic GHCFLAGS="-prof -auto-all"
	/usr/bin/time -f%e ./logic/bin/tankode-logic -t1 $(TANKODES) +RTS -p -RTS > /dev/null
	make -Clogic clean
	cat tankode-logic.prof | grep -v ' [0-5].[0-9] ......$$'

kill:
	killall sitting-duck chaser escaper left-turner right-turner zigzagger

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
