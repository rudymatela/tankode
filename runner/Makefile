# Makefile for Tankode's logic
GHCIMPORTDIRS = src:tests
GHCFLAGS = -O2
LISTHS = find src tests -name \*.hs
ALLHS = $(shell $(LISTHS))
BIN = bin/tankode-runner bin/html-palette bin/palette
TESTS = \
	tests/test-ratiomath \
	tests/test-tankode \
	tests/test-list \
	tests/test-readshow
TANKODES=../haskell/eg/{sitting-duck,chaser,escaper,left-turner,right-turner,zigzagger}
TANKODESM=../haskell/eg/{sitting-duck,chaser,left-turner,escaper}
ARGS=

all: $(BIN) display

.PHONY: display
display:
	make -Cdisplay

test: $(TESTS) test-model
	./tests/test-ratiomath
	./tests/test-tankode
	./tests/test-list
	./tests/test-readshow

test-model: $(BIN)
	./bin/tankode-runner $(TANKODES) -t1 -d --seed    42 | diff -rud tests/model/42    -
	./bin/tankode-runner $(TANKODES) -t1 -d --seed 31337 | diff -rud tests/model/31337 -
	./bin/tankode-runner $(TANKODESM) -s8x4 -t2 -d --seed 18 | diff -rud tests/model/m18   -

update-test-model: $(BIN)
	./bin/tankode-runner $(TANKODES) -t1 -d --seed    42 > tests/model/42
	./bin/tankode-runner $(TANKODES) -t1 -d --seed 31337 > tests/model/31337
	./bin/tankode-runner $(TANKODESM) -s8x4 -t2 -d --seed 18 > tests/model/m18

kill:
	killall sitting-duck chaser escaper left-turner right-turner zigzagger

bin/html-palette: src/html-palette
	mkdir -p bin
	cp src/html-palette bin/html-palette

bin/palette: src/palette
	mkdir -p bin
	cp src/palette bin/palette

bin/tankode-runner: src/tankode-runner
	mkdir -p bin
	cp src/tankode-runner bin/tankode-runner

run: all
	./bin/tankode-runner $(ARGS) $(TANKODES)

bench: all
	cat .runtimes-`hostname`
	/usr/bin/time -f%e ./bin/tankode-runner -dt1 $(TANKODES) > /dev/null

save-bench: all
	/usr/bin/time -f%e ./bin/tankode-runner -dt1 $(TANKODES) > /dev/null 2> .runtimes-`hostname`

prof:
	make clean
	make GHCFLAGS="$(GHCFLAGS) -prof -auto-all"
	/usr/bin/time -f%e ./bin/tankode-runner -dt1 $(TANKODES) +RTS -p -RTS > /dev/null
	make clean
	cat tankode-runner.prof | grep -v ' [0-5].[0-9] ......$$'

list-big-fractions: all
	./bin/tankode-runner make run | \
	grep "[0-9][0-9][0-9][0-9][0-9][0-9]/[0-9][0-9][0-9][0-9][0-9]"

list-bigger-fractions: all
	./bin/tankode-runner make run | \
	grep "[0-9][0-9][0-9][0-9][0-9][0-9][0-9]/[0-9][0-9][0-9][0-9][0-9]" \
	|| true

ghci: tests/Test.ghci

clean: clean-hi-o
	rm -f *.o *.hi $(BIN) $(TESTS) src/tankode-runner src/html-palette src/palette
	make -Cdisplay

include mk/haskell.mk
