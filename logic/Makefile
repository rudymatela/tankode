# Makefile for Tankode's logic
GHCIMPORTDIRS = src:tests
GHCFLAGS = -O2 #-prof -auto-all
LISTHS = find src tests -name \*.hs
ALLHS = $(shell $(LISTHS))
BIN = bin/tankode-logic bin/html-palette bin/palette
TESTS = \
	tests/test-ratiomath \
	tests/test-tankode

all: $(BIN)

test: $(TESTS)
	./tests/test-ratiomath
	./tests/test-tankode

bin/html-palette: src/html-palette
	mkdir -p bin
	cp src/html-palette bin/html-palette

bin/palette: src/palette
	mkdir -p bin
	cp src/palette bin/palette

bin/tankode-logic: src/tankode-logic
	mkdir -p bin
	cp src/tankode-logic bin/tankode-logic

run: all
	./bin/tankode-logic

list-big-fractions: all
	./bin/tankode-logic make run | \
	grep "[0-9][0-9][0-9][0-9][0-9][0-9]/[0-9][0-9][0-9][0-9][0-9]"

list-bigger-fractions: all
	./bin/tankode-logic make run | \
	grep "[0-9][0-9][0-9][0-9][0-9][0-9][0-9]/[0-9][0-9][0-9][0-9][0-9]" \
	|| true

ghci: tests/Test.ghci

clean: clean-hi-o
	rm -f *.o *.hi $(BIN) $(TESTS) src/tankode-logic src/html-palette src/palette

include mk/haskell.mk
