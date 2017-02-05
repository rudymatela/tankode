GHCFLAGS = -O2
GHCIMPORTDIRS = .
GHCI = ghci
LISTHS = find . -name \*.hs
ALLHS = $(shell $(LISTHS))
EGS = \
	eg/raw/chaser       \
	eg/raw/escaper      \
	eg/raw/left-turner  \
	eg/raw/right-turner \
	eg/raw/zigzagger    \
	eg/raw/wal          \
	eg/raw/sitting-duck

all: Tankode/Raw.o Tankode/Basic.o $(EGS)

ghci: Tankode/Basic.ghci

clean: clean-hi-o
	rm -f $(EGS)

include mk/haskell.mk
