GHC = ghc -O2
GHCI = ghci
EGS = \
	eg/raw/chaser       \
	eg/raw/escaper      \
	eg/raw/left-turner  \
	eg/raw/right-turner \
	eg/raw/zigzagger    \
	eg/raw/wal          \
	eg/raw/sitting-duck

all: Tankode/Raw.o Tankode/Basic.o $(EGS)

Tankode/Raw.o: Tankode/Raw.hs Tankode/Constants.hs
	$(GHC) Tankode/Raw.hs

Tankode/Basic.o: Tankode/Basic.hs Tankode/Constants.hs
	$(GHC) Tankode/Basic.hs

eg/raw/%: eg/raw/%.hs Tankode/Raw.o
	$(GHC) $<

ghci:
	$(GHCI) Tankode/Raw.hs

clean:
	rm -f */*.{o,hi} */*/*.{o,hi} $(EGS)
