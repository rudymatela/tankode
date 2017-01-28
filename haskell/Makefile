EGS = \
	eg/raw/chaser       \
	eg/raw/escaper      \
	eg/raw/left-turner  \
	eg/raw/right-turner \
	eg/raw/zigzagger    \
	eg/raw/wal          \
	eg/raw/sitting-duck

all: Tankode/Raw.o $(EGS)

Tankode/Raw.o: Tankode/Raw.hs
	ghc -O2 Tankode/Raw.hs

eg/raw/%: eg/raw/%.hs Tankode/Raw.o
	ghc -O2 $<

ghci:
	ghci Tankode/Raw.hs

clean:
	rm -f */*.{o,hi} */*/*.{o,hi} $(EGS)
