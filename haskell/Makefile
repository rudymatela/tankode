GHCFLAGS = -O2
GHCIMPORTDIRS = .
GHCI = ghci
LISTHS = find . -name \*.hs
ALLHS = $(shell $(LISTHS))
EGS = \
	eg/chaserIII        \
	eg/chaserII         \
	eg/chaser           \
	eg/escaper          \
	eg/left-turner      \
	eg/right-turner     \
	eg/zigzagger        \
	eg/wal              \
	eg/sitting-duck     \
	eg/raw1/zigzagger   \
	eg/raw/chaser       \
	eg/raw/escaper      \
	eg/raw/left-turner  \
	eg/raw/right-turner \
	eg/raw/zigzagger    \
	eg/raw/wal          \
	eg/raw/sitting-duck
LIBS = \
  Tankode/Raw.o \
  Tankode/Raw0.o \
  Tankode/Raw1.o \
  Tankode/Basic.o

all: $(LIBS) $(EGS)

$(EGS): $(LIBS)

ghci: Tankode/Basic.ghci

clean: clean-hi-o
	rm -f $(EGS)

include mk/haskell.mk
