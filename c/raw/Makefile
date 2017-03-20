CFLAGS = -I.
LDFLAGS = -lm
EGS = \
	eg/chaser \
	eg/chaserII \
	eg/sitting-duck \
	eg/left-turner \
	eg/right-turner

all: tankode.o $(EGS)

tankode.o: tankode.c tankode.h

eg/sitting-duck: tankode.o

eg/left-turner: tankode.o

eg/right-turner: tankode.o

eg/chaser: tankode.o

eg/chaserII: tankode.o

clean:
	rm -f *.o $(EGS)
