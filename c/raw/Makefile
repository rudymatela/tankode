CFLAGS = -I.
EGS = \
	eg/sitting-duck \
	eg/left-turner

all: tankode.o $(EGS)

tankode.o: tankode.c tankode.h

eg/sitting-duck: tankode.o

eg/left-turner: tankode.o

clean:
	rm -f *.o $(EGS)
