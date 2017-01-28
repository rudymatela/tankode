CFLAGS = -I.
EGS = \
	eg/sitting-duck

all: tankode.o $(EGS)

tankode.o: tankode.c tankode.h

eg/sitting-duck: tankode.o

clean:
	rm -f *.o $(EGS)
