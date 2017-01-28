Tankode's Display Protocol
==========================

If you're looking to write a Tankode, please look at the [Tankode Protocol].

The logic behind the game is in a separate program to the graphical frontend.
Those programs communicate by a one way protocol, described here.

No time to document now.  Here is an example:

	field 12/1 8/1
	tank sittingduck 2/5 2/5 2/5 3/5 3/5 3/5 4/5 4/5 4/5
	tank chaser 0/1 1/5 1/3 2/5 2/5 2/5 0/1 0/1 0/1
	tank crazy 1/1 1/1 0/1 1/1 1/2 0/1 1/1 1/1 1/1

	tick 0
	tankpos 2/3 2/3 0/1 0/1 0/1
	bullet 1/1 0/1 0/1 7/8
	tankpos 1/1 3/1 3/4 0/1 0/1
	tankpos 7/1 5/1 1/4 0/1 0/1
	bullet 2/3 0/1 0/1 1/8
	bullet 1/3 0/1 0/1 1/4

	tick 1
	tankpos 2/3 41/60 0/1 0/1 0/1
	bullet 1/1 1/20 1/20 7/8
	tankpos 61/60 3/1 3/4 0/1 0/1
	tankpos 839/120 449/90 181/720 1/720 1/720
	bullet 2/3 1/15 1/30 1/8
	bullet 1/3 1/20 1/10 1/4

	tick 2
	tankpos 2/3 7/10 0/1 0/1 0/1
	bullet 1/1 1/10 1/10 7/8
	tankpos 31/30 3/1 3/4 0/1 0/1
	tankpos 419/60 224/45 91/360 1/360 1/360
	bullet 2/3 2/15 1/15 1/8
	bullet 1/3 1/10 1/5 1/4

	...
