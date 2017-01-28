Tankode
=======

Write a Tankode (tank AI) to battle other Tankodes.

	[insert gif here, github.com/rudymatela/tankode/releases/0.1/cover.gif]

Tankode is programming-language independent.  Tankodes read-and-write
to-and-from text streams following the very simple [Tankode Protocol].

Advanced language bindings are provided for:
	* no language at the moment.


Features
--------

Tankode's features:

* inspired by robocode;
* language independent (based on reading and writing to pipes / files).


Differences to Robocode
-----------------------

Tankode has some differences to Robocode:

* some game rule changes;
* tanks do not have GPS and cannot query their `x` and `y` coordinates:
	- if needed, tanks have to calculate this based on initial position and
	  movements);
	- this makes things simpler for the implementor of simpler machines;
* battlefields are not necessarily square, and can have obstacles.


Notes
-----

* for the purposes of collision and hit detection, tanks have 1 unit of
  diameter.  bullets are points.

* 360 ticks per second, 6 ticks per frame.

Some of those may not apply anymore:

each tank is a circle for purposes of collision detection, it has radius 1
at full speed, in one second, it can walk its diameter, 1/60 of diameter in 1 tick
4 seconds,   base full turn
1 second,    gun  full turn
1/6 seconds, radar full turn
at each tick, charge increases by 1/60
a shot deals 1/6 of its charge
bullet travels 3 units per second.




[Tankode Protocol]: doc/tankode-protocol.md
[Tankode Display Protocol]: doc/tankode-display-protocol.md
