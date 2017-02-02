Tankode's TODO list
===================

after
-----

* draw scan rays as solid triangles;
	- then as fading triangles;

* improve protocol, use `+/-/= +/-/= -1..1 -3..3` as tankode output.  e.g.:
  `+ - 1 -3`.  The `+/-` part is done, now to `-1..1`.


later
-----

* add examples to c raw interface


later later
-----------

* make tank turn slower when at higher speeds (the new protocol helps, as the
  turning is symbolized by just `+/-/=`);

* broadcast tankode inputs to all tankodes, and only then read all tankode outputs;

* kill subprocesses before exiting (remove `make kill` workaround);

* sanitize tankode commands (e.g.: do not allow acceleration beyond limit, etc);

* refine collision between tanks:
  if one can move without the other moving (and not vice-versa) move it;

* detect tanks and obstacles between old and new rays;

* explicitly export stuff from modules (including module re-exports)
  so I don't have to import Tankode.Etc everywhere.

* draw health bar as a torus section on top of the body on the back of the gun;

* draw gun energy somehow (line on top of gun / two torus sections in front).

* when a tank is deactivated, it should deaccelerate to 0, instead of plain stopping


later later later
-----------------

* add more tests to geometry module


display
-------

* make bullet area proportional to power.  Instead of bullet radius proportional to power.
