Tankode's TODO list
===================


interface
---------

* complete c raw interface; add examples;

* write ruby raw interface; add examples;

* change obstacles to be arbitrary convex polygons instead of triangles (easier
  to compute on logic, not actually harder to draw)

* tankode-logic automatically launches and pipes itself to display (disablable)


game rules
----------

* calibrate constants of `Tankode.Constants`;

* refine collision between tanks:
  if one can move without the other moving (and _not_ vice-versa) move it;

* sanitize tankode commands (e.g.: do not allow shooting beyond limit, etc);

* damage tanks when they collide with each other

### low priority

* add `Tankode.Functions`, parallel to `Tankode.Constants` but with functions
  representing game rules (move functions from Physics there)

* when a tank is deactivated, it should deaccelerate to 0, instead of plain
  stopping;

* detect tanks and obstacles between old and new rays;


stability
---------

* kill subprocesses before exiting (remove `make kill` workaround);

* add more tests to geometry module


performance
-----------

* broadcast tankode inputs to all tankodes, and only then read all tankode outputs;

* explicitly export stuff from modules (including module re-exports)
  so I don't have to import Tankode.Etc everywhere.

* do not compute scan twice somehow (currently computing when passing scanned
  point to tank and to display)


display
-------

* add configuration switches

* draw health bar as a torus section on top of the body on the back of the gun;

* make bullet area proportional to power.  Instead of bullet radius proportional to power.

* draw scan rays as solid triangles;
	- then as fading triangles;


documentation and project organization
--------------------------------------

* make display and logic a subfolder of runner
