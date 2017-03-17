Tankode's TODO list
===================


interface
---------

* complete c raw interface; add examples;

* write ruby raw interface; add examples;

* fix tankode examples for the raw Haskell interface

* change obstacles to be arbitrary convex polygons instead of triangles (easier
  to compute on logic, not actually harder to draw)

* tankode-logic automatically launches and pipes itself to display (disablable)


game rules
----------

* calibrate constants of `Tankode.Constants`;

### low priority

* add `Tankode.Functions`, parallel to `Tankode.Constants` but with functions
  representing game rules (move functions from Physics there)

* when a tank is deactivated, it should deaccelerate to 0, instead of plain
  stopping;

* detect tanks and obstacles between old and new rays;


stability
---------

* kill subprocesses before exiting (remove `make kill` workaround);
  what's missing now is to deactivate surviving tankodes at the end of simulation
  or when SIGKILL is received

### low priority

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

### low priority

* draw health bar as a torus section on top of the body on the back of the gun;

* draw scan rays as solid triangles;
	- then as fading triangles;

* draw bullet explosion when hitting wall or tank (simple circle?)


documentation and project organization
--------------------------------------

* make display and logic a subfolder of runner
