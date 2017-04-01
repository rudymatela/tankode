Tankode's TODO list
===================


interface
---------

* write Python raw interface; add examples;

* write Lua raw interface; add examples;


game rules
----------

* calibrate constants of `Tankode.Constants`;

* actuallly count wins and points and show them somehow

### low priority

* add `Tankode.Functions`, parallel to `Tankode.Constants` but with functions
  representing game rules (move functions from Physics there)

* when a tank is deactivated, it should deaccelerate to 0, instead of plain
  stopping;

* detect tanks and obstacles between old and new rays;


stability
---------

* add copyright notice and license disclaimer on all relevant files

### low priority

* wait for all child processes to get rid of defunct processes
  (turns out I'll probably won't need to catch SIGCHLD after all...)
  including the display process

* add more tests to geometry module


performance
-----------

* broadcast tankode inputs to all tankodes, and only then read all tankode outputs;

* explicitly export stuff from modules (including module re-exports)
  so I don't have to import Tankode.Etc everywhere.

* do not compute scan twice somehow (currently computing when passing scanned
  point to tank and to display)


sound
-----

### low priority

* make runner work when sound files are not present

* separate sound processing from drawing


display
-------

### low priority

* draw explosion on wall hit
  (just as a simple circle?  size being half the speed?);

* draw health bar as a torus section on top of the body on the back of the gun;

* draw scan rays as solid triangles;
	- then as fading triangles;
