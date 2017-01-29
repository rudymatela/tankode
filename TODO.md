Tankode's TODO list
===================

* draw scan rays as solid triangles;
	- then as fading triangles;

* add function to draw "pills", use it to draw health bars and tank tracks;

* improve protocol, use `+/-/= +/-/= -1..1 -3..3` as tankode output.  e.g.:
  `+ - 1 -3`.


later
-----

* add examples to c raw interface;

* add some tests.


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

* draw gun energy somehow (line on top of gun, two torus sections in front).

display
-------

* leave a configurable "trail" with decreasing alpha for all objects drawn
  (when implementing, add motion blur *after* this)

* add motion blur:
  `https://en.wikibooks.org/wiki/OpenGL_Programming/Motion_Blur`,
  two frames at first.  Then test up to 6 frames (the maximum
  number of frames sent by tankode-logic).

* make bullet area proportional to power.  Instead of bullet radius proportional to power.

* use z axis on opengl so that tracks are always below bodies (even between
  different tanks).  Also think about similar things with other elements
  (bullets, radars, etc).
