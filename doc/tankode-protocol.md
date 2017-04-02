Tankode Protocol
================

Tankodes (Tank AI's) communicate with the game server by
reading-from-and-writing-to text streams following the Tankode protocol.

All coordinates and color components are represented by rationals of the form
`n/d`.  "Over" is indicated by a blank line.


identification: tankode -> server
---------------------------------

	<tank_name> <body_colour> <track_colour> <turret_colour> <bullet_colour> <radar_colour> <scan_colour>

`tank_name` should be between 1 and 31 ASCII characters.
Colors should be on "Hex" form: `#rrggbb`, each component ranging from `00` to `ff`.


### Example (identification)

"sittingduck" Tankode with yellow body, orange tracks, orange turret, orange
bullets; white radar and grey scan:

	sittingduck #ffff00 #dd7700 #ff8800 #ff8800 #ffffff #888888


game tick
---------

At each game tick, the server starts by sending a line of information to all
tankodes.  All tankodes should reply with a line describing its actions.

### server -> tankode

	<life> <speed> <enemy> <wall>

`life` and `speed` are in the range `0/1` to `1/1`.
`enemy` and `wall` are either `-` (no enemy or wall in sight) or a ratio `n/d`
representing the distance in units from this tank to the obstacle.  A tank is 1
unit wide (diameter).  `enemy` and `wall` will rarely appear at the same time:
only when the radar is looking at the exact point where a tank is touching a
wall.

### tankode -> server:

	<accel> <body-turn> <turret-turn> <radar-turn> <shoot-charge>

### Example (tick)

server -> tankode:

	3/4 2/3 3 -

tankode -> server:

	-1/2 -1/1 1/1 1/2 0

Example (full session)
----------------------

Lines alternate between tankode and server, beginning with a tankode.

	sittingduck #ffff00 #dd7700 #ff8800 #ff8800 #ffffff #888888
	1/1 0/1 - -
	1 -1/1 1/1 1/2 0
	3/4 2/3 3 -
	1/1 0 0 0 1/2


Game rules
----------

Tankodes start:

* at an arbitrary position;
* with no charge;
* with base, turret and radar aligned.

Then:

* charge increases by `1/60` each Tick;
* base   turns at the maximum speed of `1/xxx` turns per tick (`1/xxx` Hz);
* turret turns at the maximum speed of `1/xxx` turns per tick (`1/xxx` Hz);
* radar  turns at the maximum speed of `1/xxx` turns per tick (`1/xxx` Hz).

The turret is mounted on the base, if the base turns, the turret turns as well.
The radar is mounted on the turret, if the turret turns, the radar turns as
well.

Tankodes have to keep track of their own charge, base, turret and radar
alignment.  This is easier than it sounds.
