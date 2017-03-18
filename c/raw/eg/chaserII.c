#include <tankode.h>

struct tankode_out chaser(struct tankode_in in)
{
	static int turn_right = 0;
	static int seen_enemy = 0;
	struct tankode_out out = {0., 0., 0., 0., 0.};
	if (in.scanned_enemy) {
		seen_enemy = 1;
		out.shoot = 1.;
		out.accel = in.enemy > 1 ? +1 : in.enemy < 1 ? -1 : 0;
	} else {
		turn_right = seen_enemy ? !turn_right : turn_right;
		out.accel = in.speed > 0 ? -1 : in.speed < 0 ? +1 : 0;
		seen_enemy = 0;
	}
	out.body = turn_right ? -1 : +1;
	return out;
}

int main()
{
	struct tankode_id id;
	id.name   = "chaser";
	id.track  = "blue1";
	id.body   = "blue3";
	id.gun    = "grey7";
	id.radar  = "grey1";
	id.bullet = "grey9";
	id.scan   = "blue1";
	tankode_run(id, chaser);
	return 0;
}
