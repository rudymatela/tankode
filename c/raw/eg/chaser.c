#include <tankode.h>

struct tankode_out chaser(struct tankode_in in)
{
	struct tankode_out out = {0., 0., 0., 0., 0.};
	if (in.scanned_enemy) {
		out.shoot = 1.;
		out.accel = in.enemy > 1 ? +1 : in.enemy < 1 ? -1 : 0;
	} else {
		out.body  = +1.;
		out.accel = in.speed > 0 ? -1 : in.speed < 0 ? +1 : 0;
	}
	return out;
}

int main()
{
	struct tankode_id id;
	id.name   = "chaser";
	id.track  = "blue2";
	id.body   = "blue4";
	id.gun    = "grey7";
	id.radar  = "grey1";
	id.bullet = "grey9";
	id.scan   = "blue1";
	tankode_run(id, chaser);
	return 0;
}
