#include <tankode.h>

struct tankode_out right_turner(struct tankode_in in)
{
	struct tankode_out out = {0., 0., 0., 0., 0.};
	out.gun   = -MAX_GUN_TURN;
    out.body  = -1;
    out.accel = +1;
	out.shoot = in.scanned_enemy ? (1./3.) : 0.;
	return out;
}

int main()
{
	struct tankode_id id;
	id.name   = "right-turner";
	id.radar  = "grey9";
	id.body   = "grey9";
	id.track  = "red4";
	id.gun    = "blue4";
	id.bullet = "blue8";
	id.scan   = "blue8";
	tankode_run(id, right_turner);
	return 0;
}
