#include <tankode.h>

struct tankode_out left_turner(struct tankode_in in)
{
	struct tankode_out out = {0., 0., 0., 0., 0.};
	out.gun   = MAX_GUN_TURN;
	out.shoot = in.scanned_enemy ? (2./3.) : 0.;
	return out;
}

int main()
{
	struct tankode_id id;
	id.name   = "left-turner";
	id.body   = "red3";
	id.radar  = "red3";
	id.scan   = "red8";
	id.track  = "red3";
	id.bullet = "red9";
	id.gun    = "yellow7";
	tankode_run(id, left_turner);
	return 0;
}
