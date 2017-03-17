#include <tankode.h>

struct tankode_out left_turner(struct tankode_in in)
{
	struct tankode_out out = {0., 0., 0., 0., 0.};
	out.gun   = (2./3.) / 120.;
	out.shoot = in.enemy == in.enemy ? (2./3.) : 0.;
	return out;
}

int main()
{
	struct tankode_id id = {
		"left-turner",
		"orange4",
		"orange5",
		"orange6",
		"orange7",
		"orange8",
		"orange9"
	};
	tankode_run(id, left_turner);
	return 0;
}
