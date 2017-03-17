#include <tankode.h>

struct tankode_out sitting_duck(struct tankode_in in)
{
	struct tankode_out out = {0., 0., 0., 0., 0.};
	return out;
}

int main()
{
	struct tankode_id id;
	id.name   = "sitting-duck";
	id.body   = "yellow8";
	id.track  = "orange4";
	id.gun    = "orange5";
	id.bullet = "orange5";
	id.radar  = "grey9";
	id.scan   =	"orange2";
	tankode_run(id, sitting_duck);
	return 0;
}
