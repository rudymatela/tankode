#include <tankode.h>

struct tankode_out sitting_duck(struct tankode_in in)
{
	struct tankode_out out = {0., 0., 0., 0., 0.};
	return out;
}

int main()
{
	struct tankode_id id = {
		"sitting-duck",
		{.2, .2, .2},
		{.3, .3, .3},
		{.4, .4, .4},
		{.5, .5, .5},
		{.7, .7, .7},
		{.9, .9, .9}
	};
	tankode_run(id, sitting_duck);
	return 0;
}
