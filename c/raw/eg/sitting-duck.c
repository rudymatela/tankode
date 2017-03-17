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
		"orange4",
		"yellow8",
		"orange5",
		"grey9",
		"orange5",
		"orange2",
	};
	tankode_run(id, sitting_duck);
	return 0;
}
