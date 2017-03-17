#include "tankode.h"
#include <stdio.h>
#include <string.h>

const float NaN = 0.0 / 0.0;

static double get_ratio();
static void print_ratio(float);
static void print_colour(struct colour);
static int get(struct tankode_in *in);
static void put(struct tankode_out out);

void tankode_run(struct tankode_id id, struct tankode_out tankode(struct tankode_in in))
{
	struct tankode_in  in;
	struct tankode_out out;
	printf("%s",id.name);
	printf(" "); print_colour(id.track);
	printf(" "); print_colour(id.body);
	printf(" "); print_colour(id.gun);
	printf(" "); print_colour(id.radar);
	printf(" "); print_colour(id.bullet);
	printf(" "); print_colour(id.scan);
	printf("\n");
	while (get(&in))
		put(tankode(in));
}

static double get_ratio()
{
	int n,d,r = scanf(" %d/%d",&n,&d);
	return r == 2 ? (double)n / (double)d : NaN;
}

static void print_ratio(float f)
{
	printf("%i/360", (int)(f*360));
}

static void print_colour(struct colour c)
{
	printf("#%02x%02x%02x",
		(int)(c.r*255.),
		(int)(c.g*255.),
		(int)(c.b*255.));
}

static int get(struct tankode_in *in)
{
	char w[0x100];
	in->integrity = get_ratio();
	in->speed = get_ratio();
	in->enemy = get_ratio();
	in->wall = get_ratio();
	return in->integrity > 0.000000001;
}

static void put(struct tankode_out out)
{
	print_ratio(out.accel); printf(" ");
	print_ratio(out.body);  printf(" ");
	print_ratio(out.gun);   printf(" ");
	print_ratio(out.radar); printf(" ");
	print_ratio(out.shoot); printf("\n");
}
