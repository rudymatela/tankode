#include "io.h"
#include <stdio.h>
#include <string.h>
#include <math.h>


const float NaN = 0./0.;
int isNaN(float f) {return f != f;}


/* TODO: move printing to debug.h and debug.c?? */
/* for debug only: */
void print_state(struct state s)
{
	int i, j;
	printf("tick %i\n",s.tick);
	printf("field %f %f\n",s.field.width,s.field.height);
	printf("ntanks %i\n",s.n_tanks);
	printf("nobstacles %i\n",s.field.n_obstacles);
	for (i=0; i<s.field.n_obstacles; i++)
		printf("obstacle %.1f %.1f %.1f %.1f %.1f %.1f\n",
		  s.field.obstacles[i].x1,
		  s.field.obstacles[i].y1,
		  s.field.obstacles[i].x2,
		  s.field.obstacles[i].y2,
		  s.field.obstacles[i].x3,
		  s.field.obstacles[i].y3);
	for (i=0; i<s.n_tanks; i++)
		printf("tank %-12s  %.2f %.2f %.2f  %.2f %.2f %.2f  %.2f %.2f %.2f\n",
		  s.tanks[i].name,
		  s.tanks[i].base_colour.r,
		  s.tanks[i].base_colour.g,
		  s.tanks[i].base_colour.b,
		  s.tanks[i].turret_colour.r,
		  s.tanks[i].turret_colour.g,
		  s.tanks[i].turret_colour.b,
		  s.tanks[i].radar_colour.r,
		  s.tanks[i].radar_colour.g,
		  s.tanks[i].radar_colour.b);
	for (i=0; i<s.n_tanks; i++) {
		print_tankpos(s.tanks[i]);
		for (j=0; i<s.tanks[i].n_bullets; j++)
			print_bullet(s.tanks[i].bullets[j]);
	}
	printf("\n");
}


void print_bullet(struct bullet b)
{
	printf("bullet %.2f %.2f %.2f %.2f\n", b.charge, b.x, b.y, b.dir);
}


/* for debug only: */
void print_tankpos(struct tank t)
{
	printf("tankpos %.3f %.3f %.3f %.3f %.3f\n",
		t.x, t.y, t.base_dir, t.turret_dir, t.radar_dir);
}


int read_tankpos(struct tank *t)
{
	/* TODO: golf this function (read_tankpos) */
	t->x          = get_ratio();        if (isNaN(t->x))          return 0;
	t->y          = get_ratio();        if (isNaN(t->y))          return 0;
	t->base_dir   = get_ratio()*2*M_PI; if (isNaN(t->base_dir))   return 0;
	t->turret_dir = get_ratio()*2*M_PI; if (isNaN(t->turret_dir)) return 0;
	t->radar_dir  = get_ratio()*2*M_PI; if (isNaN(t->radar_dir))  return 0;
	t->integrity  = get_ratio();        if (isNaN(t->integrity))  return 0;
	t->power      = get_ratio();        if (isNaN(t->power))      return 0;
	t->scan_dist  = get_ratio();        if (isNaN(t->scan_dist))  return 0;
	return 1;
}


int read_what(char w[MAX_WHAT])
{
	return 1 == scanf(" %" MAX_WHAT_S "s", w);
}


struct state get_initial_state()
{
	int i = 0, j = 0;
	char w[MAX_WHAT];
	struct state s;
	s.tick = -1;

	if (!read_what(w) || strcmp(w,"field") != 0)
		goto err;
	s.field = get_field();

	while (read_what(w)) {
		if (strcmp(w,"obstacle") == 0) { s.field.obstacles[j++] = get_obstacle(); continue; }
		if (strcmp(w,"tank")     == 0) { s.tanks[i++]           = get_tank();     continue; }
		if (strcmp(w,"tick")     == 0) break;
		goto err;
	}
	s.field.n_obstacles = j;
	s.n_tanks           = i;
err:
	fprintf(stderr,"error (get_initial_state): see source\n");
	return s;
}


int read_tick(struct state *s)
{
	int i = -1;
	char w[MAX_WHAT] = "";
	scanf(" %d",&s->tick);
	while (read_what(w)) {
		if (strcmp(w,"tankpos") == 0) { read_tankpos(&s->tanks[++i]); s->tanks[i].n_bullets=0;       continue; }
		if (strcmp(w,"bullet")  == 0) { s->tanks[i].bullets[s->tanks[i].n_bullets++] = get_bullet(); continue; }
		if (strcmp(w,"tick")    == 0) break;
		goto err;
	}
	if (i+1 != s->n_tanks)
		goto err;
	return 1;
err:
	fprintf(stderr,"error (read_tick): read %i tanks, but should be %i\n", i, s->n_tanks);
	fprintf(stderr,"error (read_tick): the last thing I read was `%s`\n",w);
	return 0;
}


struct field get_field()
{
	struct field f = {NaN, NaN};
	if (isNaN(f.width  = get_ratio())) return f;
	if (isNaN(f.height = get_ratio())) return f;
	f.colour  = get_colour();
	f.obstacle_colour = get_colour();
	return f;
}


struct obstacle get_obstacle()
{
	struct obstacle o = {NaN, NaN, NaN, NaN, NaN, NaN};
	if (isNaN(o.x1 = get_ratio())) return o;
	if (isNaN(o.y1 = get_ratio())) return o;
	if (isNaN(o.x2 = get_ratio())) return o;
	if (isNaN(o.y2 = get_ratio())) return o;
	if (isNaN(o.x3 = get_ratio())) return o;
	if (isNaN(o.y3 = get_ratio())) return o;
	return o;
}


struct bullet get_bullet()
{
	struct bullet b;
	b.charge = get_ratio();
	b.x      = get_ratio();
	b.y      = get_ratio();
	b.dir    = get_ratio() * 2 * M_PI;
	return b;
}


struct tank get_tank()
{
	struct tank t;
	scanf(" %" MAX_TANK_NAME_S "s", t.name);
	t.track_colour  = get_colour();
	t.base_colour   = get_colour();
	t.turret_colour = get_colour();
	t.radar_colour  = get_colour();
	t.bullet_colour = get_colour();
	t.scan_colour   = get_colour();
	t.scan_colour.a = 1./3.;
	return t;
}

float get_ratio()
{
	int n,d,r = scanf(" %d/%d",&n,&d);
	return r == 2 ? (float)n / (float)d : NaN;
}

struct colour get_colour()
{
	struct colour c;
	c.r = get_ratio(),
	c.g = get_ratio(),
	c.b = get_ratio();
	c.a = 1.;
	return c;
}
