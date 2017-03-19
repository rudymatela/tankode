#ifndef _TANKODE_H
#define _TANKODE_H

/* hard limits for now (and for a long time) */
#define MAX_TANK_NAME    32
#define MAX_TANK_NAME_S "32"
#define MAX_WHAT         64
#define MAX_WHAT_S      "64"
#define MAX_TANKS       256
#define MAX_BULLETS      32
#define MAX_OBSTACLES   128

struct colour {
	float r,g,b,a;
};

struct bullet {
	float charge;
	float x, y;
	float dir;
	int exploded;
};

struct tank {
	/* ident */
	char name[MAX_TANK_NAME];
	struct colour
		track_colour,
		base_colour,
		turret_colour,
		radar_colour,
		bullet_colour,
		scan_colour;

	/* position */
	float x, y;
	float
		base_dir,
		turret_dir,
		radar_dir;

	/* status */
	float integrity;
	float power;
	float scan_dist;
	float heat;

	/* bullets */
	int n_bullets;
	struct bullet bullets[MAX_BULLETS];

	float flare_x, flare_y, flare_dir;
};

struct obstacle {
	float x1, y1;
	float x2, y2;
	float x3, y3;
};

struct field {
	float width, height;
	int n_obstacles;
	struct obstacle obstacles[MAX_OBSTACLES];
	struct colour colour, obstacle_colour;
};

struct state {
	int tick;
	struct field field;
	int n_tanks;    struct tank tanks[MAX_TANKS];
};

#endif /* _TANKODE_H */
