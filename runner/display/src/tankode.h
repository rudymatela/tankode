/* tankode-display.c -- objects of the displayer of the Tankode game
 *
 * Copyright (C) 2016, 2017  Rudy Matela
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * (development started by Rudy Matela on 2016-12-22 18:30)
 */
#ifndef _TANKODE_H
#define _TANKODE_H

/* hard limits for now (and for a long time) */
#define MAX_TANK_NAME    32
#define MAX_TANK_NAME_S "32"
#define MAX_WHAT         64
#define MAX_WHAT_S      "64"
#define MAX_TANKS       256
#define MAX_BULLETS      32
#define MAX_EXPLOSIONS   16
#define MAX_OBSTACLES   128
#define MAX_OBSTACLE_SIDES 60
#define EXPLOSION_DISCARD_AGE 20
/* in general, those bounds are not being checked,
 * they will in a future version */

struct colour {
	float r,g,b,a;
};

struct bullet {
	float charge;
	float x, y;
	float dir;
};

struct explosion {
	float charge;
	float x, y;
	int age;
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
	float previous_integrity;
	float integrity;
	float power;
	float scan_dist;
	float heat;

	/* bullets */
	int n_bullets;
	struct bullet bullets[MAX_BULLETS];

	/* explosions, represented as a circular list */
	int n_explosions;
	struct explosion explosions[MAX_EXPLOSIONS];

	float flare_x, flare_y, flare_dir;
};

struct obstacle {
	float x[MAX_OBSTACLE_SIDES];
	float y[MAX_OBSTACLE_SIDES];
	float n;
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
