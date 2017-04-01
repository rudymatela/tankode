/* tankode.h -- a library for implementing tankodes in C.
 *
 * Copyright (C) 2017  Rudy Matela
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
 */
#ifndef _TANKODE_H
#define _TANKODE_H

#include "constants.h"

struct tankode_id {
	char *name;
	char *track;
	char *body;
	char *gun;
	char *radar;
	char *bullet;
	char *scan;
};

struct tankode_in {
	double integrity;
	double speed;
	double enemy;
	double wall;
	int scanned_enemy;
	int scanned_wall;
};

struct tankode_out {
	double
	accel,
	body,
	gun,
	radar,
	shoot;
};

void tankode_run(struct tankode_id id, struct tankode_out tankode(struct tankode_in in));

#endif /* _TANKODE_H */
