/* tankode.c -- a library for implementing tankodes in C.
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
#include "tankode.h"
#include <stdio.h>
#include <string.h>
#include <math.h>

const float NaN = 0.0 / 0.0;

static double get_ratio();
static void print_ratio(double);
static void print_incdec(double);
static int get(struct tankode_in *in);
static void put(struct tankode_out out);

void tankode_run(struct tankode_id id, struct tankode_out tankode(struct tankode_in in))
{
	struct tankode_in  in;
	struct tankode_out out;
	setlinebuf(stdin);
	setlinebuf(stdout);
	printf("%s %s %s %s %s %s %s\n",
	  id.name,id.track,id.body,id.gun,id.radar,id.bullet,id.scan);
	while (get(&in))
		put(tankode(in));
}

static double get_ratio()
{
	int n,d,r = scanf(" %d/%d",&n,&d);
	return r == 2 ? (double)n / (double)d : NaN;
}

static void print_ratio(double f)
{
	printf("%i/360", (int)round(f*360));
}

static int get(struct tankode_in *in)
{
	char w[0x100];
	in->integrity = get_ratio();
	in->speed = get_ratio();
	in->enemy = get_ratio();
	in->wall = get_ratio();
	in->scanned_enemy = in->enemy == in->enemy; /* != NaN */
	in->scanned_wall  = in->wall  == in->wall;  /* != NaN */
	return in->integrity > 0.000000001;
}

static void print_incdec(double f)
{
	if (f >  0) putchar('+');
	if (f <  0) putchar('-');
	if (f == 0) putchar('=');
}

static void put(struct tankode_out out)
{
	print_incdec(out.accel); printf(" ");
	print_incdec(out.body);  printf(" ");
	print_ratio(out.gun);   printf(" ");
	print_ratio(out.radar); printf(" ");
	print_ratio(out.shoot); printf("\n");
}
