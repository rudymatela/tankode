/* io.h -- IO for the Tankode game
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
#ifndef _IO_H
#define _IO_H

#include "tankode.h"

void print_tankpos(struct tank);
void print_state(struct state);
void print_bullet(struct bullet);
int read_tankpos(struct tank *);
float get_ratio();
int read_tick(struct state *);
struct state get_initial_state();
struct field get_field();
struct obstacle get_obstacle();
struct colour get_colour();
struct tank get_tank();
struct bullet get_bullet();
struct explosion get_explosion();

#endif /* _IO_H */
