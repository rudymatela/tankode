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
struct bullet get_explosion();

#endif /* _IO_H */
