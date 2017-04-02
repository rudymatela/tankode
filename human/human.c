/* human.c -- allows humans to control tankodes using a joystick
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
 *
 *
 * This is a Hack.  This program makes the game run a bit slower: the delay to
 * sync the game logic program and the display program is introduced here.
 */
#include <SDL2/SDL.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <tankode.h>

int errxit(const char msg[])
{
	fprintf(stderr,msg);
	exit(1);
}

int init()
{
	SDL_SetHint(SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS, "1");
	/* NOTE: VIDEO only for SDL_PollEvent */
	if(SDL_Init(SDL_INIT_TIMER | SDL_INIT_VIDEO
	          | SDL_INIT_JOYSTICK | SDL_INIT_GAMECONTROLLER
			  | SDL_INIT_HAPTIC) < 0)
		errxit(SDL_GetError());
	atexit(SDL_Quit);

	if (!SDL_JoystickOpen(0)) /* TODO: allow selection of joy idx */
		errxit("unable to open joystick\n");
}

struct tankode_out human(struct tankode_in in)
{
	static int x=0, y=0;
	static int pressed[0x100] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	static int ticks = 0;
	int i;
	SDL_Event event;
	struct tankode_out out = {0., 0., 0., 0., 0.};
	while (SDL_PollEvent(&event)) switch(event.type) {
	case SDL_JOYHATMOTION:
		/* simulates axis event */
		x = 0;
		y = 0;
		if (event.jhat.value&SDL_HAT_UP)    y = -1;
		if (event.jhat.value&SDL_HAT_DOWN)  y =  1;
		if (event.jhat.value&SDL_HAT_LEFT)  x = -1;
		if (event.jhat.value&SDL_HAT_RIGHT) x =  1;
		break;

	case SDL_JOYAXISMOTION:
		switch (event.jaxis.axis) {
		case 0: x = event.jaxis.value / 256; break;
		case 1: y = event.jaxis.value / 256; break;
		}
		break;

	case SDL_JOYBUTTONDOWN:
		pressed[event.jbutton.button] = 1;
		break;

	case SDL_JOYBUTTONUP:
		pressed[event.jbutton.button] = 0;
		break;

	case SDL_QUIT:
		exit(1);

	default:
		fprintf(stderr, "Warning: Unhandled event type: %d\n", event.type);
	}
	if (y < 0)      out.accel = +1;
	else if (y > 0) out.accel = -1;
	else            out.accel =  0;
	if (x < 0)      out.body  = +1;
	else if (x > 0) out.body  = -1;
	else            out.body  =  0;
	if (pressed[0] || pressed[1] || pressed[2] || pressed[3]) out.shoot = 1;
	i = 1000 / 120 - (SDL_GetTicks() - ticks);
	fprintf(stderr,"%i %i %i %i (delay %i)\n",x,y,pressed[0],pressed[1], i);
	if (i > 0)
		SDL_Delay(i+1);
	ticks = SDL_GetTicks();
	return out;
}


int main()
{
	struct tankode_id id;
	id.name   = "human";
	id.track  = "red2";
	id.body   = "red4";
	id.gun    = "grey7";
	id.radar  = "grey1";
	id.bullet = "grey9";
	id.scan   = "red1";
	init();
	tankode_run(id, human);
}
