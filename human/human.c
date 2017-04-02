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
 * *THIS IS A WORK IN PROGRESS AND DOES NOT WORK YET*
 *
 * currently, it just print joystick events on the screen.
 *
 * I have two ideas of how to go about this:
 *   * have two processses communicating via shared memory (shmget, shmat, shmdt)
 *     one for polling the joystick, other for communicating with the server
 *   * have a single process, the reading from the server would act as the
 *     SDL_Delay
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

int main_joy()
{
	int x=0, y=0;
	SDL_Event event;
	for (;;) {
		SDL_Delay(10);

		while (SDL_PollEvent(&event)) switch(event.type) {
		case SDL_JOYHATMOTION:
			/* simulates axis event */
			x = 0;
			y = 0;
			if (event.jhat.value&SDL_HAT_UP)    y = -1;
			if (event.jhat.value&SDL_HAT_DOWN)  y =  1;
			if (event.jhat.value&SDL_HAT_LEFT)  x = -1;
			if (event.jhat.value&SDL_HAT_RIGHT) x =  1;
			fprintf(stderr,"%i %i\n",x,y);
			break;

		case SDL_JOYAXISMOTION:
			switch (event.jaxis.axis) {
			case 0: x = event.jaxis.value / 256; break;
			case 1: y = event.jaxis.value / 256; break;
			}
			/* for now, ignore */
			fprintf(stderr,"%i %i\n",x,y);
			break;

		case SDL_JOYBUTTONDOWN:
			fprintf(stderr,"%i pressed\n",event.jbutton.button);
			break;

		case SDL_JOYBUTTONUP:
			fprintf(stderr,"%i released\n",event.jbutton.button);
			break;

		case SDL_QUIT:
			goto exit;

		default:
			fprintf(stderr, "Warning: Unhandled event type: %d\n", event.type);
		}
	}

exit:
	return 0;
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
	/*tankode_run(id, human);*/
	main_joy();
}
