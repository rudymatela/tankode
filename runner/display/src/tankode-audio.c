/* tankode-audio.c -- play sounds for the Tankode game using OpenAL
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "audio.h"
#include "tankode-audio.h"

int tankode_initOK = 0;

enum sound {
	shot,
	tank_explosion, /* bullet x tank */
	wall_explosion, /* bullet x wall */
	tank_bump,      /* tank x tank */
	wall_bump,      /* tank x wall */
	tank_disable,
	n_sounds
};

char *sound_files[] =
	{ "shot.wav"
	, "explosion-on-tank.wav"
	, "explosion-on-wall.wav"
	, "bump-on-tank.wav"
	, "bump-on-wall.wav"
	, "disable.wav"
	, NULL
	};

sound_t sounds[n_sounds];

static void load_sounds();
static void unload_sounds();

static void load_sounds(char dirname[])
{
	int i;
	int r;
	char path[PATH_MAX];
	for (i=0; i<n_sounds; i++) {
		r = snprintf(path,PATH_MAX,"%s/../sounds/%s",dirname,sound_files[i]);
		if (r >= PATH_MAX) {
			fprintf(stderr, "error (load_sounds): dirname too large\n");
			exit(1);
		}
		sounds[i] = load_sound(path);
	}
}

static void unload_sounds()
{
	int i;
	for (i=0; i<n_sounds; i++)
		unload_sound(sounds[i]);
}

void tankode_audio_init(char dirname[], float x, float y, float z)
{
	audio_init(x, y, z);
	load_sounds(dirname);
	tankode_initOK = 1;
}

void tankode_audio_finalize()
{
	if (!tankode_initOK)
		return;
	unload_sounds();
	audio_finalize();
}

static void play_tankode_sound(enum sound s, float x, float y, float intensity)
{
	float pitch = 1. + .83*(1. - intensity);
	float gain  = .5 + 1.*intensity;
	if (tankode_initOK && intensity >= 1./12.)
		play_sound(sounds[s], pitch, gain, x, y);
}

void play_shot(float x, float y, float intensity)
{
	play_tankode_sound(shot,x,y,intensity);
}

void play_tank_explosion(float x, float y, float intensity)
{
	play_tankode_sound(tank_explosion,x,y,intensity);
}

void play_wall_explosion(float x, float y, float intensity)
{
	play_tankode_sound(wall_explosion,x,y,intensity);
}

void play_tank_bump(float x, float y, float intensity)
{
	play_tankode_sound(tank_bump,x,y,intensity);
}

void play_wall_bump(float x, float y, float intensity)
{
	play_tankode_sound(wall_bump,x,y,intensity);
}

void play_tank_disable(float x, float y)
{
	play_tankode_sound(tank_disable,x,y,1.);
}
