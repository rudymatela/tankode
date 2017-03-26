#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "audio.h"
#include "tankode-audio.h"

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

void tankode_audio_init(char dirname[])
{
	audio_init();
	load_sounds(dirname);
}

void tankode_audio_finalize()
{
	unload_sounds();
	audio_finalize();
}

static void play_tankode_sound(enum sound s, float x, float y, float intensity)
{
	float pitch = 1. + .83*(1. - intensity);
	float gain  = .5 + 1.*intensity;
	if (intensity >= 1./12.)
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
