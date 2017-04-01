/* audio.c -- a simple wrapper for OpenAL
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
#include "audio.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alut.h>

/* arbitrary maximum number of sources */
#define MAX_SOURCES 64

unsigned int sources[MAX_SOURCES];
int current_source = 0;
int initOK = 0;

ALCdevice  *device;
ALCcontext *context;

static void init_sources();

static void errxit(char msg[]) { fprintf(stderr,"error: %s\n",msg); exit(1); }
static void errxitif(int c, char msg[]) { if (c) errxit(msg); }
static void alerrxit(char msg[]) { errxitif(alGetError() != AL_NO_ERROR, msg); }

void audio_init(float x, float y, float z)
{
	ALfloat orientation[] = {0, 0, -1, 0, 1, 0};

	device = alcOpenDevice(NULL);
	errxitif(!device, "open device");

	context = alcCreateContext(device, NULL);
	errxitif(!alcMakeContextCurrent(context),"create context"); alerrxit("create context");

	alListener3f(AL_POSITION, x, y, z);        alerrxit("listener position");
	alListener3f(AL_VELOCITY, 0, 0, 0);        alerrxit("listener velocity");
	alListenerfv(AL_ORIENTATION, orientation); alerrxit("listener orientation");
	alListenerf(AL_GAIN, z);                   alerrxit("listener gain");
	init_sources();
	initOK = 1;
}

static void init_sources()
{
	int i;
	alGenSources(MAX_SOURCES, sources); alerrxit("gen sources");
	for (i=0; i<MAX_SOURCES; i++) {
		alSource3f(sources[i], AL_VELOCITY, 0, 0, 0);   alerrxit("source velocity");
		alSourcei (sources[i], AL_LOOPING, AL_FALSE);   alerrxit("source looping");
	}
}

void audio_finalize()
{
	if (!initOK)
		return;

	alDeleteSources(MAX_SOURCES, sources);

	device = alcGetContextsDevice(context);
	alcMakeContextCurrent(NULL);
	alcDestroyContext(context);
	alcCloseDevice(device);
}

sound_t load_sound(char filepath[])
{
	unsigned int buffer;
	ALvoid *data;
	ALsizei size, freq;
	ALenum format;
	ALboolean loop = AL_FALSE;
	alutLoadWAVFile((signed char*)filepath, &format, &data, &size, &freq, &loop);
	                                                alerrxit("load wav file");
	alGenBuffers(1, &buffer);                       alerrxit("gen buffers");
	alBufferData(buffer, format, data, size, freq); alerrxit("buffer copy");
	return buffer;
}

void unload_sound(sound_t sound)
{
	alDeleteBuffers(1, &sound);
}

void play_sound(sound_t sound, float pitch, float gain, float x, float y)
{
	int state;
	int i = current_source;
	if (!initOK)
		return;
	current_source = (current_source + 1) % MAX_SOURCES;
	alGetSourcei(sources[i], AL_SOURCE_STATE, &state); alerrxit("get source state");
	if (state == AL_PLAYING) {
		alSourceStop(sources[i]);                 alerrxit("source stop");
	}
	alSourcef (sources[i], AL_PITCH, pitch);      alerrxit("source pitch");
	alSourcef (sources[i], AL_GAIN,  gain);       alerrxit("source gain");
	alSource3f(sources[i], AL_POSITION, x, y, 0); alerrxit("source position");
	alSourcei (sources[i], AL_BUFFER, sound);     alerrxit("buffer binding");
	alSourcePlay(sources[i]);                     alerrxit("alSourcePlay");
}
