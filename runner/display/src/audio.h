/* audio.h -- a simple wrapper for OpenAL
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
#ifndef _AUDIO_H
#define _AUDIO_H

typedef unsigned int sound_t;

void audio_init(float x, float y, float z);
void audio_finalize();
void play_sound(sound_t sound, float pitch, float gain, float x, float y);
sound_t load_sound(char filepath[]);
void unload_sound(sound_t sound);

#endif /* _AUDIO_H */
