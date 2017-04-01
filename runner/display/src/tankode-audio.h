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
#ifndef _TANKODE_AUDIO_H
#define _TANKODE_AUDIO_H

void tankode_audio_init(char dirname[], float x, float y, float z);
void tankode_audio_finalize();
void play_shot(float x, float y, float intensity);
void play_tank_explosion(float x, float y, float intensity);
void play_wall_explosion(float x, float y, float intensity);
void play_tank_bump(float x, float y, float intensity);
void play_wall_bump(float x, float y, float intensity);
void play_tank_disable(float x, float y);

#endif /* _TANKODE_AUDIO_H */
