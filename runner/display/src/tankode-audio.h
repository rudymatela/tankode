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
