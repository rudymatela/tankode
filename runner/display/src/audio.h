/* simple wrapper to openal */
#ifndef _AUDIO_H
#define _AUDIO_H

typedef unsigned int sound_t;

void audio_init(float x, float y, float z);
void audio_finalize();
void play_sound(sound_t sound, float pitch, float gain, float x, float y);
sound_t load_sound(char filepath[]);
void unload_sound(sound_t sound);

#endif /* _AUDIO_H */
