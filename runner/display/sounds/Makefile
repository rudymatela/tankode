# Makefile

all:
	echo "run: $ make download && make unzip-and-link"

download:
	echo "upstream: https://opengameart.org/content/8-bit-sound-effects-library"
	wget "https://opengameart.org/sites/default/files/8-Bit%20Sound%20Library.zip"
	mv "8-Bit Sound Library.zip" 8-bit-sound-library.zip

unzip-and-link:
	rm -rf 8-bit-sound-library
	unzip 8-bit-sound-library.zip -x "__MACOSX/*" -x "*.DS_Store" -x "*/Mp3/*"
	mv "8-Bit Sound Library" 8-bit-sound-library
	ln -sf 8-bit-sound-library/Wav/Shoot_01.wav     shot.wav
	ln -sf 8-bit-sound-library/Wav/Hit_01.wav       bump-on-tank.wav
	ln -sf 8-bit-sound-library/Wav/Hit_02.wav       bump-on-wall.wav
	ln -sf 8-bit-sound-library/Wav/Explosion_02.wav explosion-on-tank.wav
	ln -sf 8-bit-sound-library/Wav/Explosion_04.wav explosion-on-wall.wav
	ln -sf 8-bit-sound-library/Wav/Explosion_00.wav disable.wav
