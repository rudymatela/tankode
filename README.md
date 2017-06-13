Tankode
=======

![Tankode logo](https://user-images.githubusercontent.com/3999598/27097071-fdf62eca-506a-11e7-9bf8-f15c06e6df20.png)

Tankode is a programming action game.  The goal is to write a program to
control a robot: a tankode.  Tankodes battle in a virtual arena: they must fire
at enemies and dodge bullets.  The last tankode standing wins.

![Tankode demo](https://user-images.githubusercontent.com/3999598/27097041-e832814c-506a-11e7-9b2d-3b28e1f7915b.gif)

The game has a simple API for Haskell, C, Ruby and Bash.  Example tankodes are
provided with the game package.

The game has been designed to be programming-language independent.  Tankodes
read-and-write to-and-from standard input-output following the very simple
[Tankode Protocol].  So, it is very easy to add new language bindings.

The game is open-source, and licensed under the [LGPLv2.1].


Tankode is a work in progress
-----------------------------

Tankode is work in progress under active development:

* __Expect it to crash often for now;__
* _Linux-only at the moment;_
* _Documentation is not great for now;_
* Code needs a major cleanup.

See the [TODO list] for more details.


Tankode compared to Robocode
----------------------------

The Tankode game is very similar to, and inspired by, [Robocode].
Differences include:

* Robocode Java and .NET only, Tankode is designed to be language independent;
* game physics;
* game graphics;
* battlefields are not necessarily square, and can have obstacles;
* tankodes cannot query their their `x` and `y` coordinates,
  if needed, those need to be calculated based on initial position and movements.


Pre-requisites
--------------

To compile and run Tankode, you'll need:

* [GCC], [glibc], [GNU Make]
* [mesa], [glu], [freeglut]
* [openal], [freealut]
* [GHC], [cabal-install], [haskell-cmdargs]
* [haskell-LeanCheck]

You probably have half of the above already installed.

On [Arch Linux], the following should be enough to install everything:

	$ pacman -S gcc glibc make
	$ pacman -S mesa glu freeglut
	$ pacman -S openal freealut
	$ pacman -S ghc cabal-install haskell-cmdargs
	$ cabal install leancheck

On [Ubuntu] and [Debian] variants, alternate calls to `apt-get install` should be
enough.

In the future, we should provide packages for installation of Tankode and its
language bindings on several systems (e.g.: Arch Linux & Ubuntu), so that users
need not to worry about dependencies.


Running the Tankode default example
-----------------------------------

For now, to compile and run Tankode, run the following commands:

	$ git clone https://github.com/rudymatela/tankode
	$ cd tankode
	$ make
	$ make -C runner/display/sounds download
	$ make -C runner/display/sounds unzip-and-link
	$ make run

See the [Makefile] to try to learn how to run your own examples and custom made
tankodes.  Future versions should have better instructions and steps than this.


Credits
-------

[Sound effects] were kindly provided by [Little Robot Sound Factory] under the
[CC-BY 3.0] license.  Little Robot does not necessarily endorses their use in
Tankode.


[Tankode Protocol]: doc/tankode-protocol.md
[Tankode Display Protocol]: doc/tankode-display-protocol.md
[TODO list]: TODO.md
[Makefile]: Makefile

[Arch Linux]:        https://archlinux.org
[Ubuntu]:            https://ubuntu.com
[Debian]:            https://debian.org
[GCC]:               https://gcc.gnu.org/
[glibc]:             https://www.gnu.org/software/libc/
[GNU Make]:          https://www.gnu.org/software/make/
[mesa]:              https://mesa3d.org/
[glu]:               http://freeglut.sourceforge.net/
[freeglut]:          http://freeglut.sourceforge.net/
[openal]:            https://openal.org
[freealut]:          https://github.com/vancegroup/freealut
[GHC]:               https://www.haskell.org/ghc/
[cabal-install]:     https://www.haskell.org/cabal/
[haskell-cmdargs]:   https://hackage.haskell.org/package/cmdargs
[haskell-LeanCheck]: https://hackage.haskell.org/package/leancheck

[LGPLv2.1]:          https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html

[Little Robot Sound Factory]: http://www.littlerobotsoundfactory.com
[Sound effects]: https://opengameart.org/content/8-bit-sound-effects-library
[CC-BY 3.0]:     https://creativecommons.org/licenses/by/3.0/

[Robocode]: http://robocode.sourceforge.net/
