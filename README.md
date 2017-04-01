Tankode
=======

![Tankode logo](https://github.com/rudymatela/tankode/releases/download/v0.0.1/tankode-logo-colour-400px.png)

Tankode is a programming action game.  The goal is to write a program to
control a robot: a tankode.  Tankodes battle in a virtual arena: they must fire
at enemies and dodge bullets.  The last tankode standing wins.

![Tankode demo](https://github.com/rudymatela/tankode/releases/download/v0.0.1/tankode-400x200.gif)

The game has a simple API for Haskell, C, Ruby and Bash.  Example tankodes are
provided with the game package.

The game has been designed to be programming-language independent.  Tankodes
read-and-write to-and-from standard input-output following the very simple
[Tankode Protocol].  So, it is very easy to add new language bindings.

The game is open-source, and licenced under the LGPLv2.


Tankode is under active development
-----------------------------------

Tankode is under active development:

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
	$ make run

See the [Makefile] to try to learn how to run your own examples and custom made
tankodes.  Future versions should have better instructions and steps than this.



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

[Robocode]: http://robocode.sourceforge.net/
