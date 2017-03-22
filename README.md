Tankode
=======

![tankode-logo](https://github.com/rudymatela/tankode/releases/download/v0.0.1/tankode-logo-colour.svg)

Tankode is a programming action game.  The goal is to write a program to
control a robot: a tankode.  Tankodes battle in a virtual arena: they must fire
at enemies and dodge bullets.  The last tankode standing wins.

![tankode-demo](https://github.com/rudymatela/tankode/releases/download/v0.0.1/tankode-400x200.gif)

The game has a simple API for Haskell, C and Bash.  Example tankodes are
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
* _Documentation is not great for now._

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


[Tankode Protocol]: doc/tankode-protocol.md
[Tankode Display Protocol]: doc/tankode-display-protocol.md
[TODO list]: TODO.md

[Robocode]: http://robocode.sourceforge.net/
