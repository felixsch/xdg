XDG - xdg standard implementation in haskell
============================================
The xdg freedesktop.org (formaly X Desktop Group) standards, which tries to specify desktop implementation details.

See [http://standards.freedesktop.org/](http://standards.freedesktop.org/) for more informations.

Build status: [![Build Status](https://secure.travis-ci.org/felixsch/xdg.png)](http://travis-ci.org/felixsch/xdg)

Why?
====

To make it easy to use the xdg standard in haskell programms. Theres has been only xdg-basedir, but this small library only covers
the basedir part of the standard.
This implementation aim's to complete the missing parts.


Install
=======

Currently this piece of software is far from really usable. That's why I don't publish it to hackage.

__To install__:

> git clone http://github.com/felixsch/xdg
> cd xdg
> cabal configure && cabal install

__This library depends currently on__:

* transformers
* parsec
* process
* filepath
* xml


TODO
====

There is a lot todo. The next step I'm working on is, to implement the recently used standard (a xml file)
