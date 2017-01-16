# webshell

The idea is to produce a web-based shell environment, using js-of-ocaml. The
goal is to explore options for interacting with a system without the need for
traditional a traditional terminal-based shell.

However, for backward compatibility there will need to be support for both. So
the first step will be a terminal emulator.

A lot of interesting shell "add ons" can be supported by the terminal emulator,
too. For instance, iTerm2 apparently does lots of cool things:
https://iterm2.com/features.html. The history information it collects seems
especially interesting.

## Terminal Emulation

