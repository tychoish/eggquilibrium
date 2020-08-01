=====================================
Eggquilibrium -- Reaching Egg Balance
=====================================

Overview
--------

This program helps you find egg equilibrium, for when you have left egg parts,
and want to use them all up. The goal is to find solutions using different
recipes that, when combined, use complete eggs without leaving parts. This is
a Hard Problem, and there are lots of different strategies:

- given a number egg parts in use, find recipes so that when complete you will
  have no left over egg parts. We call this "additive mode".

- given a number of available egg parts, find recipes to utilize *exactly* the
  egg parts that you have. We call this "utilization" mode.

- given a specific number of full eggs, find possible sets of recipes that are
  egg neutral. This is not yet implemented.

Development
-----------

The application is written in Common Lisp, and tested with SBCL, though it
should work well on other implementations. All dependencies are in Quicklisp,
and a makefile describes the build process, in general:

- install SBCL on your system.

- install Quicklisp on your system.

- place the Eggquilibrium directory in ``~/quicklisp/local-projects/``

- run ``make build`` to compile the application, or ``make test`` to run the
  tests.

Use
---

There is a command line interface, which is fully documented by the help
text.

Future Work
-----------

- add maximum number of eggs as a parameter to avoid giving solutions that are
  unhelpful, in additive mode.

- improvements to the data model, including stable and unique identifiers for
  recpies.

- produce a truly self contained binary that reads the data into the lisp
  image at build time.

- produce a web application to interact with the data and tool rather than
  requiring CLI interaction.
