Curry Libraries
===============

This repository contains the standard libraries of
the Curry distributions PAKCS and KiCS2.

During the installation process of PAKCS or KiCS2, these libraries are
copied into the default `lib` directory of these installations.
The make files `Makefile_<SYSTEM>_install` are responsible for this
system-specific installation process.
The make files `Makefile_<SYSTEM>` will be copied as `Makefile`
into the `lib` directory.

These libraries are also available as Curry package `base`, see

    https://www-ps.informatik.uni-kiel.de/~cpm/pkgs/base.html

The file `VERSION` contains the version number corresponding
to this package. This is used by the Curry package manager
to avoid installing a dependency to the `base` package
when it is identical to the base version of the Curry distribution.
One can also use the command

    > curry --base-version

to check the `base` version of a Curry system.
