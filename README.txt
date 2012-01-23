
  Duke Nukem 3d file format importers for MATLAB/Octave
  -----------------------------------------------------
              (C) 2010, 2012 Philipp Kutin

This package includes routines for importing data from Duke Nukem 3D and other
format-compatible BUILD games into MATLAB or Octave. The following formats are
supported:

* MAP version 7/8
* ART
* PAL
* VOC  (incomplete, only those blocks that Duke3d has implemented)
* GRP

All functions have the filename as first input argument. For the MAP, ART and
PAL importers, the raw data as returned from readgrp can be substituted for the
filename.

An example application is included which draws a map loaded with readmap using
the standard drawing functions. It can be used to produce printable overhead or
side views of your favourite map or to find those secret rooms you've missed.

Care has been taken to be compatible with Octave, but problems can still occur.

This whole stuff is licensed under GPL2.


-- Helixhorned



Changelog:
----------

2012-01-23:
 - added parseconsnd.m for parsing sound definitions from CON
 - faster loading of VOC files
