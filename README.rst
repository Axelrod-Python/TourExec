TourExec
========

This repository contains the software associated with
`Robert Axelrod <http://www-personal.umich.edu/%7Eaxe/>`_'s book `The Complexity of
Cooperation: Agent-Based Models of Competition and Collaboration
<http://press.princeton.edu/titles/6144.html>`_ (Princeton University Press).

The code was originally published by the
`University of Michigan Center for the Study of Complex Systems <http://lsa.umich.edu/cscs/>`_
and is now available from
`Robert Axelrod's personal website <http://www-personal.umich.edu/~axe/research/Software/CC/CC2.html>`_.

The original code has been modified in this repository to ensure that it will
compile successfully using a modern Fortran compiler.

Prerequisites
-------------

You will need the `make <https://www.gnu.org/software/make/>`_ and
`gfortran <https://gcc.gnu.org/fortran/>`_ tools installed on your system
and available from your command line.

To check if you already have them installed::

	$ make -v

Should return something similar to::

	$ GNU Make 3.81
	$ Copyright (C) 2006  Free Software Foundation, Inc.
	$ This is free software; see the source for copying conditions.
	$ There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
	$ PARTICULAR PURPOSE.

and::

	$ gfortran -v

should return something similar to::

	$ Using built-in specs.
	$ Thread model: posix
	$ gcc version 7.1.0

Installation
------------

Clone the repository and compile the Fortran code::

	$ git clone https://github.com/Axelrod-Python/TourExec.git
	$ cd TourExec
	$ make

You should now have a `bin` directory containing the the executable file,
`tourexec`, which you can run::

	$ cd bin
	$ ./tourexec

Cleanup
-------

Compiling the executable file will create some intermediary object files in an
`obj` directory. These can safely be removed using::

	$ make clean

There is also a command to remove the executable itself as well as the object
files::

	$ make remove
