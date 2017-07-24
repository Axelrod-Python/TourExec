.. image:: https://zenodo.org/badge/97823268.svg
   :target: https://zenodo.org/badge/latestdoi/97823268

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
compile successfully using a modern Fortran compiler. Also, the original single
file has been split into multiple files, with one for each strategy function,
so that those can be easily indexed within the
`Axelrod-Python Documentation <http://axelrod.readthedocs.io/en/stable/reference/overview_of_strategies.html#axelrod-s-second-tournament>`_.

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

Clone the repository, compile the Fortran code and install the executables::

	$ git clone https://github.com/Axelrod-Python/TourExec.git
	$ cd TourExec
	$ make
	$ make install

You should now be able to run the tournament::

	$ tourexec

On linux, you may get an error similar to::

	$ error while loading shared libraries: libstrategies.so:

which means that you need to add a directory to your $LD_LIBRARY_PATH
environment variable::

	$ export LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:/usr/local/lib

To remove both the executables from your machine, use::

	$ make uninstall 

Cleanup
-------

Compiling the executable file will create some intermediary object files in an
`obj` directory. These can safely be removed using::

	$ make clean

There is also a command to remove the `bin` directory as well as the object
files::

	$ make remove


Python
------

The strategy functions are available from within a shared object file,
`libstrategies.so` and can therefore be called from within Python. An example
of how to call TitForTat:

.. code:: python

    from ctypes import cdll, c_int, c_float, byref, POINTER
    # load the strategies library
    strategies = cdll.LoadLibrary('libstrategies.so')
    
    # Use the titfortat strategy from the library
    tft = strategies.ktitfortatc_
    
    # define the types of the function arguments and return value
    tft.argtypes = (
        POINTER(c_int), POINTER(c_int), POINTER(c_int), POINTER(c_int),
        POINTER(c_float))
    tft.restype = c_int
    
    # Variables for the argument to pass
    my_previous = c_int(0)  # 0 is cooperation, 1 is defection
    their_previous = c_int(1)
    move_number = c_int(1)
    my_score = c_int(0)
    their_score = c_int(0)
    noise = c_float(0)
    
    # Call the strategy passing the arguments by reference
    result = tft(
        byref(their_previous), byref(move_number), byref(my_score),
        byref(their_score), byref(noise), byref(my_previous))
    
    print(result)
