Changelog
#########

v0.3.0 (2017-07-25)
-------------------

* Bug in strategy K74RXX fixed and the strategy added back into the compiled
  output

v0.2.0 (2017-07-24)
-------------------

* Functions are split between those for strategies and those for running the
  original tournament and separate folders created for each category

* The makefile compiles the strategies into a separate shared object file
  whilst the tournament source is compiled into an executable that links to
  the shared object file

v0.1.0 (2017-07-20)
-------------------

Original downloaded code with the following changes necessary in order to
ensure that it will compile using gfortran version 7.1.0:

* Indentation in the downloaded html corrected to ensure the Fortran is valid

* Html codes '&lt;' and '&gt;' replaced with '<' and '>'

* The use of '=' in conditional expressions replaced with '=='

* Lines longer than 72 characters split into multiple lines with continuation
  characters added

* JSeconds function replaced with secnds

* DATE and TIME functions replaced with date_and_time

* Use of a real variable as an arrary index in strategy K85R corrected

* Removal of strategy K74RXX from the compiled executable due to an
  unresolved bug that prevents compilation

And splitting of the original single file into separate files for each
function.

The makefile in this release compiles the entire source code to a single
executable.