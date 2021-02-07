# Klaus

# What is this?

Klaus is a project for the "maturita" exam from IT at Gymnázium Jana Keplera and a WIP version of Bayesian text classification and search.

# Dependencies

SBCL, wish. Both available through APT.

# How to use?

Download files. Ensure that the path "portacle/all/quicklisp/setup.lisp" leads to a working installed Quicklisp from 2017-03-06 (not included for license reasons). Run SBCL in the folder "portacle". Run (load "load.lisp") to compile the program, (redownload \*classes-folder\*) to fetch website data (this will take several minutes), then (db). Press, in order, "rebuild text", "rebuild core text", "rebuild corpus". Wait for their completion. You will then have a functioning corpus. At that point, refer to the documentation.

# Caveat emptor

Not only is there no warranty or guarantee of a working state, there isn't even the expectation of a working state yet. There are known bugs and missing features in the code. The system hasn't yet been tested on any other computers than my own. 

Data provided may not be sufficient for well-defined classes, nor may the class system be entirely sound.
