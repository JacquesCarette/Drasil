Chipmunk Physics Simulator
==========================

This is a simple physics simulator based on a simplified version of the open-source Chipmunk2D game physics library.

Contents
--------
The contents of each directory are as follows:
- **example** -  contains a sample program made with Chipmunk.
- **include** - contains all the C header files necessary to compile Chipmunk.
- **lib** - will contain the Chipmunk static library. This directory and the library must be built wth make.
- **src** - contains all the C source files necessary to compile Chipmunk.
- **tests** - contains files for the C unit testing framework unity, and will contain unit tests for each Chipmunk module.

Build Instructions
------------------
Before writing/compiling any programs using Chipmunk, the static library must be built. This will be placed in the /lib directory, which will be created by make. To explicitly build the library, run:

    make lib

To build an executable of the sample program in /example, run:

    make prog

To run all tests, run:

    make test

By default, running 'make' will run 'make prog' (and will also run 'make lib' if the static library has not already been built). To build all targets at once, run:

    make all

Finally, to remove all auto-generated files (static library, sample program and test executables), run:

    make clean
