This folder contains the C language implementation of the SWHS software.

MISNotes.txt contains notes about the mapping of the MIS to the C implementation.

This implementation uses the CVODE 2.8.2 solver by Sundials to solve the ODEs in the problem.
It uses the gnuplot 5.0.3 software to generate the graphs of the results.

The software writes a file containing the output data and a PNG file containing the graphs of the water and PCM temperature and energy over the simulation time.

Before running the program, you will likely need to prepare your environment by installing dependenices, editing the makefile, and editing PATH variables. Instructions for different operating systems are below.

__Preparations for Windows and Mac OS X__

The CVODE and gnuplot libraries must be installed.

To install CVODE:

- The CVODE 2.8.2 package can be found in the CVODE folder, or it can be downloaded at http://computation.llnl.gov/projects/sundials-suite-nonlinear-differential-algebraic-equation-solvers/sundials-software

- The package contains an installation guide with specific instructions depending on your system's environment

- Note that the installation requires CMake

To install gnuplot:

- The source files and MinGW self-installers can be found in the gnuplot folder, or downloaded at https://sourceforge.net/projects/gnuplot/files/

- If using a self-installer, simply follow the on screen prompts to install

- If building from source files, follow the instructions in the INSTALL file included in the download

The SWHS software is run using make. The makefile in this directory may need some modification to work properly. In order to ensure that the makefile will work, one of the following must be done:

Option 1:

- In this directory, create a "lib" subdirectory and fill it with the CVODE libraries.

- Add all of the header files from CVODE to the "include" directory that already exists in this directory.

Option 2:

- Edit the Makefile "LIB_DIRS" variable with the path to the CVODE libraries.

- Add to the Makefile "INC_DIR" variable using -I and the path to the CVODE include (header) files. Do not remove the path that already exists in this variable, as it refers to the "include" directory that contains the header files specific to SWHS.

NOTE: The header files in the CVODE package are separated into three folders ("cvode", "nvector", and "sundials"). They should be kept in these folders and the include path in the makefile should refer to the parent folder containing these three folders.

Regardless of which of the above options you choose, you must also add the paths to the CVODE and gnuplot libraries to your system's PATH variable.

__Preparations for Linux__

The CVODE and gnuplot libraries must be installed.

To install CVODE:

- The CVODE 2.8.2 package can be found in the CVODE folder, or it can be downloaded at http://computation.llnl.gov/projects/sundials-suite-nonlinear-differential-algebraic-equation-solvers/sundials-software

- The package contains an installation guide with specific instructions depending on your system's environment

- Note that the installation requires CMake, a working C compiler and the standard C libraries.

	- To install the standard C libraries, run `sudo apt-get install libc-dev`

To install gnuplot:

- Simply run `sudo apt-get install gnuplot` from an open terminal.

The SWHS software is run using make. In order to ensure that the makefile will work, one of the following must be done:

Option 1:

- In this directory, create a "lib" subdirectory and fill it with the CVODE libraries.

- Add all of the header files from CVODE to the "include" directory that already exists in this directory.

Option 2:

- Edit the Makefile "LIB_DIRS" variable with the path to the CVODE libraries.

- Add to the Makefile "INC_DIR" variable using -I and the path to the CVODE include (header) files. Do not remove the path that already exists in this variable, as it refers to the "include" directory that contains the header files specific to SWHS.

NOTE: The header files in the CVODE package are separated into three folders ("cvode", "nvector", and "sundials"). They should be kept in these folders and the include path in the makefile should refer to the parent folder containing these three folders.

Regardless of which of the above options you choose, you must also add the path to the CVODE libraries to your system's LD_LIBRARY_PATH variable, using the following line of code:

```export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/cvode/lib```


__Running the Program__

Once preparations have been successfully completed, the program can be run from this directory using the following command:

- `make`

The software testing for SWHS uses the Unity C testing framework. To run the tests on the software, execute the following command from this directory:

- `make test`

To remove all of the generated files from your directory after running the software, use the following command:

- `make clean`

If there are any failures when the tests are run, the input files used for testing will not be removed. If you would like to remove those files quickly, use the following command:

- `make inputClean`

__Troubleshooting For Windows__

If you get an error message when trying to build CVODE that the compiler can not be found, you may need to install Microsoft Visual Studio. The CVODE CMake file seems to search specifically for the MSVC compiler, but once it is found you will have the opportunity to change the setting to a different compiler.

If, when running SWHS, the CVODE libraries can not be found, even though they have been added to your PATH variable, the problem may be that the libraries were compiled with a different compiler than is used to run SWHS. Try reinstalling the libraries using the same compiler that the makefile calls when running SWHS.