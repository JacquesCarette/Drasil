This directory contains all of the source files for testing of SWHS. The testing of SWHS is carried out using the Unity C testing framework by Throw the Switch.

FaultyInput is a directory containing input files used by invalidInputTest.c.

compareFortran is a directory containing output files, from the Fortran implementation, used by compareFortranTest.c, and their corresponding input files.

compareMatlab is a directory containing output files, from the Matlab implementation, used by compareMatlabTest.c, and their corresponding input files.

compareSolvers is a directory containing output files, from the ARKode implementation, used by compareSolversTest.c, and their corresponding input files.

inputFiles is a directory containing the input files for the C implementation which are used to generate output files for compareFortranTest.c, compareMatlabTest.c, and compareSolverTest.c.

test_runners is a directory containing the source files that will run all of the tests contained in this directory.

unrecommendedInput is a directory containing input files used by unrecommendedInputTest.c

PCM_Error.c contains functions required for calculating relative error between output files from the C, Matlab, and Fortran implementations of SWHS. This source file does not contain any tests, but is used by some of the other tests in this directory. The functions contained in this file make use of linterp.c.

compareFortranTest.c contains tests that compare the output from this C implementation to the output from the original Fortran implementation. The output files referred to in these tests, along with their corresponding input files, can be found in the compareFortran directory. These tests make use of PCM_Error.c.

compareMatlabTest.c contains tests that compare the output from this C implementation to the output from the Matlab implementation. The output files referred to in these tests, along with their corresponding input files, can be found in the compareMatlab directory. These tests make use of PCM_Error.c.

compareSolversTest.c contains tests that compare the output from this C implementation, which uses the CVODE solver, to the output from mainark.c, which uses the ARKODE solver instead of CVODE. The output files referred to in these tests, along with their corresponding input fules, can be found in the compareSolvers directory. These tests make use of PCM_Error.c.

energyTest.c contains unit tests for the Energy Module of SWHS, which is implemented in energy1.c, energy2.c, and energy3.c. 

invalidInputTest.c contains unit tests for the verify_valid function of the Input Verification Module, which is implemented in verify_params.c. The input files referred to by these tests can be found in the FaultyInput directory.

linterp.c contains a simple linear interpolation function. This source file does not contain any tests, but is used by PCM_Error.c, which is used by some of the tests in this directory.

loadParamsTest.c contains unit tests for the Input Format Module of SWHS, which is implemented in load_params.c.

mainark.c is identical to main.c except that it uses the ARKODE solver instead of CVODE. It was used to create the output files with prefix "ark" in the compareSolvers directory. To use this version of the software, the makefile in the CCode directory should be edited to refer to mainark.c instead of main.c.

unity.c and unity_fixture.c are the Unity testing framework source files.

unrecommendedInputTest.c contains unit tests for the verify_recommended function of the Input Verification Module, which is implemented in verify_params.c. The input files referred to by these tests can be found in the unrecommendedInput directory.

verifyOutputTest.c contains unit tests for the Output Verification Module, which is implemented in verify_output.c.

These tests can be run by returning to the CCode directory and running the following command:

`make test`