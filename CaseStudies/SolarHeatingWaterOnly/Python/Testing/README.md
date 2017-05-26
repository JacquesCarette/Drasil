This directory contains all of the source files for testing of SWHS. The testing uses Python's unittest module.

compareC is a directory containing output files, from the C implementation, used by compareCTest.py, and their corresponding input files.

compareFortran is a directory containing output files, from the Fortran implementation, used by compareFortranTest.py, and their corresponding input files.

compareMatlab is a directory containing output files, from the Matlab implementation, used by compareMatlabTest.py, and their corresponding input files.

inputFiles is a directory containing the input files for the Python implementation, which are used to generate output files for compareFortranTest.py, compareMatlabTest.py, and compareCTest.py.

invalidInput is a directory containing input files used by invalidInputTest.py.

unrecommendedInput is a directory containing input files used by unrecommendedInputTest.py

compareCTest.py contains tests that compare the output from this Python implementation to the output from the C implementation. The output files referred to in these tests, along with their corresponding input files, can be found in the compareC directory. These tests make use of PCM_Error.py.

compareFortranTest.py contains tests that compare the output from this Python implementation to the output from the original Fortran implementation. The output files referred to in these tests, along with their corresponding input files, can be found in the compareFortran directory. These tests make use of PCM_Error.py.

compareMatlabTest.py contains tests that compare the output from this Python implementation to the output from the Matlab implementation. The output files referred to in these tests, along with their corresponding input files, can be found in the compareMatlab directory. These tests make use of PCM_Error.py.

energyTest.py contains unit tests for the Energy Module of SWHS, which is implemented in energy.py.

eventTest.py contains unit tests for the Event Module of SWHS, which is implemented in event.py.

invalidInputTest.py contains unit tests for the verify_valid function of the Input Verification Module, which is implemented in verify_params.py. The input files referred to by these tests can be found in the invalidInput directory.

linterp.py contains a simple linear interpolation function. This source file does not contain any tests, but is used by PCM_Error.py, which is used by the tests in compareFortranTest.py, compareMatlabTest.py, and compareCTest.py.

loadParamsTest.py contains unit tests for the Input Format Module of SWHS, which is implemented in load_params.py.

PCM_Error.py contains functions required for calculating relative error between output files from the Python, C, Matlab, and Fortran implementations of SWHS. This source file does not contain any tests, but is used by tests in compareFortranTest.py, compareMatlabTest.py, and compareCTest.py. The functions in this file also make use of linterp.py.

run_all_tests.py contains the test suite of all of the tests in this directory, as well as the command for running them.

unrecommendedInputTest.py contains unit tests for the verify_recommended function of the Input Verification Module, which is implemented in verify_params.py. The input files referred to by these tests can be found in the unrecommendedInput directory.

verifyOutputTest.py contains unit tests for the Output Verification Module, which is implemented in verify_output.py.

These tests can be run by returning to the Python directory and running the following command:

`make test`
