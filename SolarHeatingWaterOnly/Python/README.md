This folder contains the Python implementation of the SWHS software.

The software writes a file containing the output data and a PNG file containing the graphs of water and PCM temperature and energy over the simulation time.

energy.py holds the Energy Equations Module.

event.py and temperature.py hold the Temperature ODEs Module.

load_params.py holds the Input Format Module.

main.py holds the Control Module.

MISNotes.txt contains notes about the mapping of the MIS to the Python implementation.

output.py holds the Output Format Module.

parameters.py holds the Input Parameters Module.

plot.py holds the Plotting Module.

test.in is a sample input parameters file.

verify_output.py holds the Output Verification Module.

verify_params.py holds the Input Verification Module.

This implementation uses PyDSTool software by Rob Clewley, which requires NumPy and SciPy. This implementation also uses matplotlib to generate graphs of the results.

The PyDSTool software is available from its GitHub page: https://github.com/robclewley/pydstool

PyDSTool is also available at SourceForge. However, at the time of this writing, the latest version of PyDSTool from SourceForge did not work for the SWHS software when tested using Python 3.5. For that reason, it is recommended that the latest version of PyDSTool be obtained by cloning the GitHub repository and installing.

Instructions for obtaining NumPy and SciPy can be found here: http://scipy.org/scipylib/download.html

- For Windows users, the SciPy installation process is a little more difficult. An easier alternative method for installation is to use wheels. When downloading wheels, be sure to download the appropriate wheel for your system and python version. 
- You can find NumPy wheel files here (if you are installing SciPy with wheels, you will need to install NumPy with wheels too): http://www.lfd.uci.edu/~gohlke/pythonlibs/#numpy
- You can find SciPy wheel files here: http://www.lfd.uci.edu/~gohlke/pythonlibs/#scipy
- Once the wheels are downloaded, open a command prompt, navigate to the directory containing the wheels, and execute the following commands:

`pip install name-of-numpy-wheel.whl`

`pip install name-of-scipy-wheel.whl`

Instructions for installing matplotlib can be found here: http://matplotlib.org/users/installing.html

Once everything is installed, move the input parameters files to this directory. Note that the input parameter files must have comments denoted with the  '#' character. They must also end with the extension '.in'. The SWHS software can be run using:

`make`

The software tests can be run using:

`make test`

All generated files can be removed using:

`make clean`

__Troubleshooting__

Mac OS X users may encounter a bug that results in this error when they run `make`:

`ValueError: unknown locale: UTF-8`

To fix this, you can add the following lines to your "~/.bash_profile" file:

`export LC_ALL=en_US.UTF-8`

`export LANG=en_US.UTF-8`

Then open a new shell window, navigate to this directory, and try `make` again.