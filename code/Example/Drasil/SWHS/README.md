This directory contains the SWHS incorporating PCM example for Drasil.

To build the SRS, run the following commands:

`cabal build`

`./dist/build/swhs/swhs`

This will build the SRS in this directory. The SRS must be built in this directory or else it will not be able to find the .png files used for figures in the SRS body. If you need to build the SRS in another directory, you can move the ATrace.png, RTrace.png, and Tank.png files to that directory to ensure that all the figures appear.