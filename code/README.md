If you are looking for **build instructions**, find them in the README file [here](https://github.com/JacquesCarette/literate-scientific-software).

--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**Data**
  - Contains the current common-knowledge base for Drasil
  
**Example**
  - Contains the currently implemented examples
  
**Language**
  - Contains the base for the Drasil language
  
**stable**
  - Contains the (currently) expected output for each of the examples
  
Makefile
  - The makefile for building Drasil and the examples

README.md
  - This file
  
drasil.cabal
  - Cabal file, used by stack to build Drasil
  
log_check.sh
  - A shell script for comparing the generated output to the expected output for each example. Outputs whether the examples match their stable versions or not
  
stack.yaml
  - Used by Stack
