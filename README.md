The Drasil Framework
====================================================

Generate all the Things!

## What is Drasil?

Drasil is a framework for generating all of the software artifacts from
a stable knowledge base, focusing currently on scientific software. The main goals
are to reduce knowledge duplication and improve traceability. We also are
extreme in our drive to reuse everything.  We hope that
maintainability, verifiability, traceability, and other software qualities will 
also be improved as a side-effect of our methods.

Drasil is used to generate *all* requisite software artifacts from a common 
*knowledge-base* using *recipes* written in a Domain-Specific Language (DSL).
These recipes allow us to specify which pieces of knowledge should be used in
which artifacts, how to transform them, and more.

Drasil is being designed and implemented using a grounded theory approach.  To
determine what is needed and make the design general, we are concurrently
implementing six examples (case studies) in Drasil.  (The specific examples are
listed below under "Building specific examples.")  These examples were first
written using a typical "manual" approach.  The full files for the case studies
are available in [a separate case studies repo](https://github.com/smiths/caseStudies).

For more information on Drasil, please read 
[our position paper](https://github.com/JacquesCarette/Drasil/blob/master/Dan/ICSE%20Workshop%20-%20SE4Science/ICSE_LiterateFrameworkForSCSoftware_LSS.pdf)
or take a look at 
[our poster](https://github.com/JacquesCarette/Drasil/blob/master/Dan/CAS%20Poster%20Competition/Poster/DrasilPoster.pdf).

## Quick Start

1. Ensure you have [Stack](https://www.haskell.org/downloads#stack) installed (if you have the Haskell Platform, you should already have Stack).
2. Run `stack setup` while in **./code/**
3. Use the `make` command to build the current version of Drasil. This will build and run all of the examples as well.
4. You can find the generated output in the build folder that appears. Each example will have its own subdirectory.

## Building specific examples

Simply run: `make argument` to build the corresponding example, where argument is detailed below:

Argument | Example
-------- | -------
gamephys_diff | Chipmunk2D
swhs_diff | Solar Water Heating System with PCM
glassbr_diff | Glass-BR
tiny_diff | HGHC toy example
ssp_diff | Slope Stability Analysis
nopcm_diff | minimal SWHS example, with PCM removed

## Running the example(s)

Please note that if `make` has beeen used, docs are already generated automatically and can be found in build.
Automated testing can be done on these examples.

After building, you can run the examples by using `stack exec NAME` where NAME is detailed below:

NAME | Example
------|-------
chipmunkdocs | Chipmunk2D
swhs | Solar Water Heating System with PCM (SWHS)
glassbr | Glass-BR
tiny | HGHC toy example
ssp | Slope Stability Analysis (SSP)
nopcm | SWHS without PCM (NoPCM)

This runs the examples manually from the .stack-work folder after building, and the generated docs will
appear in this folder (i.e. in the SRS and Website folders). Due to this placement, these generated
versions will not be subject to automated tests.

## Finding / Building the Haddock documentation

You can run `make docs` from the ./code folder to build the documentation.

See the [README](https://github.com/JacquesCarette/Drasil/tree/master/code#building-up-to-date-documentation) 
in ./code/ for more information.

--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**Dan**
  - Subdirectory for Dan Szymczak's work. Mostly papers and presentations
  
**FamilyOfGamePhysicsEngines**
  - Subdirectory containing papers on Families of Physics Engines / Material Models
  
**GOOLCodeGen**
  - An isolated compilation space for creating GOOL code for the examples.
  
**Presentations**
  - Presentations on LSS/Drasil
  
**RelatedCode**
  - Contains the *Generic Object Oriented Language* (GOOL) code.
  
**SRS_LPM_Example**
  - SRS for a fuel pin (more comprehensive than HGHC)
  
**Steve**
  - Steven Palmer's work
  
**WindowsFix**
  - Contains registry files for adding and removing the autorun of the command 
  `chcp 65001`. This is to fix an issue with unicode characters. **ONLY** affects Windows machines.
  
**code**
  - The main folder for Drasil source code and example implementations
  
**notes**
  - Assorted general/administrative notes

.gitattributes
  - Used by git
  
.gitignore
  - Used by git (specifies which file(type)s to ignore when committing)
  
.travis.yml
  - Used for continuous integration with Travis CI
  
LICENSE
  - License information
  
README.md
  - This file
  
