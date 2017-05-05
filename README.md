literate-scientific-software - The Drasil Framework
====================================================

Repository for working on both literate software and families of scientific software

## What is Drasil?

Drasil is a framework for literate scientific software development. The main goals
are to reduce knowledge duplication and improve reusability, maintainability,
verifiability, traceability, and other software qualities.

Drasil is used to generate *all* requisite software artifacts from a common 
*knowledge-base* using *recipes* written in a Domain-Specific Language (DSL).
These recipes allow us to specify which pieces of knowledge should be used in
which artifacts, how to transform them, and more.

For more information on Drasil, please read 
[this paper](https://github.com/JacquesCarette/literate-scientific-software/blob/master/Dan/ICSE%20Workshop%20-%20SE4Science/ICSE_LiterateFrameworkForSCSoftware_LSS.pdf)
or take a look at 
our [poster](https://github.com/JacquesCarette/literate-scientific-software/blob/master/Dan/CAS%20Poster%20Competition/Poster/DrasilPoster.pdf).

## Quick Start

1. Ensure you have [Stack](https://www.haskell.org/downloads#stack) installed (if you have the Haskell Platform, you should already have Stack).
2. Run `stack setup` while in **./code/**
3. Use the `make` command to build the current version of Drasil. This will build and run all of the examples as well.
4. You can find the generated output in the SRS, MIS, MG, C++, and Website folders that appear.

## Building specific examples

Simply run: `make argument` to build the corresponding example, where argument is detailed below:

Argument | Example
-------- | -------
gamephys | Chipmunk2D
swhs | Solar Water Heating System with PCM
glassbr | Glass-BR
tiny | HGHC toy example
ssp | Slope Stability Analysis
pcm | minimal SWHS example, with PCM removed

## Running the example(s)

After building, you can find the executables for the examples in the relevant subdirectories of .stack-work/dist/ca59d0ab/build/

--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**Case Studies**
  - Manually produced code/documentation for the Chipmunk2D, GlassBR, and SWHS examples

**Chipmunk2D-master**
  - A copy of the Chipmunk2D engine
  
**Dan**
  - Subdirectory for Dan Szymczak's work. Mostly papers and presentations
  
**FamilyOfGamePhysicsEngines**
  - Subdirectory containing papers on Families of Physics Engines / Material Models
  
**Presentations**
  - Presentations on LSS/Drasil
  
**RecipeForSCSoftDev**
  - Paper *A Recipe for Scientific Computing Software Development*
  
**RelatedCode**
  - Contains the *Generic Object Oriented Language* (GOOL) code.
  
**SRS_LPM_Example**
  - SRS for a fuel pin (more comprehensive than HGHC)
  
**SampleEngCalcReport**
  - A sample Engineering Calculation Report
  
**SolarHeatingWaterOnly**
  - A simplified version of the SWHS project, with PCM removed. This is the basis of the *pcm* example
  
**Steve**
  - Steven Palmer's work
  
**code**
  - The main folder for Drasil source code and example implementations
  
**hghc_Example**
  - The simplified fuel pin example. This is the basis of the *tiny* toy example 

.gitattributes
  - Used by git
  
.gitignore
  - Used by git (specifies which file(type)s to ignore when committing)
  
LICENSE
  - License information
  
README.md
  - This file
  
