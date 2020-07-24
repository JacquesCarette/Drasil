The Drasil Framework
====================================================

Generate all the Things!

[Current Generated Artifacts](https://jacquescarette.github.io/Drasil/)

## Table of Contents

1. [What is Drasil?](#what-is-drasil)
   - description of the Drasil framework
2. [Quick Start](#quick-start)
   - instructions for quick-starting the Drasil framework
3. [Building Specific Examples](#building-specific-examples)
   - instructions for how to build specific examples using Drasil
4. [Running the Example(s)](#running-the-examples)
   - instructions for running specific examples
5. [Finding / Building the Haddock Documentation](#finding--building-the-haddock-documentation)
   - Haddock documentation instructions
6. [Summary of Folder Structure and File Contents](#summary-of-folder-structure-and-file-contents)

## What is Drasil?

Drasil is a framework for generating all of the software artifacts from a stable knowledge base, focusing currently on scientific software. The main goals are to reduce knowledge duplication and improve traceability. We also are extreme in our drive to reuse everything.  We hope that maintainability, verifiability, traceability, and other software qualities will also be improved as a side-effect of our methods.

Drasil is used to generate *all* requisite software artifacts from a common *knowledge-base* using *recipes* written in a Domain-Specific Language (DSL). These recipes allow us to specify which pieces of knowledge should be used in which artifacts, how to transform them, and more.

Drasil is being designed and implemented using a grounded theory approach.  To determine what is needed and make the design general, we are concurrently implementing six examples (case studies) in Drasil.  (The specific examples are listed below under "Building specific examples.")  These examples were first
written using a typical "manual" approach.  The full files for the case studies are available in [a separate case studies repo](https://github.com/smiths/caseStudies).

For more information on Drasil, please read [our position paper](https://github.com/JacquesCarette/Drasil/blob/master/People/Dan/ICSE%20Workshop%20-%20SE4Science/ICSE_LiterateFrameworkForSCSoftware_LSS.pdf) or take a look at [our poster](https://github.com/JacquesCarette/Drasil/blob/master/People/Dan/CAS%20Poster%20Competition/Poster/DrasilPoster.pdf).

## Quick Start

If you are on Windows, we recommend you use cygwin (MinGW probably works too, but we have not tested it).  `make` is required as well; on MacOS, you may need to install XCode to get that. Most linux installs have it by default.

1. Ensure you have [Stack](https://www.haskell.org/downloads#stack) installed (if you have the Haskell Platform, you should already have Stack).
	- Also ensure that your stack version is at least 2.3.1 (latest version); for help, see [Stack Install & Upgrade](docs.haskellstack.org/en/stable/install_and_upgrade/)
2. Run `stack setup` while in **./code/**
	- Remember to  change your working directory to **./code/** first
	- Use `cd` to change working directory, `pwd` to print your current working directory
	- Refer to [File Directory](swcarpentry.github.io/shell-novice/02-filedir/index.html) for further help regarding file directory commands
	- e.g. **./Users/.../GitHub/Drasil/code** (on MacOS)
3. Use the `make` command to build the current version of Drasil. Note that this will build and run **all** of the examples as well.
	- **Warning**: this entire process takes around 10-15 minutes to complete (MacOS estimate)
4. You can find the generated output in the build folder that appears in the **./code/** folder. Each example will have its own subdirectory.

## Building Specific Examples

Simply run: `make argument` to build the corresponding example, where argument is detailed below:

Argument | Example
-------- | -------
gamephysics_diff | 2D Rigid Body Physics Library
swhs_diff | Solar Water Heating System with PCM
glassbr_diff | Glass-BR
tiny_diff | HGHC toy example
ssp_diff | Slope Stability Analysis
nopcm_diff | minimal SWHS example, with PCM removed
projectile_diff | Projectile motion analysis

## Running the Example(s)

Please note that if `make` has been used, docs are already generated automatically and can be found in build.
Automated testing can be done on these examples.

After building, you can run the examples by using `stack exec NAME` where NAME is detailed below:

NAME | Example
------|-------
gamephysics | 2D Rigid Body Physics Library
swhs | Solar Water Heating System with PCM (SWHS)
glassbr | Glass-BR
tiny | HGHC toy example
ssp | Slope Stability Analysis (SSP)
nopcm | SWHS without PCM (NoPCM)
projectile | Projectile motion analysis

This runs the examples manually from the .stack-work folder after building, and the generated docs will
appear in this folder (i.e. in the SRS and Website folders). Due to this placement, these generated
versions will not be subject to automated tests.

## Finding / Building the Haddock Documentation

You can run `make docs` from the ./code/ folder to build the documentation. **Note**: this process can take about 10-12 minutes (MacOS estimate).

See the [README](https://github.com/JacquesCarette/Drasil/tree/master/code#building-up-to-date-documentation) in ./code/ for more information.

--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**Papers**
  - Subdirectory for papers related to Drasil framework, GOOL
  
**People**
  - Contains contributions specific to some contributors (not necessarily to be implemented in Drasil)
  
**Presentations**
  - Presentations on LSS/Drasil
  
**WindowsFix**
  - Contains registry files for adding and removing the autorun of the command 
  `chcp 65001`. This is to fix an issue with unicode characters. **ONLY** affects Windows machines.
  
**code**
  - The main folder for Drasil source code and example implementations
  
**doc**
  - Documentation related to Drasil
  
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
 