Drasil [![Build Status](https://travis-ci.com/JacquesCarette/Drasil.svg?branch=master)](https://travis-ci.com/JacquesCarette/Drasil)
====================================================

Generate all the Things! See the [currently generated artifacts](https://jacquescarette.github.io/Drasil/)

## Table of Contents

1. [What is Drasil?](#what-is-drasil)
2. [Quick Start](#quick-start)
3. [Building Specific Examples](#building-specific-examples)
4. [Running the Example(s)](#running-the-examples)
5. [Finding / Building the Haddock Documentation](#finding--building-the-haddock-documentation)
6. [Repository Contents](#summary-of-folder-structure-and-file-contents)

## What is Drasil?

For well understood domains, building software ought to be a matter of engineering, based on solid scientific foundations. The ultimate test of "well understood" is being able to teach the domain language to a computer.  Drasil is a framework for generating all of the software artifacts for (well understood) research software, from the natural knowledge base of the domain.

We take advantage of the inherent duplication of knowledge present in software artifacts (code, specification, tests, etc). More precisely, we capture the information present in software artifacts so that the particular *view* of that information in the artifacts can be reproduced by Drasil. For example, the equation **F = m a** will *look* different when rendered in documentation and in Java or Python, although it will mean the same thing. In this way, we obtain *traceability*: we know the exact relationship between information in the specification document and in the code and, furthermore, we know that they are coherent by construction.

Drasil is based on a combination of the following ideas:
1. domain knowledge changes very slowly
2. software in well-understood domains can be programmatically assembled from existing knowledge
3. the various artifacts that make up software are different views on the same knowledge
4. the most important information in crafting software are the design decisions and their rationale
5. a lot of software lives for a very long time (10+ years, often as long as 40 years), which needs a different approach

To better understand the requirements for Drasil, we follow an example-driven approach, based on a set of [case studies](https://github.com/smiths/caseStudies). This is akin to test-driven engineering, but at the system level.  The [currently generated examples](https://jacquescarette.github.io/Drasil/) serve as a good introduction to what we mean.

We wrote a [our position paper](https://github.com/JacquesCarette/Drasil/blob/master/People/Dan/ICSE%20Workshop%20-%20SE4Science/ICSE_LiterateFrameworkForSCSoftware_LSS.pdf) detailing our original ideas - but this is getting somewhat obsolete now. You can also take a look at [a poster](https://github.com/JacquesCarette/Drasil/blob/master/People/Dan/CAS%20Poster%20Competition/Poster/DrasilPoster.pdf).

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
 
