Drasil [![Build Status](https://github.com/JacquesCarette/Drasil/actions/workflows/Build.yaml/badge.svg?branch=master)](https://github.com/JacquesCarette/Drasil/actions/workflows/Build.yaml)
[![DOI](https://zenodo.org/badge/23760783.svg)](https://zenodo.org/badge/latestdoi/23760783)
====================================================

Generate all the Things! Visit [our website](https://jacquescarette.github.io/Drasil/) to see all of the currently generated artifacts.

<p align="center">
    <a href="https://jacquescarette.github.io/Drasil/">
        <img src="code/drasil-website/WebInfo/images/Icon.png" alt="Drasil Tree" width="200" />
    </a>
</p>

## Table of Contents

1. [What is Drasil?](#what-is-drasil)
2. [Quick Start](#quick-start)
3. [Building Specific Examples](#building-specific-examples)
4. [Running the Example(s)](#running-the-examples)
5. [Finding / Building the Haddock Documentation](#finding--building-the-haddock-documentation)
6. [Repository Contents](#summary-of-folder-structure-and-file-contents)

## What is Drasil?

For well understood domains, building software ought to be a matter of engineering, based on solid scientific foundations. The ultimate test of "well understood" is being able to teach the domain language to a computer.  Drasil is a framework for generating all of the software artifacts for (well understood) research software, from the natural knowledge base of the domain.

We take advantage of the inherent duplication of knowledge present in software artifacts (code, specification, tests, etc). More precisely, we capture the information present in software artifacts so that the particular *view* of that information in the artifacts can be reproduced by Drasil. For example, the equation *`F = ma`* will *look* different when rendered in documentation and in Java or Python, although it will mean the same thing. In this way, we obtain *traceability*: we know the exact relationship between information in the specification document and in the code and, furthermore, we know that they are coherent by construction.

Drasil is based on a combination of the following ideas:

1. domain knowledge changes very slowly
2. software in well-understood domains can be programmatically assembled from existing knowledge
3. the various artifacts that make up software are different views on the same knowledge
4. the most important information in crafting software are the design decisions and their rationale
5. a lot of software lives for a very long time (10+ years, often as long as 40 years), which needs a different approach

To better understand the requirements for Drasil, we follow an example-driven approach, based on a set of [case studies](https://github.com/smiths/caseStudies). This is akin to test-driven engineering, but at the system level.  The [currently generated examples](https://jacquescarette.github.io/Drasil/) serve as a good introduction to what we mean.

We wrote a [position paper](https://github.com/JacquesCarette/Drasil/blob/master/People/Dan/ICSE%20Workshop%20-%20SE4Science/ICSE_LiterateFrameworkForSCSoftware_LSS.pdf) detailing our original ideas - but this is getting somewhat obsolete now. You can also take a look at [a poster](https://github.com/JacquesCarette/Drasil/blob/master/People/Dan/CAS%20Poster%20Competition/Poster/DrasilPoster.pdf). For more information on the details of Drasil, please see the [Drasil Wiki](https://github.com/JacquesCarette/Drasil/wiki). A collection of Drasil-related papers can be found [here](https://github.com/JacquesCarette/Drasil/wiki/Drasil-Papers-and-Documents). To contribute to this project, visit the [Contributor's Guide](https://github.com/JacquesCarette/Drasil/wiki/Contributor's-Guide).

## Quick Start

If you are on Windows, we recommend you use [Cygwin](https://cygwin.com/install.html). If you already have Git Bash installed, you can use that instead; you will just need to download [util-linux-ng](https://gnuwin32.sourceforge.net/packages/util-linux-ng.htm), which includes various system utilities (one of our scripts uses `rev`), and add its **bin/** to your PATH. `make` is required as well and can be installed following [these steps](https://stackoverflow.com/questions/32127524/how-to-install-and-use-make-in-windows); on MacOS, you may need to install [XCode](https://developer.apple.com/xcode/) to get that. Most Linux installs have it by default. You may also need to install [git](https://git-scm.com/downloads).

1. Ensure you have [Stack](https://www.haskell.org/downloads#stack) installed (if you have the Haskell Platform, you should already have Stack).
    - Also ensure that your Stack version is at least 2.3.1 (latest version); for help, see [Stack Install & Upgrade](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. If using Windows, you might need to add an exclusion to Windows Security for your **bin/** folder where your **stack.exe** is to prevent Windows Security from blocking or even deleting the executable.
    - This can be done by going to **Start > Settings > Update & Security > Windows Security > Virus & threat protection > Manage settings** (under **Virus & threat protection settings**) **> Add or remove exclusions** (under **Exclusions**), then selecting the **bin/** folder with your **stack.exe**
    - If Windows Security deletes the executable, simply reinstall it
    - This issue was encountered in Windows 10
3. Run `stack setup` while in **./code/**
    - Remember to  change your working directory to **./code/** first
    - Use `cd` to change working directory, `pwd` to print your current working directory
    - Refer to [File Directory](https://swcarpentry.github.io/shell-novice/02-filedir/index.html) for further help regarding file directory commands
    - e.g. **./Users/.../GitHub/Drasil/code** (on MacOS)
4. Use the basic `make` command to build Drasil and run **all** examples.
    - Run `make help` for a list of available commands.
    - **Warning**: this entire process takes around 10-15 minutes to complete (MacOS estimate)
5. You can find the generated output in the build folder that appears in the **./code/** folder. Each example will have its own subdirectory.

For more information, please visit the [New Workspace Setup Wiki](https://github.com/JacquesCarette/Drasil/wiki/New-Workspace-Setup).

## Building Specific Examples

Simply run: `make argument` to build and run the corresponding example, where `argument` is detailed below:

Argument | Example
-------- | -------
gamephysics_diff | 2D Rigid Body Physics Library
swhs_diff | Solar Water Heating System with Phase Change Material
glassbr_diff | Glass-BR
hghc_diff | HGHC Toy Example
ssp_diff | Slope Stability Analysis
swhsnopcm_diff | Minimal SWHS Example, with PCM Removed
projectile_diff | Projectile Motion Analysis
pdcontroller_diff | Proportional Derivative Controller
dblpend_diff | Double Pendulum
sglpend_diff | Single Pendulum

For more commands related to Drasil, use `make help` or check out the [Makefile documentation](https://github.com/JacquesCarette/Drasil/wiki/Makefile).

## Running the Example(s)

Please note that if `make` has been used, the Software Requirements Specification (SRS) documents are already generated automatically and can be found in `build`.
Automated testing can be done on these examples.

After building, you can run the examples by using `stack exec NAME` where NAME is detailed below:

NAME | Example
------|-------
gamephysics | 2D Rigid Body Physics Library
swhs | Solar Water Heating System with PCM (SWHS)
glassbr | Glass-BR
hghc | HGHC toy example
ssp | Slope Stability Analysis (SSP)
swhsnopcm | SWHS without PCM (SWHSNoPCM)
projectile | Projectile motion analysis
pdcontroller | Proportional Derivative Controller
dblpend | Double Pendulum
sglpend | Single Pendulum

This runs the examples manually from the .stack-work folder after building, and the generated docs will
appear in this folder (i.e. in the SRS folders). Due to this placement, these generated
versions will not be subject to automated tests. The tex files are generated,
but they are not automatically compiled.  To compile the tex files, use the
generated Makefile (in the same folder as the tex file).

## Finding / Building the Haddock Documentation

You can run `make docs` from the **./code/** folder to build the documentation. **Note**: this process can take about 10-12 minutes (MacOS estimate).

See the [README](https://github.com/JacquesCarette/Drasil/tree/master/code#building-up-to-date-documentation) in **./code/** for more information.

## Citation

Please use our provided `CITATION.cff` file for our preferred citation
information. GitHub provides an export of it in BibTeX and APA if needed.

### For Any Developers

Please note that we only add to our `CITATION.cff`/preferred citation file on a
consensual basis. If you would like your name added, please submit a PR with
your information added to the `CITATION.cff` file.

--------------------------------------------------

### Summary of Folder Structure and File Contents

--------------------------------------------------

**code**

- The main folder for Drasil source code and example implementations

**doc**

- Documents related to Drasil (contains the Contributor's Test)
  
**notes**

- Assorted general/administrative notes

**Papers**

- Subdirectory for papers related to Drasil framework, GOOL
  
**People**

- Contains contributions specific to some contributors (not necessarily to be implemented in Drasil)
  
**Presentations**

- Presentations on Literate Scientific Software and Drasil
  
**WindowsFix**

- Contains registry files for adding and removing the autorun of the command
  `chcp 65001`. This is to fix an issue with unicode characters. **ONLY** affects Windows machines.

.gitattributes

- Used by git (set language attributes so GitHub Linguist calculates code statistics as desired)
  
.gitignore

- Used by git (specifies which file(type)s to ignore when committing)

CITATION.cff

- Used to cite Drasil
  
LICENSE

- License information
  
README.md

- This file
