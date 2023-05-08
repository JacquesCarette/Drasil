If you are looking for **build instructions**, find them in the [top level README](https://github.com/JacquesCarette/Drasil).

--------------------------------------------------
## Drasil Coding Style Guide
Last updated: August 20, 2021

--------------------------------------------------

1. Use spaces instead of tabs. Preferably 2 spaces per indent level. Ex:

```Haskell
top level
  indent level 1
    indent level 2
```
2. Keep line lengths to a maximum of 80 characters.

3. Use camel case for naming. Ex: `someFunctionFoo`

4. Make names meaningful. Ex: `htTransCladFuel` as opposed to `hG`

5. One blank line between top-level definitions. No blank lines between type signatures and function definitions.

6. Surround binary operators with a single space on either side. Ex: `x ^. term`

7. Do not break operations across lines, unless they are sentence combinators (`:+:`, `+:+`, `(sC)`, etc.). Ex: 

```Haskell
  S "The" +:+ (phrase $ heatTrans ^. term) +:+ 
  S "is the"
  ...
```
For more information, please see the [Contributor's Guide](https://github.com/JacquesCarette/Drasil/wiki/Contributor's-Guide).

-------------------------------------------------
### Quick Start Legend
-------------------------------------------------

| Combinator | Usage / Meaning | Type Signature |
|------------|-----------------|----------------|
| `+:+` | Concatenates two sentences and adds a space in between them | Sentence -> Sentence -> Sentence |
| `:+:` | Concatenates two sentences | Sentence -> Sentence -> Sentence |
| `atStart` | Returns the singular form of a NamedIdea, for use at the start of a sentence (ie. Capitalized) | NamedIdea n => n -> Sentence |
| `atStart'` | Returns the plural form of a NamedIdea, for use at the start of a sentence (ie. Capitalized) | NamedIdea n => n -> Sentence |
| `cn` | Create a self-plural, common-noun NounPhrase | String -> NP |
| `cn'` | Create a common-noun NounPhrase which becomes plural by adding "s" | String -> NP |
| `introduceAbb` | Introduces an abbreviation using the titleized version of the term followed by the abbreviation in parentheses | NamedIdea n => n -> Sentence |
| `of_` | Creates a compound nounphrase from two terms, with " of " between them | (NamedIdea a, NamedIdea b) => a -> b -> NP |
| `phrase` | Returns the singular form of a NamedIdea | NamedIdea n => n -> Sentence |
| `plural` | Returns the plural form of a NamedIdea | NamedIdea n => n -> Sentence |
| `sParen` | Wraps a Sentence in parentheses | Sentence -> Sentence |
| `titleize` | Returns the singular, title form of a NamedIdea | NamedIdea n => n -> Sentence |
| `titleize'` | Returns the plural, title form of a NamedIdea | NamedIdea n => n -> Sentence |

**Note: See Language.Drasil for more information on the Sentence type**

Also take a look at the Documentation section of the [Drasil Wiki](https://github.com/JacquesCarette/Drasil/wiki) for more information, as well as the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/index.html) for more functions.

-------------------------------------------------
### Data Types and Class Instances
-------------------------------------------------

See the 
[Analysis of the Drasil Framework](https://jacquescarette.github.io/Drasil/#Sec:Analysis) for more information on which chunk types are instances of which classes.

-------------------------------------------------
### Building Up-to-Date Documentation
-------------------------------------------------

To build the documentation for Drasil, simply type `make docs`. It will build 1 variant of the Haddock documentation; an internal documentation set with all modules fully exposed. 

Noticeably, when we run `FULL=1 make docs`, it builds 2 variants of the Haddock documentation; a normal external documentation set, and an internal documentation set with all modules fully exposed.

Upon completion, you should receive several messages stating the Haddock for certain
packages has been updated, as well as directory locations for the related generated Haddock docs.

The generated Haddock documentation will be placed inside of a newly created `code/docs/` folder.
You should open up the `code/docs/index.html` file with your web browser if you'd like to view 
the external documentation set. Alternatively, if you would like to view the internal documentation set,
you should instead open the `code/docs/full/index.html` file.

--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**datafiles**

  - Contains additional "helper" files for each of the examples

**drasil-build**
  - Contains build system generators for Drasil artifacts

**drasil-code**
  - Contains the code for code generation in Drasil

**drasil-data**
  - Contains the current common-knowledge base for Drasil

**drasil-database**
* Contains the database for Drasil

**drasil-docLang**
  - Contains the document language for Drasil

**drasil-example**
  - Contains the currently implemented examples

**drasil-gen**
  - Contains the generation file(s) for actually generating code

**drasil-gool**
  - Contains GOOL, a Generic Object-Oriented Language

**drasil-lang**
  - Contains the base for the Drasil language

**drasil-metadata**
  - Records domain information for use in Drasil

**drasil-printers**
  - Contains the printers for the Drasil language

**drasil-theory**
  - Contains data structures and functions for models and definitions

**drasil-utils**
  - Contains utilities used throughout Drasil

**drasil-website**
- Contains all of the necessary code to generate the [Drasil website](https://jacquescarette.github.io/Drasil/).

**scripts**
  - Contains all Drasil-related scripts for compiling and deploying Drasil

**stable**
  - Contains the (currently) expected output for each of the examples

countCommand.txt
  - A text file containing instructions on how to count all unique words 
    within a file using bash commands

.hlint.yaml
  - Used by HLint

Drasil.md
  - (Outdated) detailed contents of some Drasil packages

Makefile
  - The makefile for building Drasil and the examples

README.md
  - This file

shell.nix
  - Installs Drasil development requirements (for use with nix)

stack.yaml
  - Used by Stack
