--------------------------------------------------
### Summary of Folder Structure and File Contents
--------------------------------------------------

**Chunk**
  - Contains different chunk data structures, classes, and helpers
  
**Code**
  - Code AST and necessary files for code generation. Includes GOOL
  
**Expr**
  - Contains helpers for the expression language
  
**HTML**
  - HTML AST and necessary files for HTML output generation

**Make**
  - AST for makefile creation for each of the output formats

**Output**
  - List of output formats

**Printing**
  - Helpers for printing

**TeX**
  - TeX AST and necessary files for TeX output generation

**URI**
  - URI AST

Chunk.hs
  - Base chunk

Code.hs
  - Re-export code generation modules

Config.hs
  - Global example configuration file for output generation

Document.hs
  - Drasil internal documentation representation

Expr.hs
  - Internal Drasil expression language

Format.hs
  - Ouput formatting of documents (TeX, HTML, or Plain)

Generate.hs
  - Code for generating artefacts

Misc.hs
  - Helper functions

NounPhrase.hs
  - Noun phrase representations in Drasil

People.hs
  - People/Author representations in Drasil

README.md
  - This file

Recipe.hs
  - Recipe data type

RefHelper.hs
  - Helper functions for references

Reference.hs
  - Reference creation function

Setup.hs
  - Used by cabal
  
Space.hs
  - Space data type

Spec.hs
  - Contains the internal Drasil Sentence representation and associated items

Symbol.hs
  - Symbol representation in Drasil. Strictly the graphical representation.
  Made up of many types of items

SymbolAlphabet.hs
  - Symbols for all lower and uppercase English letters

Unicode.hs
  - Contains special symbols including Unicode and Greek symbols

Unit.hs
  - Language of Units
  