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
  - HTML Renderer - necessary files for HTML output generation

**Make**
  - AST for makefile creation for each of the output formats

**Output**
  - List of output formats

**Printing**
  - AST for printing TeX and HTML, as well as helper functions for printing

**TeX**
  - TeX Renderer - necessary files for TeX output generation

**URI**
  - URI AST

Chunk.hs
  - Base chunk

ChunkDB.hs
  - Chunk databases. Used by System Information for looking up knowledge.

Code.hs
  - Re-export code generation modules

CodeSpec.hs
  - Code specification module.

Config.hs
  - Global example configuration file for output generation

DataDesc.hs
  - Data description language for code generation.

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

RefTypes.hs
  - Types of supported inter-document references

Reference.hs
  - Reference creation. Used for inter-document referencing and extra-document
  citations.

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
  - Language of Units - Combinators, etc
  
UnitLang.hs
  - Base Unit data types
