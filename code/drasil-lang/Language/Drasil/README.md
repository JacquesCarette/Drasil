--------------------------------------------------
### Summary of Folder Structure and File Contents
Last updated: July 20, 2018
--------------------------------------------------

**Chunk**
  - Contains different chunk data structures, classes, and helpers

**Development**
  - Contains supporting code for development

**Document**
  - Contains helpers for internal documentation representation

**Expr**
  - Contains helpers for the expression language

**Label**
  - Defines the Label type

**NounPhrase**
  - Defines the NP type

**Sentence**
  - Contains helpers for the Sentence type

**URI**
  - URI AST

ChunkDB.hs
  - Chunk databases. Used by System Information for looking up knowledge.

Classes.hs
  - Defines the classes for the Drasil data types

Config.hs
  - Global example configuration file for output generation

Development.hs
  - Re-exports the Development module

Document.hs
  - Drasil internal documentation representation

Expr.hs
  - Internal Drasil expression language

Label.hs
  - Helper functions for the Label type

Misc.hs
  - Helper functions

NounPhrase.hs
  - Noun phrase representations in Drasil

People.hs
  - People/Author representations in Drasil

README.md
  - This file

Reference.hs
  - Reference creation. Used for inter-document referencing and extra-document
  citations.

RefTypes.hs
  - Types of supported inter-document references

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

SystemInformation.hs
  - Defines the SI type

UID.hs
  - Defines the UID type

Unicode.hs
  - Contains special symbols including Unicode and Greek symbols