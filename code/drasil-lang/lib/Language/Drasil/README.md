--------------------------------------------------
### Summary of Folder Structure and File Contents
Last updated: August 12, 2021
--------------------------------------------------

**Note on `.Core` Modules:**
Several subdirectories contain a `Core.hs` file (e.g., `Chunk/Concept/Core.hs`, `Document/Core.hs`, `NounPhrase/Core.hs`).
These modules follow a naming convention where `.Core` indicates the module contains foundational
type definitions and data structures for that subsystem, typically with minimal dependencies.
They serve as the basis upon which other modules in the same subsystem build additional functionality.

**Chunk**
  - Contains different chunk data types and constructors.

**Classes**
  - Contains most classes for use with Drasil data types.

**Data**
  - Contains data for dates and citations.

**Development**
  - Contains supporting code for development (as of now, only contains helper functions for working with NamedIdeas and Sentences).

**Document**
  - Contains data types for creating the Drasil document language.

**Expr**
  - Contains helpers for the expression language.

**Label**
  - Defines the Label type, used in References.

**ModelExpr**
  - Defines an expression language focused on display features.

**NounPhrase**
  - Defines data types for making noun phrases with proper pluralization and capitalization.

**Sentence**
  - Contains helpers for bridging between Sentences and Chunk UIDs

**Symbol**
  - Contains helpers for dealing with Symbols and Stages.

**Uncertainty**
  - Contains the Uncertainty type and helpers.

**URI**
  - URI AST (defines URI types).

Classes.hs
  - Defines the classes for use with the Drasil data types, including chunks, references, and models.

Constraint.hs
  - Constraint representations in Drasil.

DecoratedReference.hs
  - Defines References that can hold extra reference information.

Derivation.hs
  - Defines the Derivation type and helpers.

Development.hs
  - Re-exports some expression and sentence-related modules.

Document.hs
  - Drasil internal documentation representation types and functions.

Expr.hs
  - Internal Drasil expression language.

Misc.hs
  - Various helper functions.

NounPhrase.hs
  - Noun phrase representations through types and various constructor functions.

People.hs
  - People/Author representations in Drasil.

README.md
  - This file

Reference.hs
  - Reference creation for document referencing to sections, citations, and URLs.

Sentence.hs
  - Defines Sentence data type and helper functions.

ShortHands.hs
  - Symbols for all lower and uppercase English and Greek letters.

ShortName.hs
  - Defines ShortName data type.

Space.hs
  - Space data type and helpers.

Stages.hs
  - Defines Stage data type.

Symbol.hs
  - Strictly graphical representation in Drasil made up of many types of items. Includes symbol sorting functions.

UID.hs
  - Defines the UID type.

Uncertainty.hs
  - Imports module from uncertainty folder and adds a few helper functions.

Unicode.hs
  - Contains special symbols including the degree and partial differentiation symbols.

UnitLang.hs
  - Defines the language for working with Units.
