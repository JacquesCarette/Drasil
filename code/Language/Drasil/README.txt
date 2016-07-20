----------------------------------------------------------
Quick Summary of Folder Structure and File Contents
June 28, 2016
----------------------------------------------------------

CCode
  - Contains the C AST, Translator, and Printer.

Chunk
  - Contains the non-standard chunk definitions.

Expr
  - Contains tools for working with Expr.

HTML
  - Contains the HTML AST, Translator, and Printer.

Output
  - Contains definitions of output formats.

Printing
  - Contains printing helpers.

TeX
  - Contains the TeX AST, Translator, and Printer.

URI
  - Contains the URI AST.

Chunk.hs
  - Defines the standard base chunks (Chunk, Concept, Quantity).

Config.hs
  - Defines configurable parameters and flags for the document output.
  - Includes things like verbose data definition description flag,
    - Necessary LaTeX packages, etc.

Document.hs
  - Defines AST for Document structure and Layout Objects.
  - Used by all recipes.
  - This AST is for the macro-scoped language.

Expr.hs
  - Defines the internal expression language.
  - Currently includes common functions (log, sum, abs, etc.).
  - Necessary for chunks with defining equations.

Format.hs
  - Defines text formatting (TeX, Plain, or HTML).
  
Generate.hs
  - Contains the gen function which is used for generating output.

Instances.hs
  - Collects "orphan" instances that don't make sense to be defined elsewhere.
  - Includes an instance for rendering symbols and 
      function for printing them in plaintext.

Misc.hs
  - Includes mkTable (miscellaneous function used by examples).

Recipe.hs
  - Defines the recipe datatype.

RefHelpers.hs
  - Defines helper functions for referencing. 

Reference.hs
  - Defines the "makeRef" function for creating references.

SI_Units.hs
  - Defines the SI Unit library (a piece of common knowledge)
  - Contains fundamental and derivative SI Units.

Setup.hs
  - Used by cabal.

Spec.hs
  - Defines the Sentence AST (expression layout language; micro-scoped).
  - Used in all recipes.

Symbol.hs
  - Defines how Symbols are represented internally.

SymbolAlphabet.hs
  - Defines the standard English alphabet (upper and lower cases) 
      as atomic Symbols.

Unicode.hs
  - Defines all special characters.

Unit.hs
  - Contains means for defining units, both fundamental and derived.