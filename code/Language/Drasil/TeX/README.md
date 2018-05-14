----------------------------------------------------------
### Quick Summary of Folder Structure and File Contents
April 29, 2018
----------------------------------------------------------

Helpers.hs
  - Contains helper functions for pretty printing TeX.

Monad.hs
  - Maintains the printing context (text vs math mode).

Preamble.hs
  - Generates the TeX preamble based on the necessary packages for a doc

Print.hs
  - Defines the pretty printer for generating the
  LaTeX documents from the AST representation.

README.md
  - This file.
