----------------------------------------------------------
Quick Summary of Folder Structure and File Contents
June 28, 2016
----------------------------------------------------------

AST.hs
  - Defines the TeX language AST as used by Drasil.

Helpers.hs
  - Contains helper functions for pretty printing TeX.
  
Import.hs
  - Defines the translator which converts Drasil's AST 
      representation to the TeX AST representation.
      
Print.hs
  - Defines the pretty printer for generating the 
      LaTeX documents from the AST representation.