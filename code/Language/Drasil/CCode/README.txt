----------------------------------------------------------
Quick Summary of Folder Structure and File Contents
June 28, 2016
----------------------------------------------------------

AST.hs
  - Defines the Code language AST as used by 
      Drasil (currently only represents C).

Import.hs
  - Defines the translator which converts Drasil's AST 
      representation to the C AST representation.
      
Print.hs
  - Defines the pretty printer for generating the 
      source code from the C AST representation.