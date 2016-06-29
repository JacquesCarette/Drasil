----------------------------------------------------------
Quick Summary of Folder Structure and File Contents
June 28, 2016
----------------------------------------------------------

AST.hs
  - Defines the HTML language AST as used by Drasil.

Helpers.hs
  - Defines helper functions for pretty printing HTML.
      
Import.hs
  - Defines the translator which converts Drasil's AST 
      representation to the HTML AST representation.
      
Print.hs
  - Defines the pretty printer for generating the HTML 
      and CSS documents from the HTML AST representation.