----------------------------------------------------------
### Quick Summary of Folder Structure and File Contents
April 29, 2018
----------------------------------------------------------

AST.hs
  - Layout AST used for both HTML and TeX.

Citation.hs
  - Citation handling for layout AST.

Helpers.hs
  - Defines printing helper functions for plaintext.
  - Includes functions for wrapping braces,parentheses,etc.

Import.hs
  - Translator for moving from Drasil AST to layout AST

LayoutObj.hs
  - The layout AST version of layout objects. Similar to the Drasil layout objects.
  
README.md
  - This file.