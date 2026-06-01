----------------------------------------------------------
### Quick Summary of Folder Structure and File Contents
Last updated: December 31, 2024
----------------------------------------------------------

**Import**
  - Defines necessary external functions for layout AST.

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
  
PrintingInformation.hs
  - Defines types and functions to gather all the information needed for printing.

README.md
  - This file.