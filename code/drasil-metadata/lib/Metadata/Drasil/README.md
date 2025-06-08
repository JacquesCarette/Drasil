--------------------------------------------------
### Summary of Folder Structure and File Contents
Last updated: Aug. 04, 2023
--------------------------------------------------

DrasilMeta.hs
  - Contains the schema of `DrasilMetadata.json` with an internal representation
    and provides `DrasilMetaCall.hs` with the TemplateHaskell code to read the
    JSON file.

DrasilMetaCall.hs
  - Reads `DrasilMeta.json` at compile-time.

DrasilMetadata.json
  - Data about "Drasil."

README.md
  - This file