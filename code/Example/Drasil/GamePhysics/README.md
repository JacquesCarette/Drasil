Notes (2/6/2016):

Some library modifications needed to build this project:
- Modified project-specific units in Units.hs
- Added Omega and Zeta to Unicode.hs
- Modified Drasil.hs to import all SymbolAlphabets
- Modified Main.hs to only generate the specific Chipmunk SRS

These modified files can be found in the library-modifications folder.
The original files must be replaced by these files before attempting
to build the lss executable.

Buld instructions:
Running "cabal build" or "cabal install ." from the directory containing 
Drasil.cabal should be sufficient to generate the lss executable.
The lss.exe in dist/build/lss will generate the documentation.
