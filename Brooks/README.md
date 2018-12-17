A finally-tagless implementation of GOOL (Generic Object Oriented Language)

- New.hs defines the "Symantics" typeclasses for the finally tagless implementation

- LanguageRenderers/ contains instances of the "Symantics" typeclasses for generating code in (currently) Java and Python.

- NewLanguageRenderer.hs contains default functions for printing code

- Example/ contains some example programs written in GOOL. To generate Java and Python code for these programs, run `stack ghci` followed by `main`

