List of refactorings that should be done to the code base.

# Smaller refactors

## drasil-utils
- [ ] move Utils.Drasil.CSV.makeCSV to (drasil-printers) Language.Drasil.CSV.prepForCSV
- [ ] make Utils.Drasil.ListUtils.atLeast2 a local function at its single use site
- [ ] move all in Utils.Drasil.Document to (drasil-printers)
- [ ] remove all uses of Utils.Drasil.Strings.toPlainName, and instead throw an error
  if a 'bad' name is used. Then delete toPlainName. [Probably will require further refactoring to make work]
- [ ] move Utils.Drasil.Database.invert to (drasil-database)

## drasil-lang
- [ ] move Drasil.Code to drasil-code
- [ ] move Language.Drasil.Derivation to drasil-theory (PR #4583)
- [ ] move Language.Drasil.Document and .* to drasil-docLang
- [ ] move Language.Drasil.NounPhrase to Language.Drasil.NaturalLanguage.English.NounPhrase
- [ ] move Language.Drasil.Chunk.DifferentialModel to (drasil-theory) Theory.Drasil.DifferentialModel

## dasil-code
- [ ] change Data.Drasil.ExternalLibraries.ODELibraries to not go via Language.Drasil.Code (PR #4582)
- [ ] move Language.Drasil.Code.Imperative.GOOL to drasil-GOOL

## drasil-gool
- [ ] redesign L-values ([#4398](../issues/4398))
- [ ] remove global `GOOLState` and replace with renderer-local state
- [ ] investigate RendererSym, and figure out what needs to be changed/removed (see [#4398](../issues/4398) and [#4404](../issues/4404))
- [ ] resolve issues with some parts of GOOL living in drasil-code
- [ ] for list, set, etc. APIs, differentiate explicitly between pure and impure functions/methods.
- [ ] investigate duplicate typeclasses between InterfaceX and RendererClassesX (e.g. NumericExpression and Unary/BinaryOpSym)
- [ ] anything else in [#4404](../issues/4404)

## drasil-build
- [ ] rename package drasil-buildLang

## drasil-printers
- [ ] remove dependency on Drasil.System by changing PrintingInformation to contain the needed
  pieces from it, rather than extracting each time
- [ ] move Language.Drasil.Printing.Import.Helpers function (resolveCapT, resolveCaP, capHelper) to
  drasil-lang
- [ ] factor Language.Drasil.Printing.Import.Helpers.processExpo to be a 1-liner
- [ ] then Language.Drasil.Printing.Import.Helpers.processExpo to drasil-lang ?
- [ ] have Language.Drasil.TeX.Print.genTeX take a LayoutObject rather than a Document

## drasil-data
- [ ] move Data.Drasil.Theories to drasil-theory? drasil-theorydb?

# Larger

## drasil-printers
- [ ]  move the 'Import' functions out of here, and into proper polymorphic traversals (Plate?) at
  their definition site

## drasil-docLang
- [ ] remove the dependencies on drasil-printers. This is likely to be 6-10 smaller refactors.
