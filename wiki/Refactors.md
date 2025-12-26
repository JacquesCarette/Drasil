List of refactorings that should be done to the code base.

## dsrail-utils
- [ ] move Utils.Drasil.CSV.makeCSV to (drasil-printers) Language.Drasil.CSV.prepForCSV
- [ ] make Utils.Drasil.ListUtils.atLeast2 a local function at its single use site
- [ ] move all in Utils.Drasil.Document to (drasil-printers)
- [ ] remove all uses of Utils.Drasil.Strings.toPlainName, and instead throw an error
  if a 'bad' name is used. Then delete toPlainName. [Probably will require further refactoring to make work]
- [ ] move Utils.Drasil.Database.invert to (drasil-database)

## drasil-lang
- [ ] move Drasil.Code to drasil-code
- [ ] move Language.Drasil.Derivation to drasil-docLang
- [ ] move Language.Drasil.Document and .* to drasil-docLang
- [ ] move Language.Drasil.NounPhrase to Language.Drasil.NaturalLanguage.English.NounPhrase
- [ ] move Language.Drasil.Chunk.DifferentialModel to (drasil-theory) Theory.Drasil.DifferentialModel

## dasil-code
- [ ] change Data.Drasil.ExternalLibraries.ODELibraries to not go via Language.Drasil.Code
- [ ] move Language.Drasil.Code.Imperative.GOOL to drasil-GOOL

