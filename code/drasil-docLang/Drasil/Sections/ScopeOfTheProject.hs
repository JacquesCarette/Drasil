module Drasil.Sections.ScopeOfTheProject 
  (scopeOfTheProjF) where

import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP, sAnd)
import Data.Drasil.Concepts.Documentation (section_, input_, output_, useCase, scpOfTheProj)

scopeOfTheProjF :: Sentence -> Contents -> Contents -> Section
scopeOfTheProjF kWord useCaseTableContents indCases = SRS.scpOfTheProj
  [introSOTP kWord]
  [prodUCTF useCaseTableContents, indPRCaseF indCases]

introSOTP :: Sentence -> Contents
introSOTP kWord = foldlSP [S "This", phrase section_, 
  S "presents the" +:+. phrase (scpOfTheProj phrase), 
  S "It describes the expected use of", kWord,
  S "as well as the", plural input_ `sAnd` plural output_, 
  S "of each action. The", plural useCase, S "are", 
  phrase input_ `sAnd` phrase output_ `sC` 
  S "which defines the action of getting the", 
  phrase input_, S "and displaying the", phrase output_]

prodUCTF :: Contents -> Section
prodUCTF useCaseTableContents = SRS.prodUCTable [useCaseTableContents] []

indPRCaseF :: Contents -> Section
indPRCaseF cases = SRS.indPRCase [cases] []
