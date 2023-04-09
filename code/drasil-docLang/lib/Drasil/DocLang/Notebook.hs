module Drasil.DocLang.Notebook where

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc (introduction, learnObj, caseProb, 
  prpsOfDoc, review, summary, example, appendix, reference)

--  Notebook constructor. 
-- Create the notebook from given system name, authors, and sections
--doc :: NamedIdea c => c -> Sentence -> [Section] -> Document
--doc  sys = Document (Doc.notebook `S.forTPS` sys)

-- * Section Constructors

-- | Section constructors for the lesson plan documents/jupyter notebooks.
-- should be fixed once we have a more concrete idea of the notebook structure
-- TODO: Remove [Section] since the structure of lesson plans should not be nested
-- maybe add a new type Chapter for lesson plans?
intro, learnObj, review, caseProb, summary, appendix, reference, example :: [Contents] -> [Section] -> Section
intro     cs ss = section (titleize Doc.introduction) cs ss introLabel
learnObj  cs ss = section (titleize' Doc.learnObj)    cs ss learnObjLabel
review    cs ss = section (titleize Doc.review)       cs ss reviewLabel
caseProb  cs ss = section (titleize Doc.caseProb)     cs ss caseProbLabel
example   cs ss = section (titleize Doc.example)      cs ss exampleLabel
summary   cs ss = section (titleize Doc.summary)      cs ss summaryLabel
appendix  cs ss = section (titleize Doc.appendix)     cs ss appendixLabel
reference cs ss = section (titleize' Doc.reference)   cs ss referenceLabel

--Labels--
sectionReferences :: [Reference]
sectionReferences = [introLabel, learnObjLabel, docPurposeLabel, referenceLabel,
  reviewLabel, appendixLabel, summaryLabel, exampleLabel]

-- * Section References

-- | Individual section reference labels. Used in creating example sections for the notebook.
introLabel, learnObjLabel, docPurposeLabel, referenceLabel, 
  reviewLabel, caseProbLabel, appendixLabel, summaryLabel, exampleLabel :: Reference
introLabel          = makeSecRef "Intro"            $ titleize Doc.introduction
learnObjLabel       = makeSecRef "LearnObj"         $ titleize' Doc.learnObj
docPurposeLabel     = makeSecRef "DocPurpose"       $ titleize Doc.prpsOfDoc
referenceLabel      = makeSecRef "References"       $ titleize' Doc.reference
reviewLabel         = makeSecRef "Review"           $ titleize Doc.review
caseProbLabel       = makeSecRef "CaseProb"         $ titleize Doc.caseProb
appendixLabel       = makeSecRef "Appendix"         $ titleize Doc.appendix
summaryLabel        = makeSecRef "Summary"          $ titleize Doc.summary
exampleLabel        = makeSecRef "Example"          $ titleize Doc.example