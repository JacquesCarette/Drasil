module Drasil.SRS
 (doc, doc', intro, prpsOfDoc, scpOfReq, orgOfDoc, genSysDec,
  userChar, sysCon, specSysDec, probDesc, goalStmt, solCharSpec, assump,
  thModel, genDefn, dataDefn, inModel, datCon, require, nonfuncReq, funcReq,
  likeChg) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc

-- Local function to keep things looking clean, not exported.
forTT' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT' = for'' titleize titleize'

doc, doc' :: NamedIdea c => c -> Sentence -> [Section] -> Document
doc sys authors secs = Document (Doc.srs `for` sys) authors secs
doc' sys authors secs = Document (Doc.srs `forTT'` sys) authors secs

intro, prpsOfDoc, scpOfReq, orgOfDoc, genSysDec, userChar, sysCon, specSysDec,
 probDesc, goalStmt, solCharSpec, assump, thModel, genDefn, dataDefn,
 inModel, datCon, require, nonfuncReq, funcReq, likeChg
 :: [Contents] -> [Section] -> Section

intro       = section (titleize Doc.introduction)
prpsOfDoc   = section (titleize Doc.prpsOfDoc)
scpOfReq    = section (titleize Doc.scpOfReq)
orgOfDoc    = section (titleize Doc.orgOfDoc)

genSysDec   = section (titleize Doc.generalSystemDescription)
userChar    = section (titleize' Doc.userCharacteristic)
sysCon      = section (titleize' Doc.systemConstraint)

specSysDec  = section (titleize Doc.specificsystemdescription)
probDesc    = section (titleize Doc.problemDescription)
goalStmt    = section (titleize Doc.goalStmt)
solCharSpec = section (titleize Doc.solutionCharSpec)
assump      = section (titleize' Doc.assumption)
thModel     = section (titleize' Doc.thModel)
genDefn     = section (titleize' Doc.genDefn)
dataDefn    = section (titleize' Doc.dataDefn)
inModel     = section (titleize' Doc.inModel)
datCon      = section (titleize' Doc.datumConstraint)

require     = section (titleize' Doc.requirement)
nonfuncReq  = section (titleize' Doc.nonfunctionalRequirement)
funcReq     = section (titleize' Doc.functionalRequirement)
likeChg     = section (titleize' Doc.likelyChg)
