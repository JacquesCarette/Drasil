module Drasil.SRS (doc, doc', intro, prpsOfDoc, scpOfReq, orgOfDoc, genSysDec, userChar, sysCon, specSysDec, probDesc,
  goalStmt, solCharSpec, assump, thModel, genDefn, dataDefn, inModel, datCon, require,
  nonfuncReq, funcReq, likeChg) where
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

intro, prpsOfDoc, scpOfReq, orgOfDoc, genSysDec, userChar, sysCon, specSysDec, probDesc,
  goalStmt, solCharSpec, assump, thModel, genDefn, dataDefn, inModel, datCon, require,
  nonfuncReq, funcReq, likeChg :: [Contents] -> [Section] -> Section

intro conts sects = section (titleize Doc.introduction) conts sects
prpsOfDoc conts sects = section (titleize Doc.prpsOfDoc) conts sects
scpOfReq conts sects = section (titleize Doc.scpOfReq) conts sects
orgOfDoc conts sects = section (titleize Doc.orgOfDoc) conts sects

genSysDec conts sects = section (titleize Doc.generalSystemDescription) conts sects
userChar conts sects = section (titleize' Doc.userCharacteristic) conts sects
sysCon conts sects = section (titleize' Doc.systemConstraint) conts sects

specSysDec conts sects = section (titleize Doc.specificsystemdescription) conts sects
probDesc conts sects = section (titleize Doc.problemDescription) conts sects
goalStmt conts sects = section (titleize Doc.goalStmt) conts sects
solCharSpec conts sects = section (titleize Doc.solutionCharSpec) conts sects
assump conts sects = section (titleize' Doc.assumption) conts sects
thModel conts sects = section (titleize' Doc.thModel) conts sects
genDefn conts sects = section (titleize' Doc.genDefn) conts sects
dataDefn conts sects = section (titleize' Doc.dataDefn) conts sects
inModel conts sects = section (titleize' Doc.inModel) conts sects
datCon conts sects = section (titleize' Doc.datumConstraint) conts sects

require conts sects = section (titleize' Doc.requirement) conts sects
nonfuncReq conts sects = section (titleize' Doc.nonfunctionalRequirement) conts sects
funcReq conts sects = section (titleize' Doc.functionalRequirement) conts sects
likeChg conts sects = section (titleize' Doc.likelyChg) conts sects