module Drasil.Metadata.Documentation
  ( abbAcc, abbreviation, acronym, appendix, assumption, charOfIR, characteristic
  , client, consVals
  , constraint, content, context, corSol, customer, datum, datumConstraint
  , definition, description, document, functional, functionalRequirement
  , general, generalSystemDescription, goal, goalStmt, individual, indPRCase
  , introduction, intReader, learnObj, likelyChg, nonfunctional, nonfunctionalRequirement
  , notebook, offShelf, offShelfSolution, organization, orgOfDoc
  , physical, physSyst, physicalSystem, product_, productUC, prodUCTable
  , problem, problemDescription, project, property, propOfCorSol
  , prpsOfDoc, purpose, reference, refMat
  , requirement, scope, scpOfTheProj, scpOfReq
  , software, softwareReq, solution, solutionCharSpec, solutionCharacteristic
  , specific, specification, specificsystemdescription, srs, stakeholder
  , statement, symbol_, system, systemConstraint, sysCont, systemdescription, table_
  , terminology, termAndDef, tOfCont, tOfSymb, tOfUnit, traceyMandG, traceyMatrix
  , unlikelyChg, useCase, user, userCharacteristic
  )
  where

import Control.Lens ((^.))

import Drasil.Metadata.Concepts.Math (graph, unit_)
import Drasil.Metadata.Domains (documentc, softEng)

import Language.Drasil.Development (NPStruct)
import Language.Drasil (CI, NP, IdeaDict, nc, cn, cn', cnIES, cnICES, cnUM,
  commonIdeaWithDict, fterms, compoundPhraseP1, titleizeNP', term)
import Language.Drasil.Chunk.Concept.NamedCombinators
  (combineNINI, compoundNC, compoundNCPP, of_, of_PS, ofAPS, of_NINP, theGen
  , compoundNCPSPP, and_, and_TGen, and_PP)

softReqSpec :: NP
softReqSpec = fterms compoundPhraseP1 softwareReq specification

abbAcc, abbreviation, acronym, appendix, characteristic, charOfIR, client, consVals,
  constraint, content, context, corSol, customer, datum,
  definition, description, document, functional, general, goal, individual,
  introduction, intReader, nonfunctional, offShelf, organization, orgOfDoc,
  physical, project, product_, problem, property, propOfCorSol, prpsOfDoc, purpose,
  reference, refMat, scope, scpOfReq, software, solution, specific, specification,
  stakeholder, statement, symbol_, system, table_, terminology, termAndDef,
  tOfCont, tOfSymb, tOfUnit, traceyMatrix,
  useCase, user :: IdeaDict

abbAcc          = nc "TAbbAcc"        (abbreviation `and_PP` acronym)
abbreviation    = nc "abbreviation"   (cn'    "abbreviation"       )
acronym         = nc "acronym"        (cn'    "acronym"            )
appendix        = nc "appendix"       (cnICES "appendix"           )
characteristic  = nc "characteristic" (cn'    "characteristic"     )
charOfIR        = nc "charOfIR"       (characteristic `of_PS` intReader)
client          = nc "client"         (cn'    "client"             )
constraint      = nc "constraint"     (cn'    "constraint"         )
consVals        = nc "consVals"       (cn "values of auxiliary constants")
content         = nc "content"        (cn'    "content"            )
context         = nc "context"        (cn'    "context"            )
corSol          = nc "corSol"         (cn' "correct solution")
customer        = nc "customer"       (cn'    "customer"           )
datum           = nc "datum"          (cnUM   "datum"              )
definition      = nc "definition"     (cn'    "definition"         )
description     = nc "description"    (cn'    "description"        )
document        = nc "document"       (cn'    "document"           )
functional      = nc "functional"     (cn'    "functional"         ) --FIXME: Adjective
general         = nc "general"        (cn'    "general"            ) --FIXME: Adjective
goal            = nc "goal"           (cn'    "goal"               )
individual      = nc "individual"     (cn'    "individual"         )
intReader       = nc "intReader"      (cn'    "intended reader"    )
introduction    = nc "introduction"   (cn'    "introduction"       )
nonfunctional   = nc "non-functional" (cn'    "non-functional"     ) --FIXME: Adjective
offShelf        = nc "Off-the-Shelf"  (cn'    "Off-the-Shelf"      )
organization    = nc "organization"   (cn'    "organization"       )
orgOfDoc        = nc "orgOfDoc"       (organization `of_` document)
physical        = nc "physical"       (cn'    "physical"           ) --FIXME: Adjective
product_        = nc "product"        (cn'    "product"            )
problem         = nc "problem"        (cn'    "problem"            )
project         = nc "project"        (cn'    "project"            )
property        = nc "property"       (cnIES  "property"           )
propOfCorSol    = nc "propOfCorSol"   (property `ofAPS` corSol     )
prpsOfDoc       = nc "prpsOfDoc"      (purpose `of_` document      )
purpose         = nc "purpose"        (cn'    "purpose"            )
reference       = nc "reference"      (cn'    "reference"          )
refMat          = nc "refMat"         (cn' "reference material")
scope           = nc "scope"          (cn'    "scope"              )
scpOfReq        = nc "scpOfReq"       (scope `of_` requirement     )
software        = nc "software"       (cn     "software"           )
solution        = nc "solution"       (cn'    "solution"           )
specific        = nc "specific"       (cn'    "specific"           ) --FIXME: Adjective
specification   = nc "specification"  (cn'    "specification"      )
stakeholder     = nc "stakeholder"    (cn'    "stakeholder"        )
statement       = nc "statement"      (cn'    "statement"          )
symbol_         = nc "symbol"         (cn'    "symbol"             )
system          = nc "system"         (cn'    "system"             )
table_          = nc "table"          (cn'    "table"              )
terminology     = nc "terminology"    (cnIES  "terminology"        )
termAndDef      = nc "termAndDef"     (terminology `and_` definition)
tOfCont         = nc "tOfCont"        (table_ `of_` content)
tOfSymb         = nc "tOfSymb"        (table_ `of_` symbol_)
tOfUnit         = nc "tOfUnit"        (table_ `of_` unit_)
traceyMatrix    = nc "traceyMatrix"   (cnICES "traceability matrix")
useCase         = nc "useCase"        (cn'    "use case"           )
user            = nc "user"           (cn'    "user"               )

datumConstraint, productUC, functionalRequirement, generalSystemDescription, indPRCase,
  nonfunctionalRequirement, offShelfSolution, physicalSystem, problemDescription,
  prodUCTable, softwareReq, solutionCharacteristic, solutionCharSpec,
  specificsystemdescription, sysCont, systemConstraint, systemdescription,
  userCharacteristic :: IdeaDict
datumConstraint = compoundNCPP datum constraint
productUC                    = compoundNC product_ useCase
functionalRequirement        = compoundNC functional requirement
generalSystemDescription     = compoundNC general systemdescription
indPRCase                    = compoundNC individual productUC
nonfunctionalRequirement     = compoundNC nonfunctional requirement
offShelfSolution             = compoundNC offShelf solution
prodUCTable                  = compoundNC productUC table_
problemDescription           = compoundNC problem description
physicalSystem               = compoundNC physical system
softwareReq     = compoundNCPP software requirement
solutionCharSpec             = compoundNCPSPP solutionCharacteristic specification
solutionCharacteristic       = compoundNC solution characteristic
specificsystemdescription    = compoundNC specific systemdescription
sysCont                      = compoundNC system context
systemConstraint             = compoundNC system constraint
systemdescription            = compoundNC system description
userCharacteristic           = compoundNC user characteristic

traceyMandG :: IdeaDict
traceyMandG         = nc "traceyMandG"        (and_TGen (\t -> titleizeNP' (t ^. term)) (\t -> titleizeNP' (t ^. term)) traceyMatrix graph)

assumption, goalStmt, learnObj, likelyChg, notebook, physSyst, requirement, srs,
  unlikelyChg :: CI
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption")                                    "A"       [softEng]
goalStmt    = commonIdeaWithDict "goalStmt"    (combineNINI goal statement)                          "GS"      [softEng]
learnObj    = commonIdeaWithDict "learnObj"    (cn' "learning objective")                            "LO"      [documentc]
likelyChg   = commonIdeaWithDict "likelyChg"   (cn' "likely change")                                 "LC"      [softEng]
physSyst    = commonIdeaWithDict "physSyst"    (combineNINI physicalSystem description)              "PS"      [softEng]
notebook    = commonIdeaWithDict "notebook"    (cn' "notebook")                                      "NB"      [softEng]
requirement = commonIdeaWithDict "requirement" (cn' "requirement")                                   "R"       [softEng]
srs         = commonIdeaWithDict "srs"         softReqSpec                                           "SRS"     [softEng]
unlikelyChg = commonIdeaWithDict "unlikelyChg" (cn' "unlikely change")                               "UC"      [softEng]

scpOfTheProj :: (IdeaDict -> NPStruct) -> IdeaDict
scpOfTheProj oper = nc "scpOfTheProj" (scope `of_NINP` theGen oper project) -- reasonable hack?

