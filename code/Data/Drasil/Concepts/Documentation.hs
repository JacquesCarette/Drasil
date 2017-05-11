module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk.CommonIdea (CINP, commonINP)
import Language.Drasil.Chunk.NamedIdea (of', of_, npnc, NPNC, compoundNPNC, compoundNPNC')
import Data.Drasil.Concepts.Math (graph)
import Language.Drasil.NounPhrase

assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel, mg, vav, desSpec, constraint :: CINP
--FIXME: Add compound NounPhrases instead of cn'
assumption  = commonINP "assumption"  (cn' "assumption")                    "A"
dataDefn    = commonINP "dataDefn"    (cn' "data definition")               "DD"
desSpec     = commonINP "desSpec"     (cn' "design specification")          "DS"
genDefn     = commonINP "genDefn"     (cn' "general definition")            "GD"
goalStmt    = commonINP "goalStmt"    (cn' "goal statement")                "GS" 
inModel     = commonINP "inModel"     (cn' "instance model")                "IM" 
likelyChg   = commonINP "likelyChg"   (cn' "likely change")                 "LC"
physSyst    = commonINP "physSyst"    (cn' "physical system description")   "PS" 
requirement = commonINP "requirement" (cn' "requirement")                   "R"
thModel     = commonINP "thModel"     (cn' "theoretical model")             "T"
mg          = commonINP "mg"          (cn' "module guide")                  "MG" 
srs         = commonINP "srs"  (cn' "software requirements specification")  "SRS"
vav         = commonINP "vav"         (cn' "verification and validation")   "VAV"
constraint  = commonINP "constraint"  (cn' "constraint")                    "CSTR" -- FIXME: Eventually only have one constraint 

---------------------------------------------------------------------

-- concepts relating to the templates and their contents

appendix, characteristic, condition, constraint_, connection,
  datum, definition, dependency, description, design, document, figure, 
  functional, general, information, introduction, model, name_, nonfunctional, 
  offShelf, organization, physical, problem, program, purpose, quantity, 
  reference, requirement_, scope, section_, solution, specific, specification, 
  stakeholder, symbol_, system, table_, terminology, theory, unit_, user :: NPNC

appendix        = npnc "appendix"       (cnICES "appendix")
characteristic  = npnc "characteristic" (cn' "characteristic")
condition       = npnc "condition"      (cn' "condition")
constraint_     = npnc "constraint"     (cn' "constraint") -- FIXME: Eventually only have one constraint 
connection      = npnc "connection"     (cn' "connection")
datum           = npnc "datum"          (cnUM  "datum")
definition      = npnc "definition"     (cn' "definition")
dependency      = npnc "dependency"     (cnIES "dependency")
description     = npnc "description"    (cn' "description")
design          = npnc "design"         (cn' "design")
document        = npnc "document"       (cn' "document")
figure          = npnc "figure"         (cn' "figure")
functional      = npnc "functional"     (cn' "functional") --FIXME: Adjective
general         = npnc "general"        (cn' "general")  -- FIXME: Adjective
information     = npnc "information"    (cn "information")
introduction    = npnc "introduction"   (cn' "introduction")
model           = npnc "model"          (cn' "model")
name_           = npnc "name"           (cn' "name")
nonfunctional   = npnc "nonfunctional"  (cn' "nonfunctional") -- FIXME: Adjective
offShelf        = npnc "Off-the-Shelf"  (cn' "Off-the-Shelf")
organization    = npnc "organization"   (cn' "organization")
physical        = npnc "physical"       (cn' "physical") -- FIXME: Adjective
problem         = npnc "problem"        (cn' "problem")
program         = npnc "program"        (cn' "program")
purpose         = npnc "purpose"        (cn' "purpose")
quantity        = npnc "quantity"       (cnIES "quantity") --general enough to be in documentaion.hs?
reference       = npnc "reference"      (cn' "reference")
requirement_    = npnc "requirement"    (cn' "requirement") -- FIXME: Eventually only have one requirement 
scope           = npnc "scope"          (cn' "scope")
section_        = npnc "section"        (cn' "section")
solution        = npnc "solution"       (cn' "solution")
specific        = npnc "specific"       (cn' "specific") -- FIXME: Adjective
specification   = npnc "specification"  (cn' "specification")
stakeholder     = npnc "stakeholder"    (cn' "stakeholder")
symbol_         = npnc "symbol"         (cn' "symbol")
system          = npnc "system"         (cn' "system")
table_          = npnc "table"          (cn' "table")
terminology     = npnc "terminology"    (cnIES "terminology")
theory          = npnc "theory"         (cnIES "theory")
traceyMatrix    = npnc "traceyMatrix"   (cnICES "traceability matrix")
unit_           = npnc "unit"           (cn' "unit")
user            = npnc "user"           (cn' "user")

indPRCase, orgOfDoc, prodUCTable, prpsOfDoc, refmat, sciCompS, scpOfReq, tOfSymb, tOfUnits :: NPNC

indPRCase    = npnc "indPRCase"    (cn' "individual product use case")
orgOfDoc     = npnc "orgOfDoc"     (organization `of_` document)
prodUCTable  = npnc "prodUCTable"  (cn' "product use case table")
prpsOfDoc    = npnc "prpsOfDoc"    (purpose `of_` document)
refmat       = npnc "refmat"       (cn' "reference material")
sciCompS     = npnc "sciCompS"     (cn' "scientific computing software")
scpOfReq     = npnc "scpOfReq"     (scope `of'` requirement)
tOfSymb      = npnc "tOfSymb"      (table_ `of'` symbol_)
tOfUnits     = npnc "tOfUnits"     (table_ `of'` unit_)

-- compounds

characteristicSpecification, generalSystemDescription, physicalConstraint,
  physicalSystem, problemDescription, specificsystemdescription, 
  systemdescription, systemConstraint, userCharacteristic, datumConstraint,
  functionalRequirement, nonfunctionalRequirement, solutionCharSpec,
  offShelfSolution :: NPNC
  
characteristicSpecification  = compoundNPNC' characteristic specification
generalSystemDescription     = compoundNPNC general systemdescription
physicalConstraint           = compoundNPNC physical constraint_
physicalSystem               = compoundNPNC physical system
problemDescription           = compoundNPNC problem description
specificsystemdescription    = compoundNPNC specific systemdescription
systemdescription            = compoundNPNC system description
systemConstraint             = compoundNPNC system constraint_
userCharacteristic           = compoundNPNC user characteristic
datumConstraint              = compoundNPNC' datum constraint_
functionalRequirement        = compoundNPNC functional requirement_
nonfunctionalRequirement     = compoundNPNC nonfunctional requirement_
solutionCharSpec             = compoundNPNC solution characteristicSpecification
offShelfSolution             = compoundNPNC offShelf solution