module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk.CommonIdea (CINP, commonINP)
import Language.Drasil.Chunk.NamedIdea (of', of_, npnc, NPNC, compoundNPNC)
import Language.Drasil.NounPhrase

assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel, mg, vav :: CINP
--FIXME: Add compound NounPhrases instead of cn'
assumption  = commonINP "assumption"  (cn' "assumption")                    "A"
dataDefn    = commonINP "dataDefn"    (cn' "data definition")               "DD"
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

---------------------------------------------------------------------

-- concepts relating to the templates and their contents

section_, physical, system, description, specific, general, symbol_, units_, 
  table_, introduction, organization, document, purpose, characteristics, characteristic, specification,
    unit_, problem, theory, definition, model, information :: NPNC
section_        = npnc "section"        (cn' "section")
physical        = npnc "physical"       (cn' "physical")
system          = npnc "system"         (cn' "system")
description     = npnc "description"    (cn' "description")
specific        = npnc "specific"       (cn' "specific") -- ??
general         = npnc "general"        (cn' "general")  --FIXME: adjective?
symbol_         = npnc "symbol"         (cn' "symbol")
units_          = npnc "units"          (cn' "units") -- FIXME: Eventually this plural version needs to be removed
unit_           = npnc "unit"           (cn' "unit")
table_          = npnc "table"          (cn' "table")
introduction    = npnc "introduction"   (cn' "introduction")
organization    = npnc "organization"   (cn' "organization")
document        = npnc "document"       (cn' "document")
purpose         = npnc "purpose"        (cn' "purpose")
characteristics = npnc "characteristics" (cn' "characteristics") --FIXME: Eventually this plural version needs to be removed
characteristic  = npnc "characteristic" (cn' "characteristic")
specification   = npnc "specification"  (cn' "specification")
problem         = npnc "problem"        (cn' "problem")
theory          = npnc "theory"         (cnIES "theory")
definition      = npnc "definition"     (cn' "definition")
model           = npnc "model"          (cn' "model")
information     = npnc "information"    (cn "information")

tOfSymb, refmat, orgOfDoc, prpsOfDoc, tOfUnits, sciCompS :: NPNC

refmat       = npnc "refmat"       (cn' "reference material")
sciCompS     = npnc "sciCompS"      (cn' "scientific computing software")
tOfSymb      = npnc "tOfSymb"      (table_ `of'` symbol_)
orgOfDoc     = npnc "orgOfDoc"     (organization `of_` document)
prpsOfDoc    = npnc "prpsOfDoc"    (purpose `of_` document)
tOfUnits     = npnc "tOfUnits"     (table_ `of'` unit_)

-- compounds
systemdescription, specificsystemdescription, characteristicsSpecification, 
  physicalSystem, generalSystemDescription, problemDescription :: NPNC
systemdescription            = compoundNPNC system description
specificsystemdescription    = compoundNPNC specific systemdescription
generalSystemDescription     = compoundNPNC general systemdescription
characteristicsSpecification = compoundNPNC characteristics specification
physicalSystem               = compoundNPNC physical system
problemDescription           = compoundNPNC problem description