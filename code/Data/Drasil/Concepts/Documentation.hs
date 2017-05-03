module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk.CommonIdea (CINP, commonINP)
import Language.Drasil.Chunk.NamedIdea (of', of_, npnc, NPNC, compoundNPNC)
import Language.Drasil.NounPhrase

assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel, mg :: CINP
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
srs         = commonINP "srs"         (cn' "software requirements specification") 
  "SRS"

---------------------------------------------------------------------

-- concepts relating to the templates and their contents

section_, system, description, specific, general, symbol_, units_, 
  table_, introduction, organization, document, purpose, characteristics, specification :: NPNC
section_      = npnc "section"      (cn' "section")
system       = npnc "system"       (cn' "system")
description  = npnc "description"  (cn' "description")
specific     = npnc "specific"     (cn' "specific") -- ??
general      = npnc "general"      (cn' "general")
symbol_      = npnc "symbol"       (cn' "symbol")
units_       = npnc "units"        (cn' "units")
table_       = npnc "table"        (cn' "table")
introduction = npnc "introduction" (cn' "introduction")
organization = npnc "organization" (cn' "organization")
document     = npnc "document"     (cn' "document")
purpose      = npnc "purpose"      (cn' "purpose")
characteristics = npnc "characteristics" (cn' "characteristics")
specification = npnc "specification" (cn' "specification")

tOfSymb, refmat, orgOfDoc, prpsOfDoc :: NPNC

refmat       = npnc "refmat"       (cn' "reference material")
tOfSymb      = npnc "tOfSymb"      (table_ `of'` symbol_)
orgOfDoc     = npnc "orgOfDoc"     (organization `of_` document)
prpsOfDoc    = npnc "prpsOfDoc"    (purpose `of_` document)

-- compounds
systemdescription, specificsystemdescription, characteristicsSpecification, generalSystemDescription :: NPNC
systemdescription         = compoundNPNC system   description
specificsystemdescription = compoundNPNC specific systemdescription
generalSystemDescription = compoundNPNC general systemdescription
characteristicsSpecification = compoundNPNC characteristics specification