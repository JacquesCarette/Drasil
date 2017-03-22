module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk.CommonIdea (CI, commonidea)
import Language.Drasil.Chunk.NamedIdea (NamedChunk, nc, of'
                                       , ncs, npnc, NPNC, compoundNPNC)
import Language.Drasil.NounPhrase

assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel, mg :: CI

assumption  = commonidea "assumption"  "Assumption"                          "A"
dataDefn    = commonidea "dataDefn"    "Data Definition"                     "DD"
genDefn     = commonidea "genDefn"     "General Definition"                  "GD"
goalStmt    = commonidea "goalStmt"    "Goal Statement"                      "GS" 
inModel     = commonidea "inModel"     "Instance Model"                      "IM" 
likelyChg   = commonidea "likelyChg"   "Likely Change"                       "LC"
physSyst    = commonidea "physSyst"    "Physical System Description"         "PS" 
requirement = commonidea "requirement" "Requirement"                         "R"
srs         = commonidea "srs"         "Software Requirements Specification" "SRS"
thModel     = commonidea "thModel"     "Theoretical Model"                   "T"
mg          = commonidea "mg"          "Module Guide"                        "MG" 

---------------------------------------------------------------------

-- concepts relating to the templates and their contents

section, system, description, specific, symbol_, units_, 
  table_, introduction:: NPNC
section      = npnc "section"      (cn' "section")
system       = npnc "system"       (cn' "system")
description  = npnc "description"  (cn' "description")
specific     = npnc "specific"     (cn' "specific") -- ??
symbol_      = npnc "symbol"       (cn' "symbol")
units_       = npnc "units"        (cn' "units")
table_       = npnc "table"        (cn' "table")
introduction = npnc "introduction" (cn' "introduction")

tOfSymb, refmat :: NamedChunk

refmat       = nc  "refmat"      "Reference Material"
tOfSymb      = ncs "tOfSymb"   ((titleize table_) `of'` (titleize' symbol_))

-- compounds
systemdescription, specificsystemdescription  :: NPNC
systemdescription         = compoundNPNC system   description
specificsystemdescription = compoundNPNC specific systemdescription

