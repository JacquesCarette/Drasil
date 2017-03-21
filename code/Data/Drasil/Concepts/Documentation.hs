module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk.CommonIdea (CI, commonidea)
import Language.Drasil.Chunk.NamedIdea (NamedChunk, nc, of_
                                       , ncs, npnc, NPNC, compoundNPNCTitle)
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
table_, introduction, symbols :: NamedChunk
section, system, description, specific, symbol_, units_ :: NPNC
section      = npnc "section"     (cn' "section")
system       = npnc "system"      (cn' "system")
description  = npnc "description" (cn' "description")
specific     = npnc "specific"    (cn' "specific") -- ??
symbol_      = npnc "symbol"      (cn' "symbol")
symbols      = nc "symbols"      "Symbols" -- Hack!
units_       = npnc "units"       (cn' "units")
table_       = nc "table"        "Table"
introduction = nc "introduction" "Introduction"

refmat, tOfSymb :: NamedChunk
refmat       = nc  "refmat"      "Reference Material"
tOfSymb      = ncs "tOfSymb"   $ table_ `of_` symbols

-- compounds
systemdescription, specificsystemdescription  :: NPNC
systemdescription         = compoundNPNCTitle system   description
specificsystemdescription = compoundNPNCTitle specific systemdescription

