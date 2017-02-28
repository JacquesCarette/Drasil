module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk.CommonIdea (CI, commonidea)
import Language.Drasil.Chunk.NamedIdea (NamedChunk, nc, compoundterm, of_, ncs)

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
section, system, description, specific, symbol_, units_, table_,
  introduction :: NamedChunk
section      = nc "section"      "Section"
system       = nc "system"       "System"
description  = nc "description"  "Description"
specific     = nc "specific"     "Specific" -- ??
symbol_      = nc "symbol"       "Symbol"
symbols      = nc "symbols"      "Symbols" -- Hack!
units_       = nc "units"        "Units"
table_       = nc "table"        "Table"
introduction = nc "introduction" "Introduction"

refmat, tOfSymb :: NamedChunk
refmat       = nc  "refmat"      "Reference Material"
tOfSymb      = ncs "tOfSymb"   $ table_ `of_` symbols

-- compounds
systemdescription, specificsystemdescription :: NamedChunk
systemdescription         = compoundterm system   description
specificsystemdescription = compoundterm specific systemdescription

