module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk.NamedIdea


--FIXME: Use actual Acronyms instead of CCs.
assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel, mg :: NamedChunk

--FIXME: These shouldn't be ConceptChunks or NamedIdeas, but should be
--    Some subset of them, no?
assumption  = nc' "assumption" "Assumption" "A"
dataDefn    = nc' "dataDefn" "Data Definition" "DD"
genDefn     = nc' "genDefn" "General Definition" "GD"
goalStmt    = nc' "goalStmt" "Goal Statement" "GS" 
inModel     = nc' "inModel" "Instance Model" "IM" 
likelyChg   = nc' "likelyChg" "Likely Change" "LC"
physSyst    = nc' "physSyst" "Physical System Description" "PS" 
requirement = nc' "requirement" "Requirement" "R"
srs         = nc' "srs" "Software Requirements Specification" "SRS"
thModel     = nc' "thModel" "Theoretical Model" "T"
mg          = nc' "mg" "Module Guide" "MG" 

---------------------------------------------------------------------

-- concepts relating to the templates and their contents
section, system, description, specific, symbol_, units_, table_ :: NamedChunk
section = nc "section" "Section"
system = nc "system" "System"
description = nc "description" "Description"
specific = nc "specific" "Specific" -- ??
symbol_ = nc "symbol" "Symbol"
units_ = nc "units" "Units"
table_ = nc "table" "Table"

refmat, tOfSymb :: NamedChunk
refmat = nc "refmat" "Reference Material"
tOfSymb = nc "tOfSymb" "Table of Symbols"

-- compounds
systemdescription, specificsystemdescription :: NamedChunk
systemdescription = compoundterm system description
specificsystemdescription = compoundterm specific systemdescription

