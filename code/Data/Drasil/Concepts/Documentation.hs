module Data.Drasil.Concepts.Documentation where

import Language.Drasil.Chunk


--FIXME: Use actual Acronyms instead of CCs.
assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel, mg :: ConceptChunk

--FIXME: These shouldn't be ConceptChunks or NamedIdeas, but should be
--    Some subset of them, no?
assumption  = dcc "assumption" "A" "Assumption"
dataDefn    = dcc "dataDefn" "DD" "Data Definition"
genDefn     = dcc "genDefn" "GD" "General Definition"
goalStmt    = dcc "goalStmt" "GS" "Goal Statement"
inModel     = dcc "inModel" "IM" "Instance Model"
likelyChg   = dcc "likelyChg" "LC" "Likely Change"
physSyst    = dcc "physSyst" "PS" "Physical System Description"
requirement = dcc "requirement" "R" "Requirement"
srs         = dcc "srs" "SRS" "Software Requirements Specification"
thModel     = dcc "thModel" "T" "Theoretical Model"
mg          = dcc "mg" "MG" "Module Guide"

---------------------------------------------------------------------

-- concepts relating to the templates and their contents
section, system, description, specific, symbol_, units_, table_ :: NamedChunk
section = makeCC "section" "Section"
system = makeCC "system" "System"
description = makeCC "description" "Description"
specific = makeCC "specific" "Specific" -- ??
symbol_ = makeCC "symbol" "Symbol"
units_ = makeCC "units" "Units"
table_ = makeCC "table" "Table"

refmat, tsymb :: NamedChunk
refmat = makeCC "refmat" "Reference Material"
tsymb = makeCC "tsymb" "Table of Symbols"

-- compounds
systemdescription, specificsystemdescription :: NamedChunk
systemdescription = compoundterm system description
specificsystemdescription = compoundterm specific systemdescription

