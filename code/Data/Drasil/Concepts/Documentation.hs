module Data.Drasil.Concepts.Documentation where

import Language.Drasil

--FIXME: Use actual Acronyms instead of CCs.

assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel :: NamedChunk

assumption  = makeCC "A" "Assumption"
dataDefn    = makeCC "DD" "Data Definition"
genDefn     = makeCC "GD" "General Definition"
goalStmt    = makeCC "GS" "Goal Statement"
inModel     = makeCC "IM" "Instance Model"
likelyChg   = makeCC "LC" "Likely Change"
physSyst    = makeCC "PS" "Physical System Description"
requirement = makeCC "R" "Requirement"
srs         = makeCC "SRS" "Software Requirements Specification"
thModel     = makeCC "T" "Theoretical Model"