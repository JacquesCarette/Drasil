module Drasil.Code.CodeExpr.Extract (
    eDep, eDep',
    eNamesRI, eNamesRI'
) where

import Data.Containers.ListUtils (nubOrd)

import Drasil.Database (UID)

import Drasil.Code.CodeExpr.Lang (CodeExpr, eNames, eNames', eNamesRI, eNamesRI')

-- | Get dependencies from an equation.
eDep :: CodeExpr -> [UID]
eDep = nubOrd . eNames

-- | Get dependencies from an equation, without functions.
eDep' :: CodeExpr -> [UID]
eDep' = nubOrd . eNames'
