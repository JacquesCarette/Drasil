-- | Defines functions to find Chunk UIDs within 'ModelExpr's.
module Language.Drasil.ModelExpr.Extract (meDep) where

import Data.Containers.ListUtils (nubOrd)

import Drasil.Database (UID)

import Language.Drasil.ModelExpr.Lang (ModelExpr, meNames)

-- | Get dependencies from an equation.
meDep :: ModelExpr -> [UID]
meDep = nubOrd . meNames
