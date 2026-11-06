-- | Converts a 'SmithEtAlSRS' into a 'CodeSpec'.
module Drasil.Generator.Code.CodeSpec (
  srs2CodeSpec
) where

import Control.Lens ((^.), set)
import Data.List ((\\))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Data.List.Extras (subsetOf)
import Drasil.Code.CodeExpr.Development (expr)
import Drasil.Database (ChunkDB, HasUID(..), insertAll)
import qualified Drasil.SRS as S
import Drasil.SRS (HasSmithEtAlSRS(..))
import Drasil.System (HasSystemMeta(..), systemdb)
import Language.Drasil (SimpleQDef, DefiningExpr(defnExpr))
import Language.Drasil.Code (Choices(extLibs, maps, handWiredDefs,
  defaultConfigFiles, extraMods), Maps(conceptMatch), CodeSpec, Input, Const,
  mkCodeSpec, getODE, mapODE, quantvar, qtov, qtoc, constraintMap, codevars,
  odeInfoReqs, odeLibReqs)
import Theory.Drasil (DataDefinition, qdEFromDD, getEqModQdsFromIm)

import Drasil.Generator.Code.ICOSolutionSearch (solveExecOrder)

-- | Creates a 'CodeSpec' from a 'SmithEtAlSRS' and 'Choices'.
srs2CodeSpec :: S.SmithEtAlSRS -> Choices -> CodeSpec
srs2CodeSpec si@S.ICO{ S._inputs = ins
                      , S._outputs = outs
                      , S._constraints = cs
                      , S._constants = cnsts } chs =
  let els = extLibs chs
      libReqs = concatMap odeLibReqs els
      infoReqs = concatMap odeInfoReqs els
      db = insertAll (libReqs <> infoReqs) $ si ^. systemdb
      -- FIXME: This _should_ be different in at least one way. The SM from
      -- the SRS _should_ reference "defining an SRS that ...". This SM should
      -- reference said SRS, saying "building a software design satisfying
      -- said SRS."
      sysMeta = set systemdb db $ si ^. systemMeta
      ddefs = si ^. dataDefns
      inputs' = quantvar <$> NE.toList ins
      const' = qtov <$> filter ((`Map.notMember` conceptMatch (maps chs)) . (^. uid))
        cnsts
      derived = qtov <$> getDerivedInputs ddefs inputs' const' db
      rels = (fmap qtoc (getEqModQdsFromIm (si ^. instModels) <> mapMaybe qdEFromDD ddefs) \\ derived)
        <> mapODE (getODE $ extLibs chs)
        <> fmap qtoc (handWiredDefs chs)
      -- TODO: When we have better DEModels, we should be deriving our ODE information
      --       directly from the instance models (ims) instead of directly from the choices.
      outs' = quantvar <$> NE.toList outs
      allInputs = inputs' <> fmap quantvar derived
      exOrder = solveExecOrder rels (allInputs <> fmap quantvar cnsts) outs' db
  in mkCodeSpec sysMeta inputs' derived outs' (defaultConfigFiles chs) exOrder
       (constraintMap cs) const' (extraMods chs)

-- | Determines the derived inputs: the data definitions that can be calculated
-- immediately from the knowns (inputs and constants). Only data definitions
-- defined by an 'Expr' are considered (see 'qdEFromDD').
getDerivedInputs :: [DataDefinition] -> [Input] -> [Const] ->
  ChunkDB -> [SimpleQDef]
getDerivedInputs ddefs ins cnsts db =
  filter ((`subsetOf` refSet) . flip codevars db . expr . (^. defnExpr)) (mapMaybe qdEFromDD ddefs)
  where refSet = ins <> fmap quantvar cnsts
