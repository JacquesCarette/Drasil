{-# LANGUAGE LambdaCase #-}
module Drasil.TraceTable where

import Drasil.DocumentLanguage.Core

import Language.Drasil
import Language.Drasil.Development (lnames')
import Database.Drasil (TraceMap, traceMap)
import Theory.Drasil (Theory(..))

import Control.Lens ((^.))
import Data.Functor.Constant (Constant(Constant))
import Data.Generics.Multiplate (foldFor, preorderFold, purePlate)

dependencyPlate :: DLPlate (Constant [(UID, [UID])])
dependencyPlate = preorderFold $ purePlate {
  pdSub = Constant <$> \case
    (Goals _ c) -> getDependenciesOf [defs] c
    _ -> [],
  scsSub = Constant <$> \case
    (Assumptions a) -> getDependenciesOf [defs] a
    (TMs _ _ t) -> getDependenciesOf [\x -> map (^. defn) (x ^. defined_quant) ++
      map (^. defn) (x ^. operations), notes] t
    (DDs _ _ d _) -> getDependenciesOf [derivs, notes] d
    (GDs _ _ g _) -> getDependenciesOf [defs, derivs, notes] g
    (IMs _ _ i _) -> getDependenciesOf [derivs, notes] i
    _ -> [],
  reqSub = Constant . getDependenciesOf [defs] <$> \case
    (FReqsSub c _) -> c
    (NonFReqsSub c) -> c,
  lcsSec = Constant . getDependenciesOf [defs] <$> \(LCsProg c) -> c,
  ucsSec = Constant . getDependenciesOf [defs] <$> \(UCsProg c) -> c
} where
  getDependenciesOf :: HasUID a => [a -> [Sentence]] -> [a] -> [(UID, [UID])]
  getDependenciesOf fs = map (\x -> (x ^. uid, concatMap (lnames' . ($ x)) fs))
  defs :: Definition a => a -> [Sentence]
  defs x = [x ^. defn]
  derivs :: HasDerivation a => a -> [Sentence]
  derivs = (^. derivations)
  notes :: HasAdditionalNotes a => a -> [Sentence]
  notes = (^. getNotes)

generateTraceMap :: [DocSection] -> TraceMap
generateTraceMap = traceMap . concatMap (foldFor docSec dependencyPlate)
