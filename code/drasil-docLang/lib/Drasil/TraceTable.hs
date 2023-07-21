{-# LANGUAGE LambdaCase #-}
-- | Defines a DLPlate for tracability between pieces of information.
module Drasil.TraceTable where

import Drasil.DocumentLanguage.Core

import Language.Drasil
import Language.Drasil.Development (lnames')
import Database.Drasil (TraceMap, traceMap)
import Theory.Drasil (Theory(..))

import Control.Lens ((^.))
import Data.Functor.Constant (Constant(Constant))
import Data.Generics.Multiplate (foldFor, preorderFold, purePlate)
import Data.List (transpose)

-- | Creates a dependency plate for 'UID's.
dependencyPlate :: DLPlate (Constant [(UID, [UID])])
dependencyPlate = preorderFold $ purePlate {
  pdSub = Constant <$> \case
    (Goals _ c) -> getDependenciesOf [defs] c
    _ -> [],
  scsSub = Constant <$> \case
    (Assumptions a) -> getDependenciesOf [defs] a
    (TMs _ _ t)     -> getDependenciesOf [\x -> map (^. defn) (x ^. defined_quant) ++
      map (^. defn) (x ^. operations), notes] t
    (DDs _ _ d _) -> getDependenciesOf [derivs, notes] d
    (GDs _ _ g _) -> getDependenciesOf [defs, derivs, notes] g
    (IMs _ _ i _) -> getDependenciesOf [derivs, notes] i
    _ -> [],
  reqSub = Constant <$> \case
    (FReqsSub c t) -> getDependenciesOf [defs] c ++
      map (\x -> getSourcesOf (x ^. accessContents)) t
    (NonFReqsSub c) -> getDependenciesOf [defs] c,
  lcsSec = Constant . getDependenciesOf [defs] <$> \(LCsProg c) -> c,
  ucsSec = Constant . getDependenciesOf [defs] <$> \(UCsProg c) -> c
} where
  getDependenciesOf :: HasUID a => [a -> [Sentence]] -> [a] -> [(UID, [UID])]
  getDependenciesOf fs = map (\x -> (x ^. uid, concatMap (lnames' . ($ x)) fs))
  defs :: Definition a => a -> [Sentence]
  defs x = [x ^. defn]
  derivs :: HasDerivation a => a -> [Sentence]
  derivs x = maybe [] (\(Derivation h d) -> h : d) $ x ^. derivations
  notes :: HasAdditionalNotes a => a -> [Sentence]
  notes = (^. getNotes)
  getSourcesOf :: RawContent -> (UID, [UID])
  getSourcesOf (Table s1 s2 t _) = (last $ lnames' [t], lnames' $ isSource (s1, transpose s2))
  getSourcesOf _ = error "Unexpected type of LabelledContent in Functional Requirement"
  isSource :: ([Sentence], [[Sentence]]) -> [Sentence]
  isSource (S "Source" : _, hd1 : _) = hd1
  isSource (_ : tl, _ : tl1) = isSource (tl, tl1)
  isSource ([], _) = []
  isSource (_, []) = []

-- | Creates a traceability map from document sections.
generateTraceMap :: [DocSection] -> TraceMap
generateTraceMap = traceMap . concatMap (foldFor docSec dependencyPlate)
