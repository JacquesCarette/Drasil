{-# LANGUAGE LambdaCase #-}
module Drasil.TraceTable where

import Drasil.DocumentLanguage.Core

import Language.Drasil
import Language.Drasil.Development (lnames')
import Database.Drasil (TraceMap, traceMap)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel, Theory(..))

import Control.Lens ((^.))
import Data.Functor.Constant (Constant(Constant))
import Data.Generics.Multiplate (foldFor, preorderFold, purePlate)
import Data.Maybe (mapMaybe)

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

getTraceMapFromDocSec :: [DocSection] -> SSDSec
getTraceMapFromDocSec (SSDSec ssd:_) = ssd
getTraceMapFromDocSec (_:tl)         = getTraceMapFromDocSec tl
getTraceMapFromDocSec []             = error "No SSDSec found."

getTraceMapFromSSDSec :: SSDSec -> [SSDSub]
getTraceMapFromSSDSec (SSDProg s) = s

getTraceMapFromSSDSub :: [SSDSub] -> SolChSpec
getTraceMapFromSSDSub (SSDSolChSpec s:_) = s
getTraceMapFromSSDSub (_:tl)             = getTraceMapFromSSDSub tl
getTraceMapFromSSDSub []                 = error "No SolChSpec found."

getTraceMapFromSolCh :: SolChSpec -> [SCSSub]
getTraceMapFromSolCh (SCSProg s) = s

getTraceMapFromTM :: [SCSSub] -> [TheoryModel]
getTraceMapFromTM (TMs _ _ t:_) = t
getTraceMapFromTM (_:tl)        = getTraceMapFromTM tl
getTraceMapFromTM []            = error "No TM found."

getTraceMapFromGD :: [SCSSub] -> [GenDefn]
getTraceMapFromGD (GDs _ _ gd _:_) = gd
getTraceMapFromGD (_:tl)           = getTraceMapFromGD tl
getTraceMapFromGD []               = []

getTraceMapFromDD :: [SCSSub] -> [DataDefinition]
getTraceMapFromDD l = concat $ mapMaybe getDD l
  where getDD (DDs _ _ d _) = Just d
        getDD _           = Nothing

getTraceMapFromIM :: [SCSSub] -> [InstanceModel]
getTraceMapFromIM (IMs _ _ imod _:_) = imod
getTraceMapFromIM (_:tl)             = getTraceMapFromIM tl
getTraceMapFromIM []                 = []

extractSFromNotes :: HasAdditionalNotes l => l -> [Sentence]
extractSFromNotes c = c ^. getNotes

extractSFromDeriv :: HasDerivation l => l -> [Sentence]
extractSFromDeriv c = c ^. derivations

getSCSSub :: [DocSection] -> [SCSSub]
getSCSSub a = getTraceMapFromSolCh $ getTraceMapFromSSDSub $ getTraceMapFromSSDSec
 $ getTraceMapFromDocSec a

generateTraceMap :: [DocSection] -> TraceMap
generateTraceMap = traceMap . concatMap (foldFor docSec dependencyPlate)

-- This is a hack as ConceptInstance cannot be collected yet.
generateTraceMap' :: [ConceptInstance] -> TraceMap
generateTraceMap' = traceMap . map (\x -> (x ^. uid, lnames' [x ^. defn]))
