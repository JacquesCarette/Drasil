{-# LANGUAGE GADTs, TemplateHaskell #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.TraceTable where

import Control.Lens ((^.))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Language.Drasil
import Language.Drasil.Development (lnames')

import Drasil.DocumentLanguage


traceMap' :: HasUID l => (l -> [Sentence]) -> [l] -> TraceMap
traceMap' f = traceMap $ lnames' . f

getTraceMapFromDocSec :: [DocSection] -> SSDSec
getTraceMapFromDocSec ((SSDSec ssd):_)  = ssd
getTraceMapFromDocSec  (_:tl)          = getTraceMapFromDocSec tl
getTraceMapFromDocSec []                = error "No SSDSec found."

getTraceMapFromSSDSec :: SSDSec -> [SSDSub]
getTraceMapFromSSDSec (SSDProg s)       = s

getTraceMapFromSSDSub :: [SSDSub] -> SolChSpec
getTraceMapFromSSDSub ((SSDSolChSpec s):_) = s
getTraceMapFromSSDSub (_:tl)              = getTraceMapFromSSDSub tl
getTraceMapFromSSDSub []                    = error "No SolChSpec found."

getTraceMapFromSolCh :: SolChSpec -> [SCSSub]
getTraceMapFromSolCh (SCSProg s) = s

getTraceMapFromTM :: [SCSSub] -> [TheoryModel]
getTraceMapFromTM ((TMs _ t):_)     = t
getTraceMapFromTM  (_:tl)           = getTraceMapFromTM tl
getTraceMapFromTM []                = error "No TM found."

getTraceMapFromGD :: [SCSSub] -> [GenDefn]
getTraceMapFromGD ((GDs _ gd _):_)  = gd
getTraceMapFromGD  (_:tl)           = getTraceMapFromGD tl
getTraceMapFromGD []                = []

getTraceMapFromDD :: [SCSSub] -> [DataDefinition]
getTraceMapFromDD l = concat $ mapMaybe getDD l
  where getDD (DDs _ d _) = Just d
        getDD _           = Nothing

getTraceMapFromIM :: [SCSSub] -> [InstanceModel]
getTraceMapFromIM ((IMs _ im _):_)  = im
getTraceMapFromIM  (_:tl)           = getTraceMapFromIM tl
getTraceMapFromIM []                = []

extractSFromNotes :: HasAdditionalNotes l => l -> [Sentence]
extractSFromNotes c = c ^. getNotes

extractSFromDeriv :: HasDerivation l => l -> [Sentence]
extractSFromDeriv c = c ^. derivations

getSCSSub :: [DocSection] -> [SCSSub]
getSCSSub a = getTraceMapFromSolCh $ getTraceMapFromSSDSub $ getTraceMapFromSSDSec
 $ getTraceMapFromDocSec a

generateTraceMap :: [DocSection] -> TraceMap
generateTraceMap a = Map.unionsWith (\(w,x) (y,z) -> (w ++ y, ordering x z)) [
  (traceMap' extractSFromNotes tt), (traceMap' extractSFromNotes gd),
  (traceMap' extractSFromNotes dd), (traceMap' extractSFromNotes im),
  -- Theory models do not have derivations.
  (traceMap' extractSFromDeriv gd),
  (traceMap' extractSFromDeriv dd), (traceMap' extractSFromDeriv im)]
  where
    tt = getTraceMapFromTM $ getSCSSub a
    gd = getTraceMapFromGD $ getSCSSub a
    im = getTraceMapFromIM $ getSCSSub a
    dd = getTraceMapFromDD $ getSCSSub a
    ordering x y = if x == y then x else error "Expected ordering between smaller TraceMaps to be the same"

-- This is a hack as ConceptInstance cannot be collected yet.
generateTraceMap' :: [ConceptInstance] -> TraceMap
generateTraceMap' = traceMap' (\x -> [x ^. defn])
