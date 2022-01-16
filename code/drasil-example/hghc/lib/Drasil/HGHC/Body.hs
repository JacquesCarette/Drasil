module Drasil.HGHC.Body (srs, si, symbMap, printSetting, fullSI) where

import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.SRSDocument
import qualified Utils.Drasil.Sentence as S

import Drasil.HGHC.HeatTransfer (fp, hghc, dataDefs, htInputs, htOutputs, 
    nuclearPhys, symbols)

import Data.Drasil.SI_Units (siUnits, fundamentals, derived, degree)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (doccon, doccon')
import Data.Drasil.Concepts.Math (mathcon)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
  
srs :: Document
srs = mkDoc mkSRS S.forT si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

si :: SystemInformation
si = SI {
  _sys         = hghc,
  _kind        = Doc.srs,
  _authors     = [spencerSmith],
  _quants      = symbols,
  _purpose     = [],
  _concepts    = [] :: [UnitalChunk],
  _instModels  = [], -- FIXME; empty _instModels
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = htInputs,
  _outputs     = htOutputs,
  _defSequence = [] :: [Block SimpleQDef],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = rdb [] [] -- FIXME?
}

{-
tocTree :: Tree D.SRSSection
tocTree = 
  Node D.TableOfContents [
    Node D.RefSec [
      Node D.TUnits [],
      Node D.TSymb []],
    Node D.SSDSec [
      Node D.SolChSpec [
        Node D.DDs []]]
  ]
-}

toC :: [SRSSection]
toC = [TableContents,
    Ref' [
      TU, 
      TS],
    SSD [
      SolChSpec' [
        DD]]
  ]

mkSRS :: SRSDecl
mkSRS = [
  TableOfContents $ ToCProg toC,
  RefSec $ RefProg intro,
    TUnits TUProg, 
    TSymb $ tsymb [TSPurpose, SymbConvention [Lit $ nw nuclearPhys, Manual $ nw fp]],
  SSDSec $ SSDProg EmptyS,
    SolChSpec $ SCSProg EmptyS,
      DDs $ DDProg [] [Label, Symbol, Units, DefiningEquation, Description Verbose IncludeUnits] HideDerivation]

symbMap :: ChunkDB
symbMap = cdb symbols (map nw symbols ++ map nw doccon ++ map nw fundamentals ++ map nw derived
  ++ [nw fp, nw nuclearPhys, nw hghc, nw degree] ++ map nw doccon' ++ map nw mathcon)
  ([] :: [ConceptChunk])-- FIXME: Fill in concepts
  siUnits dataDefs [] [] [] [] [] [] []

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols)
           ([] :: [ConceptChunk]) ([] :: [UnitDefn])
           [] [] [] [] [] [] [] ([] :: [Reference])