{-# Language TupleSections #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage (mkDoc, fillcdbSRS, findAllRefs) where

import Control.Lens ((^.), set)
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Maybe (maybeToList, mapMaybe)
import qualified Data.Map as Map (keys)

import Utils.Drasil (invert)

import Drasil.DocDecl (SRSDecl, mkDocDesc)
import qualified Drasil.DocDecl as DD
import Drasil.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..),
  DerivationDisplay(..), DocDesc, DocSection(..), OffShelfSolnsSec(..), GSDSec(..),
  GSDSub(..), IntroSec(..), IntroSub(..), LCsSec(..), LFunc(..),
  PDSub(..), ProblemDescription(..), RefSec(..), RefTab(..), ReqrmntSec(..),
  ReqsSub(..), SCSSub(..), StkhldrSec(..), StkhldrSub(..), SolChSpec(..),
  SSDSec(..), SSDSub(..), TraceabilitySec(..), TraceConfig(..),
  TSIntro(..), UCsSec(..), getTraceConfigUID)
import Drasil.DocumentLanguage.Definitions (ddefn, derivation, instanceModel,
  gdefn, tmodel)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)
import Drasil.TraceTable (generateTraceMap)

import Language.Drasil hiding (kind)
import Language.Drasil.Display (compsy)

import Drasil.Database (findOrErr, ChunkDB(..), insertAll)
import Drasil.Database.SearchTools (findAllDataDefns, findAllGenDefns,
  findAllInstMods, findAllTheoryMods, findAllConcInsts, findAllLabelledContent)

import Drasil.System (System(SI), whatsTheBigIdea, _sys, _systemdb, _quants, _authors, refTable, refbyTable, traceTable, systemdb)
import Drasil.GetChunks (ccss, ccss', citeDB)

import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccGen)
import Drasil.Sections.TableOfContents (toToC)
import Drasil.Sections.TableOfSymbols (table, tsIntro)
import Drasil.Sections.TableOfUnits (tOfUnitSIName, tuIntro, defaultTUI)
import qualified Drasil.DocLang.SRS as SRS (appendix,
  genSysDes, likeChg, unlikeChg, reference, solCharSpec,
  stakeholder, tOfCont, tOfSymb, tOfUnit, userChar, offShelfSol, refMat,
  tOfAbbAcc)
import Drasil.DocLang.References (secRefs)
import qualified Drasil.Sections.AuxiliaryConstants as AC (valsOfAuxConstantsF)
import qualified Drasil.Sections.GeneralSystDesc as GSD (genSysIntro,
  systCon, usrCharsF, sysContxt)
import qualified Drasil.Sections.Introduction as Intro (charIntRdrF,
  introductionSection, orgSec, purposeOfDoc, scopeOfRequirements)
import qualified Drasil.Sections.Requirements as R (reqF, fReqF, nfReqF)
import qualified Drasil.Sections.SpecificSystemDescription as SSD (assumpF,
  datConF, dataDefnF, genDefnF, goalStmtF, inModelF, physSystDesc, probDescF,
  propCorSolF, solutionCharSpecIntro, specSysDescr, termDefnF, thModF, helperCI,
  tmStub, ddStub, gdStub, imStub, pdStub)
import qualified Drasil.Sections.Stakeholders as Stk (stakeholderIntro,
  tClientF, tCustomerF)
import qualified Drasil.DocumentLanguage.TraceabilityMatrix as TM (
  generateTraceTableView, traceMHeader, layoutUIDs)
import qualified Drasil.DocumentLanguage.TraceabilityGraph as TG (traceMGF)
import Drasil.DocumentLanguage.TraceabilityGraph (traceyGraphGetRefs, genTraceGraphLabCons)
import Drasil.Sections.TraceabilityMandGs (traceMatStandard)
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)

import qualified Data.Drasil.Concepts.Documentation as Doc (likelyChg, section_,
  software, unlikelyChg)
import qualified Data.Map.Strict as M

-- * Main Function

-- | Creates a document from a document description, a title combinator function, and system information.
mkDoc :: SRSDecl -> (IdeaDict -> IdeaDict -> Sentence) -> (System, DocDesc) -> Document
mkDoc srs comb (si@SI {_sys = sys, _authors = docauthors}, dd) =
  Document (whatsTheBigIdea si `comb` nw sys) (foldlList Comma List $ map (S . name) docauthors)
    (findToC srs) $ mkSections si dd

-- * Functions to Fill 'ChunkDB'

-- FIXME: None of these 'fill$x$InTheCDB' should exist here. Generating new
-- chunks and inserting them into the ChunkDB is not an issue, but:
-- - The traceability-stuff should be internal to ChunkDB.
-- - The References and LabelledContent entirely need to be rebuilt. Some will
--   be chunks that are manually written, others will not be chunks.

-- | Assuming a given 'ChunkDB' with no traces and minimal/no references, fill
-- in for rest of system information. Currently fills in references,
-- traceability matrix information and 'IdeaDict's.
fillcdbSRS :: SRSDecl -> System -> (System , DocDesc)
fillcdbSRS srsDec si =
  (fillLC dd $ fillReferences sections $ fillTraceMaps dd si , dd)
  where
    dd :: DocDesc
    dd = mkDocDesc si srsDec
    sections :: [Section]
    sections = mkSections si dd

-- | Fill in the 'Section's and 'LabelledContent' maps of the 'ChunkDB' from the 'SRSDecl'.
fillLC :: DocDesc -> System -> System
fillLC sd si@SI{ _sys = sn }
  | containsTraceSec sd = si2
  | otherwise = si
  where
    chkdb = si ^. systemdb
    -- Pre-generate a copy of all required LabelledContents (i.e., traceability
    -- graphs) for insertion in the ChunkDB.
    createdLCs = genTraceGraphLabCons $ programName sn
    -- FIXME: This is a semi-hack. This is only strictly necessary for the
    -- traceability graphs. Those are all chunks that should exist but not be
    -- handled like this. They should be created and included in the
    -- meta-ChunkDB of `drasil-docLang`.
    chkdb2 = insertAll createdLCs chkdb
    si2 = set systemdb chkdb2 si

    containsTraceSec :: DocDesc -> Bool
    containsTraceSec ((TraceabilitySec _):_) = True
    containsTraceSec (_:ss)                = containsTraceSec ss
    containsTraceSec []                    = False

-- | Takes in existing information from the Chunk database to construct a database of references.
fillReferences :: [Section] -> System -> System
fillReferences allSections si@SI{_sys = sys} = si2
  where
    -- get old chunk database + ref database
    chkdb = si ^. systemdb
    cites = citeDB si
    -- get refs from SRSDecl. Should include all section labels and labelled content.
    refsFromSRS = concatMap findAllRefs allSections
    -- get refs from the stuff already inside the chunk database
    ddefs   = findAllDataDefns chkdb
    gdefs   = findAllGenDefns chkdb
    imods   = findAllInstMods chkdb
    tmods   = findAllTheoryMods chkdb
    concIns = findAllConcInsts chkdb
    lblCon  = findAllLabelledContent chkdb
    newRefs = M.fromList $ map (\x -> (x ^. uid, x)) $ refsFromSRS
      ++ map (ref . makeTabRef' . getTraceConfigUID) (traceMatStandard si)
      ++ secRefs -- secRefs can be removed once #946 is complete
      ++ traceyGraphGetRefs (programName sys) ++ map ref cites
      ++ map ref ddefs ++ map ref gdefs ++ map ref imods
      ++ map ref tmods ++ map ref concIns ++ map ref lblCon
    si2 = set refTable (M.union (si ^. refTable) newRefs) si

-- | Recursively find all references in a section (meant for getting at 'LabelledContent').
findAllRefs :: Section -> [Reference]
findAllRefs (Section _ cs r) = r : concatMap findRefSecCons cs
  where
    findRefSecCons :: SecCons -> [Reference]
    findRefSecCons (Sub s) = findAllRefs s
    findRefSecCons (Con (LlC (LblC _ rf _))) = [rf]
    findRefSecCons _ = []

-- | Fills in the traceabiliy matrix and graphs section of the system information using the document description.
fillTraceMaps :: DocDesc -> System -> System
fillTraceMaps dd si = si''
  where
    tdb = generateTraceMap dd
    si' = set traceTable tdb si
    si'' = set refbyTable (invert tdb) si'

-- | Constructs the unit definitions ('UnitDefn's) found in the document description ('DocDesc') from a database ('ChunkDB').
extractUnits :: DocDesc -> ChunkDB -> [UnitDefn]
extractUnits dd cdb = collectUnitDeps cdb $ ccss' (getDocDesc dd) (egetDocDesc dd) cdb

-- | For a given list of 'Quantity's, collects the 'UnitDefn's dependencies of
-- their units (i.e., what units their units are defined with).
collectUnitDeps :: Quantity c => ChunkDB -> [c] -> [UnitDefn]
collectUnitDeps db = map (`findOrErr` db) . concatMap getUnits . mapMaybe (getUnitLup db)

getUnitLup :: HasUID c => ChunkDB -> c -> Maybe UnitDefn
getUnitLup m c = getUnit (findOrErr (c ^. uid) m :: DefinedQuantityDict)

-- * Section Creator Functions

-- | Helper for creating the different document sections.
mkSections :: System -> DocDesc -> [Section]
mkSections si dd = map doit dd
  where
    doit :: DocSection -> Section
    doit TableOfContents      = mkToC dd
    doit (RefSec rs)          = mkRefSec si dd rs
    doit (IntroSec is)        = mkIntroSec si is
    doit (StkhldrSec sts)     = mkStkhldrSec sts
    doit (SSDSec ss)          = mkSSDSec si ss
    doit (AuxConstntSec acs)  = mkAuxConsSec acs
    doit Bibliography         = mkBib (citeDB si)
    doit (GSDSec gs')         = mkGSDSec gs'
    doit (ReqrmntSec r)       = mkReqrmntSec r
    doit (LCsSec lc)          = mkLCsSec lc
    doit (UCsSec ulcs)        = mkUCsSec ulcs
    doit (TraceabilitySec t)  = mkTraceabilitySec t si
    doit (AppndxSec a)        = mkAppndxSec a
    doit (OffShelfSolnsSec o) = mkOffShelfSolnSec o

-- ** Table of Contents

-- | Helper for making the Table of Contents section.
mkToC :: DocDesc -> Section
mkToC dd = SRS.tOfCont [intro, UlC $ ulcc $ Enumeration $ Bullet $ map ((, Nothing) . toToC) dd] []
  where
    intro = mkParagraph $ S "An outline of all sections included in this SRS is recorded here for easy reference."

-- ** Reference Materials

-- | Helper for creating the reference section and subsections.
-- Includes Table of Symbols, Units and Abbreviations and Acronyms.
mkRefSec :: System -> DocDesc -> RefSec -> Section
mkRefSec si dd (RefProg c l) = SRS.refMat [c] (map (mkSubRef si) l)
  where
    mkSubRef :: System -> RefTab -> Section
    mkSubRef si' TUnits = mkSubRef si' $ TUnits' defaultTUI tOfUnitSIName
    mkSubRef SI {_systemdb = db} (TUnits' con f) =
        SRS.tOfUnit [tuIntro con, LlC $ f (nub $ sortBy compUnitDefn $ extractUnits dd db)] []
    -- FIXME: _quants = v should be removed from this binding and symbols should
    -- be acquired solely through document traversal, however #1658. If we do
    -- just the doc traversal here, then we lose some symbols which only appear
    -- in a table in GlassBR. If we grab symbols from tables (by removing the `isVar`)
    -- in ExtractDocDesc, then the passes which extract `DefinedQuantityDict`s will
    -- error out because some of the symbols in tables are only `QuantityDict`s, and thus
    -- missing a `Concept`.
    mkSubRef SI {_quants = v, _systemdb = cdb} (TSymb con) =
      SRS.tOfSymb
      [tsIntro con,
                LlC $ table Equational (sortBySymbol
                $ filter (`hasStageSymbol` Equational)
                (nub $ map dqdWr v ++ ccss' (getDocDesc dd) (egetDocDesc dd) cdb))
                atStart] []
    mkSubRef SI {_systemdb = cdb} (TSymb' f con) =
      mkTSymb (ccss (getDocDesc dd) (egetDocDesc dd) cdb) f con

    mkSubRef _ (TAandA ideas) =
      SRS.tOfAbbAcc
        [LlC $ tableAbbAccGen $ nub ideas]
        []



-- | Helper for creating the table of symbols.
mkTSymb :: (Quantity e, Concept e, Eq e, MayHaveUnit e) =>
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = SRS.tOfSymb [tsIntro c,
  LlC $ table Equational
    (sortBy (compsy `on` eqSymb) $ filter (`hasStageSymbol` Equational) (nub v))
    (lf f)]
    []
  where lf Term = atStart
        lf Defn = capSent . (^. defn)
        lf (TermExcept cs) = \x -> if (x ^. uid) `elem` map (^. uid) cs then
          capSent (x ^. defn) else atStart x --Compare chunk uids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = \x -> if (x ^. uid) `elem` map (^.uid) cs then
          atStart x else capSent (x ^. defn)
        lf TAD = \tDef -> titleize tDef +: EmptyS +:+. capSent (tDef ^. defn)

-- ** Introduction

-- | Makes the Introduction section into a 'Section'.
mkIntroSec :: System -> IntroSec -> Section
mkIntroSec si (IntroProg probIntro progDefn l) =
  Intro.introductionSection probIntro progDefn $ map (mkSubIntro si) l
  where
    mkSubIntro :: System -> IntroSub -> Section
    mkSubIntro _ (IPurpose intro) = Intro.purposeOfDoc intro
    mkSubIntro _ (IScope main) = Intro.scopeOfRequirements main
    mkSubIntro SI {_sys = sys} (IChar assumed topic asset) =
      Intro.charIntRdrF sys assumed topic asset (SRS.userChar [] [])
    mkSubIntro _ (IOrgSec b s t) = Intro.orgSec b s t
    -- FIXME: s should be "looked up" using "b" once we have all sections being generated

-- ** Stakeholders

-- | Helper for making the Stakeholders section.
mkStkhldrSec :: StkhldrSec -> Section
mkStkhldrSec (StkhldrProg l) = SRS.stakeholder [Stk.stakeholderIntro] $ map mkSubs l
  where
    mkSubs :: StkhldrSub -> Section
    mkSubs (Client kWrd details) = Stk.tClientF kWrd details
    mkSubs (Cstmr kWrd)          = Stk.tCustomerF kWrd

-- ** General System Description

-- | Helper for making the General System Description section.
mkGSDSec :: GSDSec -> Section
mkGSDSec (GSDProg l) = SRS.genSysDes [GSD.genSysIntro] $ map mkSubs l
   where
     mkSubs :: GSDSub -> Section
     mkSubs (SysCntxt cs)            = GSD.sysContxt cs
     mkSubs (UsrChars intro)         = GSD.usrCharsF intro
     mkSubs (SystCons cntnts subsec) = GSD.systCon cntnts subsec

-- ** Specific System Description

-- | Helper for making the Specific System Description section.
mkSSDSec :: System -> SSDSec -> Section
mkSSDSec si (SSDProg l) =
  SSD.specSysDescr $ map (mkSubSSD si) l
  where
    mkSubSSD :: System -> SSDSub -> Section
    mkSubSSD sysi (SSDProblem pd)    = mkSSDProb sysi pd
    mkSubSSD sysi (SSDSolChSpec scs) = mkSolChSpec sysi scs

-- | Helper for making the Specific System Description Problem section.
mkSSDProb :: System -> ProblemDescription -> Section
mkSSDProb _ (PDProg prob subSec subPD) = SSD.probDescF prob (subSec ++ map mkSubPD subPD)
  where mkSubPD (TermsAndDefs sen concepts) = SSD.termDefnF sen concepts
        mkSubPD (PhySysDesc prog parts dif extra) = SSD.physSystDesc prog parts dif extra
        mkSubPD (Goals ins g) = SSD.goalStmtF ins (mkEnumSimpleD g) (length g)


-- | Helper for making the Solution Characteristics Specification section.
mkSolChSpec :: System -> SolChSpec -> Section
mkSolChSpec si (SCSProg l) =
  SRS.solCharSpec [SSD.solutionCharSpecIntro (siSys si) SSD.imStub] $
    map (mkSubSCS si) l
  where
    mkSubSCS :: System -> SCSSub -> Section
    mkSubSCS si' (TMs intro fields ts) =
      SSD.thModF (siSys si') $ map mkParagraph intro ++ map (LlC . tmodel fields si') ts
    mkSubSCS si' (DDs intro fields dds ShowDerivation) = --FIXME: need to keep track of DD intro.
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ concatMap f dds
      where f e = LlC (ddefn fields si' e) : maybeToList (derivation e)
    mkSubSCS si' (DDs intro fields dds _) =
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ map f dds
      where f e = LlC $ ddefn fields si' e
    mkSubSCS si' (GDs intro fields gs' ShowDerivation) =
      SSD.genDefnF $ map mkParagraph intro ++ concatMap (\x -> LlC (gdefn fields si' x) : maybeToList (derivation x)) gs'
    mkSubSCS si' (GDs intro fields gs' _) =
      SSD.genDefnF $ map mkParagraph intro ++ map (LlC . gdefn fields si') gs'
    mkSubSCS si' (IMs intro fields ims ShowDerivation) =
      SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub SSD.gdStub $ map mkParagraph intro ++
      concatMap (\x -> LlC (instanceModel fields si' x) : maybeToList (derivation x)) ims
    mkSubSCS si' (IMs intro fields ims _) =
      SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub SSD.gdStub $ map mkParagraph intro ++
      map (LlC . instanceModel fields si') ims
    mkSubSCS si' (Assumptions ci) =
      SSD.assumpF $ mkEnumSimpleD $ map (`SSD.helperCI` si') ci
    mkSubSCS _ (Constraints end cs)  = SSD.datConF end cs
    mkSubSCS _ (CorrSolnPpties c cs) = SSD.propCorSolF c cs
    siSys :: System -> IdeaDict
    siSys SI {_sys = sys} = nw sys

-- ** Requirements

-- | Helper for making the Requirements section.
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsProg l) = R.reqF $ map mkSubs l
  where
    mkSubs :: ReqsSub -> Section
    mkSubs (FReqsSub  frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)
    mkSubs (NonFReqsSub nfrs) = R.nfReqF (mkEnumSimpleD nfrs)

-- ** Likely Changes

-- | Helper for making the Likely Changes section.
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsProg c) = SRS.likeChg (introChgs Doc.likelyChg c: mkEnumSimpleD c) []

-- ** Unlikely Changes

-- | Helper for making the Unikely Changes section.
mkUCsSec :: UCsSec -> Section
mkUCsSec (UCsProg c) = SRS.unlikeChg (introChgs Doc.unlikelyChg  c : mkEnumSimpleD c) []

-- | Intro paragraph for likely and unlikely changes
introChgs :: NamedIdea n => n -> [ConceptInstance] -> Contents
introChgs xs [] = mkParagraph $ emptySectSentPlu [xs]
introChgs xs _ = foldlSP [S "This", phrase Doc.section_, S "lists the",
  plural xs, S "to be made to the", phrase Doc.software]

-- ** Traceability

-- | Helper for making the Traceability Matrices and Graphs section.
mkTraceabilitySec :: TraceabilitySec -> System -> Section
mkTraceabilitySec (TraceabilityProg progs) si@SI{_sys = sys} = TG.traceMGF trace
  (map (\(TraceConfig _ pre _ _ _) -> foldlList Comma List pre) fProgs)
  (map LlC trace) (programName sys) []
  where
    trace = map (\(TraceConfig u _ desc cols rows) ->
      TM.generateTraceTableView u desc cols rows si) fProgs
    fProgs = filter (\(TraceConfig _ _ _ cols rows) ->
      not $ null (header (TM.layoutUIDs rows si) si)
         || null (header (TM.layoutUIDs cols si) si)) progs

-- | Helper to get headers of rows and columns
header :: ([UID] -> [UID]) -> System -> [Sentence]
header f = TM.traceMHeader (f . Map.keys . (^. refbyTable))

-- ** Off the Shelf Solutions

-- | Helper for making the Off-the-Shelf Solutions section.
mkOffShelfSolnSec :: OffShelfSolnsSec -> Section
mkOffShelfSolnSec (OffShelfSolnsProg cs) = SRS.offShelfSol cs []

-- ** Auxiliary Constants

-- | Helper for making the Values of Auxiliary Constants section.
mkAuxConsSec :: AuxConstntSec -> Section
mkAuxConsSec (AuxConsProg key listOfCons) = AC.valsOfAuxConstantsF key $ sortBySymbol listOfCons

-- ** References

-- | Helper for making the References section.
mkBib :: BibRef -> Section
mkBib bib = SRS.reference [UlC $ ulcc (Bib bib)] []

-- ** Appendix

-- | Helper for making the Appendix section.
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxProg cs) = SRS.appendix cs []

-- ** Find Table of Contents

-- Find more concise way to do this
-- | Finds whether the Table of Contents is in a SRSDecl.
findToC :: SRSDecl -> ShowTableOfContents
findToC [] = NoToC
findToC (DD.TableOfContents:_) = ToC
findToC (_:dds) = findToC dds
