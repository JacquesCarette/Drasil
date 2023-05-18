{-# Language TupleSections #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage where

import Drasil.DocDecl (SRSDecl, mkDocDesc)
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

import Database.Drasil hiding (cdb)
import SysInfo.Drasil

import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccGen)
import Drasil.Sections.TableOfContents (toToC, findToC)
import Drasil.Sections.TableOfSymbols (table, tsIntro)
import Drasil.Sections.TableOfUnits (tOfUnitSIName, tuIntro, defaultTUI)
import qualified Drasil.DocLang.SRS as SRS (appendix, genDefn,
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
  tmStub, ddStub, imStub, pdStub)
import qualified Drasil.Sections.Stakeholders as Stk (stakeholderIntro,
  tClientF, tCustomerF)
import qualified Drasil.DocumentLanguage.TraceabilityMatrix as TM (
  generateTraceTableView)
import qualified Drasil.DocumentLanguage.TraceabilityGraph as TG (traceMGF)
import Drasil.DocumentLanguage.TraceabilityGraph (traceyGraphGetRefs)
import Drasil.Sections.TraceabilityMandGs (traceMatStandard)

import qualified Data.Drasil.Concepts.Documentation as Doc (likelyChg, section_,
  software, unlikelyChg)

import Control.Lens ((^.), set)
import Data.Function (on)
import Data.List (nub, sortBy, sortOn)
import qualified Data.Map as Map (elems, toList, assocs)
import Data.Char (isSpace)

-- * Main Function
-- | Creates a document from a document description, a title combinator function, and system information.
mkDoc :: SRSDecl -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkDoc dd comb si@SI {_sys = sys, _kind = kind, _authors = authors} =
  Document (nw kind `comb` nw sys) (foldlList Comma List $ map (S . name) authors) (findToC l) $
  mkSections fullSI l where
    fullSI = fillcdbSRS dd si
    l = mkDocDesc fullSI dd

-- * Functions to Fill 'CunkDB'

-- TODO: Move all of these "filler" functions to a new file?
-- TODO: Add in 'fillTermMap' once #2775 is complete.
-- | Assuming a given 'ChunkDB' with no traces and minimal/no references, fill in for rest of system information.
-- Currently fills in references, traceability matrix information and 'IdeaDict's.
fillcdbSRS :: SRSDecl -> SystemInformation -> SystemInformation
fillcdbSRS srsDec si = fillSecAndLC srsDec $ fillReferences srsDec $ fillTraceSI srsDec si

{-Don't want to manually add concepts here, Drasil should do it automatically. Perhaps through traversal?
fillConcepts :: SystemInformation -> SystemInformation
fillConcepts si = si2
  where
    si2 = set sysinfodb chkdb2 si
    chkdb = si ^. sysinfodb
    tmtbl = termTable chkdb
    chkdb2 = chkdb{termTable = termMap $ nub (map nw doccon ++ map nw doccon' ++ map nw softwarecon ++ map nw physicCon ++ map nw physicCon' ++ map nw physicalcon ++ map nw educon ++ map nw mathcon ++ map nw mathcon' ++ map nw compcon ++ map nw compcon' ++ map nw solidcon ++ map nw thermocon ++ (map (fst.snd) $ Map.assocs tmtbl))}
-}

{- FIXME: See #2775
-- Fill in term map from all concepts and quantities.
fillTermMap :: SystemInformation -> SystemInformation
fillTermMap si = si2
  where
    -- Get current contents of si
    chkdb = si ^. sysinfodb
    -- extract everything that could possibly lead to an 'IdeaDict'
    symbs    = map (fst.snd) $ Map.assocs $ symbolTable chkdb
    trms     = map (fst.snd) $ Map.assocs $ termTable chkdb
    concepts = map (fst.snd) $ Map.assocs $ defTable chkdb

    -- TODO: Uncomment these when the second part of #2775 is resolved.
    -- Some Definitions and models overwrite the term for a given UID.
    -- We don't really want this behaviour, so it should be resolved by
    -- changing some of the constructors for ModelKind found in drasil-theory

    ddefs   = map (fst.snd) $ Map.assocs $ chkdb ^. dataDefnTable
    gdefs   = map (fst.snd) $ Map.assocs $ chkdb ^. gendefTable
    imods   = map (fst.snd) $ Map.assocs $ chkdb ^. insmodelTable
    tmods   = map (fst.snd) $ Map.assocs $ chkdb ^. theoryModelTable
    concIns = map (fst.snd) $ Map.assocs $ chkdb ^. conceptinsTable
    -- fill in the appropriate chunkdb field
    chkdb2 = chkdb {termTable = termMap $ nub $ map nw symbs ++ map nw trms
      ++ map nw concepts ++ map nw concIns
      ++ map nw ddefs ++ map nw gdefs ++ map nw imods ++ map nw tmods}
    -- return the filled in system information
    si2 = set sysinfodb chkdb2 si
    -}

-- | Fill in the 'Section's and 'LabelledContent' maps of the 'ChunkDB' from the 'SRSDecl'.
fillSecAndLC :: SRSDecl -> SystemInformation -> SystemInformation
fillSecAndLC dd si = si2
  where
    -- Get current contents of si
    chkdb = si ^. sysinfodb
    -- extract sections and labelledcontent
    allSections = concatMap findAllSec $ mkSections si $ mkDocDesc si dd
    allLC = concatMap findAllLC allSections
    existingSections = map (fst.snd) $ Map.assocs $ chkdb ^. sectionTable
    existingLC = map (fst.snd) $ Map.assocs $ chkdb ^. labelledcontentTable
    -- fill in the appropriate chunkdb fields
    chkdb2 = set labelledcontentTable (idMap $ nub $ existingLC ++ allLC)
      $ set sectionTable (idMap $ nub $ existingSections ++ allSections) chkdb
    -- return the filled in system information
    si2 = set sysinfodb chkdb2 si
    -- Helper and finder functions
    findAllSec :: Section -> [Section]
    findAllSec s@(Section _ cs _) = s : concatMap findAllSubSec cs
    findAllSubSec :: SecCons -> [Section]
    findAllSubSec (Sub s) = findAllSec s
    findAllSubSec _ = []
    findAllLC :: Section -> [LabelledContent]
    findAllLC (Section _ cs _) = concatMap findLCSecCons cs
    findLCSecCons :: SecCons -> [LabelledContent]
    findLCSecCons (Sub s) = findAllLC s
    findLCSecCons (Con (LlC lblcons)) = [lblcons]
    findLCSecCons _ = []

-- | Takes in existing information from the Chunk database to construct a database of references.
fillReferences :: SRSDecl -> SystemInformation -> SystemInformation
fillReferences dd si@SI{_sys = sys} = si2
  where
    -- get old chunk database + ref database
    chkdb = si ^. sysinfodb
    rfdb = refdb si
    -- convert SRSDecl into a list of sections (to easily get at all the references used in the SRS)
    allSections = mkSections si $ mkDocDesc si dd
    -- get refs from SRSDecl. Should include all section labels and labelled content.
    refsFromSRS = concatMap findAllRefs allSections
    -- get refs from the stuff already inside the chunk database
    inRefs = concatMap dRefToRef ddefs ++ concatMap dRefToRef gdefs ++ concatMap dRefToRef imods ++ concatMap dRefToRef tmods
    ddefs  = map (fst.snd) $ Map.assocs $ chkdb ^. dataDefnTable
    gdefs   = map (fst.snd) $ Map.assocs $ chkdb ^. gendefTable
    imods   = map (fst.snd) $ Map.assocs $ chkdb ^. insmodelTable
    tmods   = map (fst.snd) $ Map.assocs $ chkdb ^. theoryModelTable
    concIns = map (fst.snd) $ Map.assocs $ chkdb ^. conceptinsTable
    secs    = map (fst.snd) $ Map.assocs $ chkdb ^. sectionTable
    lblCon  = map (fst.snd) $ Map.assocs $ chkdb ^. labelledcontentTable
    cites   = citeDB si -- map (fst.snd) $ Map.assocs $ rfdb  ^. citationDB
    conins  = map (fst.snd) $ Map.assocs $ rfdb  ^. conceptDB
    -- search the old reference table just in case the user wants to manually add in some references
    refs    = map (fst.snd) $ Map.assocs $ chkdb ^. refTable
    -- set new reference table in the chunk database
    chkdb2 = set refTable (idMap $ nub $ refsFromSRS ++ inRefs
      ++ map (ref.makeTabRef'.getTraceConfigUID) (traceMatStandard si) ++ secRefs -- secRefs can be removed once #946 is complete
      ++ traceyGraphGetRefs (filter (not.isSpace) $ abrv sys) ++ map ref cites
      ++ map ref conins ++ map ref ddefs ++ map ref gdefs ++ map ref imods
      ++ map ref tmods ++ map ref concIns ++ map ref secs ++ map ref lblCon
      ++ refs) chkdb
    -- set new chunk database into system information
    si2 = set sysinfodb chkdb2 si

-- | Helper that gets references from definitions and models.
dRefToRef :: HasDecRef a => a -> [Reference]
dRefToRef r = map ref $ r ^. getDecRefs

-- | Recursively find all references in a section (meant for getting at 'LabelledContent').
findAllRefs :: Section -> [Reference]
findAllRefs (Section _ cs r) = r: concatMap findRefSecCons cs
  where
    findRefSecCons :: SecCons -> [Reference]
    findRefSecCons (Sub s) = findAllRefs s
    findRefSecCons (Con (LlC (LblC rf _))) = [rf]
    findRefSecCons _ = []

-- | Helper for filling in the traceability matrix and graph information into the system.
fillTraceSI :: SRSDecl -> SystemInformation -> SystemInformation
fillTraceSI dd si = fillTraceMaps l $ fillReqs l si
  where
    l = mkDocDesc si dd

-- | Fills in the traceabiliy matrix and graphs section of the system information using the document description.
fillTraceMaps :: DocDesc -> SystemInformation -> SystemInformation
fillTraceMaps dd si@SI{_sysinfodb = db} = si {_sysinfodb =
  set refbyTable (generateRefbyMap tdb) $ set traceTable tdb db} where
  tdb = generateTraceMap dd

-- | Fills in the requirements section of the system information using the document description.
fillReqs :: DocDesc -> SystemInformation -> SystemInformation
fillReqs [] si = si
fillReqs (ReqrmntSec (ReqsProg x):_) si@SI{_sysinfodb = db} = genReqs x
  where
    genReqs [] = si
    genReqs (FReqsSub c _:_) = si {_sysinfodb = set conceptinsTable newCI db} where
        newCI = idMap $ nub $ c ++ map fst (sortOn snd $ map snd $ Map.toList $ db ^. conceptinsTable)
    genReqs (_:xs) = genReqs xs
fillReqs (_:xs) si = fillReqs xs si

-- | Constructs the unit definitions ('UnitDefn's) found in the document description ('DocDesc') from a database ('ChunkDB').
extractUnits :: DocDesc -> ChunkDB -> [UnitDefn]
extractUnits dd cdb = collectUnits cdb $ ccss' (getDocDesc dd) (egetDocDesc dd) cdb

-- * Section Creator Functions

-- | Helper for creating the different document sections.
mkSections :: SystemInformation -> DocDesc -> [Section]
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
mkRefSec :: SystemInformation -> DocDesc -> RefSec -> Section
mkRefSec si dd (RefProg c l) = SRS.refMat [c] (map (mkSubRef si) l)
  where
    mkSubRef :: SystemInformation -> RefTab -> Section
    mkSubRef si' TUnits = mkSubRef si' $ TUnits' defaultTUI tOfUnitSIName
    mkSubRef SI {_sysinfodb = db} (TUnits' con f) =
        SRS.tOfUnit [tuIntro con, LlC $ f (nub $ sortBy compUnitDefn $ extractUnits dd db)] []
    -- FIXME: _quants = v should be removed from this binding and symbols should
    -- be acquired solely through document traversal, however #1658. If we do
    -- just the doc traversal here, then we lose some symbols which only appear
    -- in a table in GlassBR. If we grab symbols from tables (by removing the `isVar`)
    -- in ExtractDocDesc, then the passes which extract `DefinedQuantityDict`s will
    -- error out because some of the symbols in tables are only `QuantityDict`s, and thus
    -- missing a `Concept`.
    mkSubRef SI {_quants = v, _sysinfodb = cdb} (TSymb con) =
      SRS.tOfSymb 
      [tsIntro con,
                LlC $ table Equational (sortBySymbol
                $ filter (`hasStageSymbol` Equational) 
                (nub $ map qw v ++ ccss' (getDocDesc dd) (egetDocDesc dd) cdb))
                atStart] []
    mkSubRef SI {_sysinfodb = cdb} (TSymb' f con) =
      mkTSymb (ccss (getDocDesc dd) (egetDocDesc dd) cdb) f con
    mkSubRef SI {_usedinfodb = db} TAandA =
      SRS.tOfAbbAcc [LlC $ tableAbbAccGen $ nub $ map fst $ Map.elems $ termTable db] []

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
mkIntroSec :: SystemInformation -> IntroSec -> Section
mkIntroSec si (IntroProg probIntro progDefn l) =
  Intro.introductionSection probIntro progDefn $ map (mkSubIntro si) l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> Section
    mkSubIntro _ (IPurpose intro) = Intro.purposeOfDoc intro
    mkSubIntro _ (IScope main) = Intro.scopeOfRequirements main
    mkSubIntro SI {_sys = sys} (IChar assumed topic asset) =
      Intro.charIntRdrF sys assumed topic asset (SRS.userChar [] [])
    mkSubIntro _ (IOrgSec i b s t) = Intro.orgSec i b s t
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
mkSSDSec :: SystemInformation -> SSDSec -> Section
mkSSDSec si (SSDProg l) =
  SSD.specSysDescr $ map (mkSubSSD si) l
  where
    mkSubSSD :: SystemInformation -> SSDSub -> Section
    mkSubSSD sysi (SSDProblem pd)    = mkSSDProb sysi pd
    mkSubSSD sysi (SSDSolChSpec scs) = mkSolChSpec sysi scs

-- | Helper for making the Specific System Description Problem section.
mkSSDProb :: SystemInformation -> ProblemDescription -> Section
mkSSDProb _ (PDProg prob subSec subPD) = SSD.probDescF prob (subSec ++ map mkSubPD subPD)
  where mkSubPD (TermsAndDefs sen concepts) = SSD.termDefnF sen concepts
        mkSubPD (PhySysDesc prog parts dif extra) = SSD.physSystDesc prog parts dif extra
        mkSubPD (Goals ins g) = SSD.goalStmtF ins (mkEnumSimpleD g) (length g)
                

-- | Helper for making the Solution Characteristics Specification section.
mkSolChSpec :: SystemInformation -> SolChSpec -> Section
mkSolChSpec si (SCSProg l) =
  SRS.solCharSpec [SSD.solutionCharSpecIntro (siSys si) SSD.imStub] $
    map (mkSubSCS si) l
  where
    mkSubSCS :: SystemInformation -> SCSSub -> Section
    mkSubSCS _ (TMs _ _ [])      = error "There are no Theoretical Models"
    mkSubSCS _ (GDs _ _ [] _)    = SSD.genDefnF []
    mkSubSCS _ (DDs _ _ [] _) = error "There are no Data Definitions"
    mkSubSCS _ (IMs _ _ [] _)    = error "There are no Instance Models"
    mkSubSCS si' (TMs intro fields ts) =
      SSD.thModF (siSys si') $ map mkParagraph intro ++ map (LlC . tmodel fields si') ts
    mkSubSCS si' (DDs intro fields dds ShowDerivation) = --FIXME: need to keep track of DD intro.
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ concatMap f dds
      where f e = [LlC $ ddefn fields si' e, derivation e]
    mkSubSCS si' (DDs intro fields dds _) =
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ map f dds
      where f e = LlC $ ddefn fields si' e
    mkSubSCS si' (GDs intro fields gs' ShowDerivation) =
      SSD.genDefnF $ map mkParagraph intro ++ concatMap (\x -> [LlC $ gdefn fields si' x, derivation x]) gs'
    mkSubSCS si' (GDs intro fields gs' _) =
      SSD.genDefnF $ map mkParagraph intro ++ map (LlC . gdefn fields si') gs'
    mkSubSCS si' (IMs intro fields ims ShowDerivation) =
      SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      concatMap (\x -> [LlC $ instanceModel fields si' x, derivation x]) ims
    mkSubSCS si' (IMs intro fields ims _) =
      SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      map (LlC . instanceModel fields si') ims
    mkSubSCS si' (Assumptions ci) =
      SSD.assumpF $ mkEnumSimpleD $ map (`SSD.helperCI` si') ci
    mkSubSCS _ (Constraints end cs)  = SSD.datConF end cs
    mkSubSCS _ (CorrSolnPpties c cs) = SSD.propCorSolF c cs
    siSys :: SystemInformation -> IdeaDict
    siSys SI {_sys = sys} = nw sys

-- ** Requirements

-- | Helper for making the Requirements section.
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsProg l) = R.reqF $ map mkSubs l
  where
    mkSubs :: ReqsSub -> Section
    mkSubs (FReqsSub  frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)
    mkSubs (FReqsSub' frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)
    mkSubs (NonFReqsSub nfrs) = R.nfReqF (mkEnumSimpleD nfrs)

-- ** Likely Changes

-- | Helper for making the Likely Changes section.
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsProg c) = SRS.likeChg (intro : mkEnumSimpleD c) []
  where intro = foldlSP [S "This", phrase Doc.section_, S "lists the",
                plural Doc.likelyChg, S "to be made to the", phrase Doc.software]

-- ** Unlikely Changes

-- | Helper for making the Unikely Changes section.
mkUCsSec :: UCsSec -> Section
mkUCsSec (UCsProg c) = SRS.unlikeChg (intro : mkEnumSimpleD c) []
  where intro = foldlSP [S "This", phrase Doc.section_, S "lists the",
                plural Doc.unlikelyChg, S "to be made to the", phrase Doc.software]

-- ** Traceability

-- | Helper for making the Traceability Matrices and Graphs section.
mkTraceabilitySec :: TraceabilitySec -> SystemInformation -> Section
mkTraceabilitySec (TraceabilityProg progs) si@SI{_sys = sys} = TG.traceMGF trace
  (map (\(TraceConfig _ pre _ _ _) -> foldlList Comma List pre) progs)
  (map LlC trace) (filter (not.isSpace) $ abrv sys) []
  where
  trace = map (\(TraceConfig u _ desc rows cols) -> TM.generateTraceTableView
    u desc rows cols si) progs

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
