{-# Language TupleSections #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage where

import Drasil.DocDecl (SRSDecl, mkDocDesc)
import Drasil.DocumentLanguage.Core (AppndxSec(..), AuxConstntSec(..), TableOfContents(..),
  DerivationDisplay(..), DocDesc, DocSection(..), OffShelfSolnsSec(..), 
  IntroSec(..), IPurposeSub(..), IScopeSub(..), ICharSub(..), IOrgSub(..),
  StkhldrSec(..), ClientSub(..), CstmrSub(..), 
  GSDSec(..), SysCntxt(..), UsrChars(..), SystCons(..),
  SSDSec(..), ProblemDescription(..), TermsAndDefs(..), PhySysDesc(..), Goals(..),
  SolChSpec(..), Assumptions(..), TMs(..), GDs(..), DDs(..), IMs(..), Constraints(..), CorrSolnPpties(..), 
  ReqrmntSec(..), FReqsSub'(..), FReqsSub(..), NonFReqsSub(..),
  RefSec(..), TUnits(..), TUnits'(..), TSymb(..), TSymb'(..), TAandA(..),
  LCsSec(..), LFunc(..), TraceabilitySec(..), TraceConfig(..),
  TSIntro(..), UCsSec(..), getTraceConfigUID)
import Drasil.DocumentLanguage.Definitions (ddefn, derivation, instanceModel,
  gdefn, tmodel)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)
import Drasil.TraceTable (generateTraceMap)

import Language.Drasil
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
    findAllSec s@(Section _ _ cs _) = s : concatMap findAllSubSec cs
    findAllSubSec :: SecCons -> [Section]
    findAllSubSec (Sub s) = findAllSec s
    findAllSubSec _ = []
    findAllLC :: Section -> [LabelledContent]
    findAllLC (Section _ _ cs _) = concatMap findLCSecCons cs
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
findAllRefs (Section _ _ cs r) = r: concatMap findRefSecCons cs
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
fillReqs (FReqsSub (FReqsProg c _):_) si@SI{_sysinfodb = db} = si {_sysinfodb = set conceptinsTable newCI db} where
  newCI = idMap $ nub $ c ++ map fst (sortOn snd $ map snd $ Map.toList $ db ^. conceptinsTable)
fillReqs (_:xs) si = fillReqs xs si

-- | Constructs the unit definitions ('UnitDefn's) found in the document description ('DocDesc') from a database ('ChunkDB').
extractUnits :: DocDesc -> ChunkDB -> [UnitDefn]
extractUnits dd cdb = collectUnits cdb $ ccss' (getDocDesc dd) (egetDocDesc dd) cdb

-- * Section Creator Functions
-- CHECK: whether si is needed in each mksection

-- | Helper for creating the different document sections.
mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si dd = map doit dd
  where
    doit :: DocSection -> Section
    doit (TableOfContents st)    = mkToC st
    doit (RefSec rs)             = mkRefSec rs
    doit (TUnits tu)             = mkTUnits si dd tu
    doit (TUnits' tu')           = mkTUnits' si dd tu'
    doit (TSymb ts)              = mkTSymb si dd ts
    doit (TSymb' ts')            = mkTSymb' si dd ts'
    doit (TAandA ta)             = mkTAandA si ta
    doit (IntroSec is)           = mkIntroSec is
    doit (IPurposeSub ip)        = mkIPurpSub ip
    doit (IScopeSub is)          = mkIScopeSub is
    doit (ICharSub ic)           = mkICharSub si ic
    doit (IOrgSub io)            = mkIOrgSub io
    doit (StkhldrSec sts)        = mkStkhldrSec sts
    doit (ClientSub c)           = mkClientSub c
    doit (CstmrSub c)            = mkCstmrSub c
    doit (GSDSec gs')            = mkGSDSec gs'
    doit (SysCntxt sc)           = mkSysCntxt sc
    doit (UsrChars ucc)          = mkUsrChars ucc
    doit (SystCons sc)           = mkSystCons sc
    doit (SSDSec ss)             = mkSSDSec ss
    doit (ProblemDescription pd) = mkSSDProb si pd
    doit (TermsAndDefs td)       = mkTermsAndDefs td
    doit (PhySysDesc psd)        = mkPhySysDesc psd
    doit (Goals g)               = mkGoals g
    doit (SolChSpec scs)         = mkSolChSpec si scs
    doit (Assumptions a)         = mkAssump si a
    doit (TMs tm)                = mkTM si tm
    doit (GDs gd)                = mkGD si gd
    doit (DDs dd')               = mkDD si dd'
    doit (IMs im)                = mkIM si im
    doit (Constraints c)         = mkConstraint si c
    doit (CorrSolnPpties csp)    = mkCorrSolnPpties si csp
    doit (ReqrmntSec r)          = mkReqrmntSec r
    doit (FReqsSub fr)           = mkFReqsSub fr
    doit (FReqsSub' fr')         = mkFReqsSub' fr'
    doit (NonFReqsSub nfr)       = mkNonFReqsSub nfr
    doit (AuxConstntSec acs)     = mkAuxConsSec acs 
    doit Bibliography            = mkBib (citeDB si)
    doit (LCsSec lc)             = mkLCsSec lc
    doit (UCsSec ulcs)           = mkUCsSec ulcs
    doit (TraceabilitySec t)     = mkTraceabilitySec t si
    doit (AppndxSec a)           = mkAppndxSec a
    doit (OffShelfSolnsSec o)    = mkOffShelfSolnSec o

-- ** Table of Contents

-- | Helper for making the Table of Contents section.
mkToC :: TableOfContents -> Section
mkToC (ToCProg dd) = SRS.tOfCont 0 [intro, UlC $ ulcc $ Enumeration $ Bullet $ map ((, Nothing) . toToC) dd]
  where
    intro = mkParagraph $ S "An outline of all sections included in this SRS is recorded here for easy reference."

-- ** Reference Materials

-- | Helper for creating the reference section and subsections.
-- Includes Table of Symbols, Units and Abbreviations and Acronyms.
mkRefSec :: RefSec -> Section
mkRefSec (RefProg c) = SRS.refMat 0 [c]

mkTUnits :: SystemInformation -> DocDesc -> TUnits -> Section
mkTUnits si' dd TUProg = mkTUnits' si' dd $ TUProg' defaultTUI tOfUnitSIName

mkTUnits' :: SystemInformation -> DocDesc -> TUnits' -> Section
mkTUnits' SI {_sysinfodb = db} dd (TUProg' con f) =
  SRS.tOfUnit 1 [tuIntro con, LlC $ f (nub $ sortBy compUnitDefn $ extractUnits dd db)]
  -- FIXME: _quants = v should be removed from this binding and symbols should
  -- be acquired solely through document traversal, however #1658. If we do
  -- just the doc traversal here, then we lose some symbols which only appear
  -- in a table in GlassBR. If we grab symbols from tables (by removing the `isVar`)
  -- in ExtractDocDesc, then the passes which extract `DefinedQuantityDict`s will
  -- error out because some of the symbols in tables are only `QuantityDict`s, and thus
  -- missing a `Concept`.
    
mkTSymb :: SystemInformation -> DocDesc -> TSymb -> Section    
mkTSymb SI {_quants = v, _sysinfodb = cdb} dd (TSProg con) =
  SRS.tOfSymb 1
    [tsIntro con,
              LlC $ table Equational (sortBySymbol
              $ filter (`hasStageSymbol` Equational) 
              (nub $ map qw v ++ ccss' (getDocDesc dd) (egetDocDesc dd) cdb))
              atStart]

mkTSymb' :: SystemInformation -> DocDesc -> TSymb' -> Section
mkTSymb' SI {_sysinfodb = cdb} dd (TSProg' f con) =
  mkTSymbol (ccss (getDocDesc dd) (egetDocDesc dd) cdb) f con

mkTAandA :: SystemInformation -> TAandA -> Section 
mkTAandA SI {_usedinfodb = db} TAAProg =
  SRS.tOfAbbAcc 1 [LlC $ tableAbbAccGen $ nub $ map fst $ Map.elems $ termTable db]

-- | Helper for creating the table of symbols.
mkTSymbol :: (Quantity e, Concept e, Eq e, MayHaveUnit e) =>
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymbol v f c = SRS.tOfSymb 1 [tsIntro c,
  LlC $ table Equational
    (sortBy (compsy `on` eqSymb) $ filter (`hasStageSymbol` Equational) (nub v))
    (lf f)] 
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
mkIntroSec :: IntroSec -> Section
mkIntroSec (IntroProg probIntro progDefn) = Intro.introductionSection probIntro progDefn

mkIPurpSub :: IPurposeSub -> Section  
mkIPurpSub (IPurposeProg intro) = Intro.purposeOfDoc intro

mkIScopeSub :: IScopeSub -> Section  
mkIScopeSub (IScopeProg main) = Intro.scopeOfRequirements main

mkICharSub :: SystemInformation -> ICharSub -> Section  
mkICharSub SI {_sys = sys} (ICharProg assumed topic asset) = Intro.charIntRdrF sys assumed topic asset (SRS.userChar 1 [])

mkIOrgSub :: IOrgSub -> Section  
mkIOrgSub (IOrgProg i b s t) = Intro.orgSec i b s t
  -- FIXME: s should be "looked up" using "b" once we have all sections being generated

-- ** Stakeholders

-- | Helper for making the Stakeholders section.
mkStkhldrSec :: StkhldrSec -> Section
mkStkhldrSec (StkhldrProg _) = SRS.stakeholder 0 [Stk.stakeholderIntro]

mkClientSub :: ClientSub -> Section
mkClientSub (ClientProg kWrd details) = Stk.tClientF kWrd details

mkCstmrSub :: CstmrSub -> Section
mkCstmrSub (CstmrProg kWrd) = Stk.tCustomerF kWrd

-- ** General System Description

-- | Helper for making the General System Description section.
mkGSDSec :: GSDSec -> Section
mkGSDSec (GSDProg _) = SRS.genSysDes 0 [GSD.genSysIntro] 

mkSysCntxt :: SysCntxt -> Section
mkSysCntxt (SysCntxtProg cs) = GSD.sysContxt cs

mkUsrChars :: UsrChars -> Section
mkUsrChars (UsrCharsProg intro) = GSD.usrCharsF intro

mkSystCons :: SystCons -> Section
mkSystCons (SystConsProg cntnts) = GSD.systCon cntnts

-- ** Specific System Description

-- | Helper for making the Specific System Description section.
mkSSDSec :: SSDSec -> Section
mkSSDSec (SSDProg _) = SSD.specSysDescr 

-- CHECK later 
--  where
--    mkSubSSD :: SystemInformation -> SSDSub -> Section
--    mkSubSSD sysi (SSDProblem pd)    = mkSSDProb sysi pd
--    mkSubSSD sysi (SSDSolChSpec scs) = mkSolChSpec sysi scs

-- | Helper for making the Specific System Description Problem section.
mkSSDProb :: SystemInformation -> ProblemDescription -> Section
mkSSDProb _ (PDProg prob) = SSD.probDescF prob 

mkTermsAndDefs :: TermsAndDefs -> Section
mkTermsAndDefs (TDProg sen concepts) = SSD.termDefnF sen concepts

mkPhySysDesc :: PhySysDesc -> Section
mkPhySysDesc (PSDProg prog parts dif extra) = SSD.physSystDesc prog parts dif extra

mkGoals :: Goals -> Section
mkGoals (GProg ins g) = SSD.goalStmtF ins (mkEnumSimpleD g)

-- | Helper for making the Solution Characteristics Specification section.
mkSolChSpec :: SystemInformation -> SolChSpec -> Section
mkSolChSpec si (SCSProg _) = SRS.solCharSpec 1 [SSD.solutionCharSpecIntro (siSys si) SSD.imStub] 

mkTM :: SystemInformation -> TMs -> Section
mkTM _ (TMProg _ _ [])      = error "There are no Theoretical Models"
mkTM si' (TMProg intro fields ts) =
      SSD.thModF (siSys si') $ map mkParagraph intro ++ map (LlC . tmodel fields si') ts

mkGD :: SystemInformation -> GDs -> Section
mkGD _ (GDProg _ _ [] _)    = SSD.genDefnF []
mkGD si' (GDProg intro fields gs' ShowDerivation) =
  SSD.genDefnF $ map mkParagraph intro ++ concatMap (\x -> [LlC $ gdefn fields si' x, derivation x]) gs'
mkGD si' (GDProg intro fields gs' _) =
  SSD.genDefnF $ map mkParagraph intro ++ map (LlC . gdefn fields si') gs'

mkDD :: SystemInformation -> DDs -> Section
mkDD _ (DDProg _ _ [] _) = error "There are no Data Definitions"
mkDD si' (DDProg intro fields dds ShowDerivation) = --FIXME: need to keep track of DD intro.
  SSD.dataDefnF EmptyS $ map mkParagraph intro ++ concatMap f dds
  where f e = [LlC $ ddefn fields si' e, derivation e]
mkDD si' (DDProg intro fields dds _) =
  SSD.dataDefnF EmptyS $ map mkParagraph intro ++ map f dds
  where f e = LlC $ ddefn fields si' e

mkIM :: SystemInformation -> IMs -> Section
mkIM _ (IMProg _ _ [] _)    = error "There are no Instance Models"
mkIM si' (IMProg intro fields ims ShowDerivation) =
  SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub (SRS.genDefn 2 []) $ map mkParagraph intro ++
  concatMap (\x -> [LlC $ instanceModel fields si' x, derivation x]) ims
mkIM si' (IMProg intro fields ims _) =
  SSD.inModelF SSD.pdStub SSD.ddStub SSD.tmStub (SRS.genDefn 2 []) $ map mkParagraph intro ++
  map (LlC . instanceModel fields si') ims

mkAssump :: SystemInformation -> Assumptions -> Section
mkAssump si' (AssumpProg ci) =
  SSD.assumpF $ mkEnumSimpleD $ map (`SSD.helperCI` si') ci
    
mkConstraint :: SystemInformation -> Constraints -> Section
mkConstraint _ (ConstProg end cs)  = SSD.datConF end cs

mkCorrSolnPpties :: SystemInformation -> CorrSolnPpties -> Section
mkCorrSolnPpties _ (CorrSolProg c cs) = SSD.propCorSolF c cs

siSys :: SystemInformation -> IdeaDict
siSys SI {_sys = sys} = nw sys

-- ** Requirements

-- | Helper for making the Requirements section.
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsProg _) = R.reqF

mkFReqsSub :: FReqsSub -> Section
mkFReqsSub (FReqsProg frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)

mkFReqsSub' :: FReqsSub' -> Section
mkFReqsSub' (FReqsProg' frs tbs) = R.fReqF (mkEnumSimpleD frs ++ map LlC tbs)

mkNonFReqsSub :: NonFReqsSub -> Section
mkNonFReqsSub (NonFReqsProg nfrs) = R.nfReqF (mkEnumSimpleD nfrs)


-- ** Likely Changes

-- | Helper for making the Likely Changes section.
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsProg c) = SRS.likeChg 0 (intro : mkEnumSimpleD c)
  where intro = foldlSP [S "This", phrase Doc.section_, S "lists the",
                plural Doc.likelyChg, S "to be made to the", phrase Doc.software]

-- ** Unlikely Changes

-- | Helper for making the Unikely Changes section.
mkUCsSec :: UCsSec -> Section
mkUCsSec (UCsProg c) = SRS.unlikeChg 0 (intro : mkEnumSimpleD c)
  where intro = foldlSP [S "This", phrase Doc.section_, S "lists the",
                plural Doc.unlikelyChg, S "to be made to the", phrase Doc.software]

-- ** Traceability

-- | Helper for making the Traceability Matrices and Graphs section.
mkTraceabilitySec :: TraceabilitySec -> SystemInformation -> Section
mkTraceabilitySec (TraceabilityProg progs) si@SI{_sys = sys} = TG.traceMGF trace
  (map (\(TraceConfig _ pre _ _ _) -> foldlList Comma List pre) progs)
  (map LlC trace) (filter (not.isSpace) $ abrv sys)
  where
  trace = map (\(TraceConfig u _ desc rows cols) -> TM.generateTraceTableView
    u desc rows cols si) progs

-- ** Off the Shelf Solutions

-- | Helper for making the Off-the-Shelf Solutions section.
mkOffShelfSolnSec :: OffShelfSolnsSec -> Section
mkOffShelfSolnSec (OffShelfSolnsProg cs) = SRS.offShelfSol 0 cs 

-- ** Auxiliary Constants

-- | Helper for making the Values of Auxiliary Constants section.
mkAuxConsSec :: AuxConstntSec -> Section
mkAuxConsSec (AuxConsProg key listOfCons) = AC.valsOfAuxConstantsF key $ sortBySymbol listOfCons

-- ** References

-- | Helper for making the References section.
mkBib :: BibRef -> Section
mkBib bib = SRS.reference 0 [UlC $ ulcc (Bib bib)] 

-- ** Appendix

-- | Helper for making the Appendix section.
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxProg cs) = SRS.appendix 0 cs 