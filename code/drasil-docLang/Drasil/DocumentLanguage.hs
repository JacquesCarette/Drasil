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
  GSDSub(..), IntroSec(..), IntroSub(..), LCsSec(..), LFunc(..), Literature(..),
  PDSub(..), ProblemDescription(..), RefSec(..), RefTab(..), ReqrmntSec(..),
  ReqsSub(..), SCSSub(..), StkhldrSec(..), StkhldrSub(..), SolChSpec(..),
  SSDSec(..), SSDSub(..), TConvention(..), TraceabilitySec(..), TraceConfig(..),
  TSIntro(..), TUIntro(..), UCsSec(..))
import Drasil.DocumentLanguage.Definitions (ddefn, derivation, instanceModel,
  gdefn, tmodel, helperRefs)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc)
import Drasil.TraceTable (generateTraceMap)

import Language.Drasil hiding (Manual, Vector, Verb) -- Manual - Citation name conflict. FIXME: Move to different namespace
                                                     -- Vector - Name conflict (defined in file)
import Utils.Drasil

import Database.Drasil(SystemInformation(SI), citeDB, termTable, ccss, ccss',
  _authors, _kind, _quants, _sys, _usedinfodb, _sysinfodb, collectUnits,
  ChunkDB, traceTable, refbyTable, generateRefbyMap)

import Control.Lens ((^.), over, set)
import qualified Data.Map as Map (elems)

import Drasil.Sections.TableOfAbbAndAcronyms (tableOfAbbAndAcronyms)
import Drasil.Sections.TableOfSymbols (table, symbTableRef)
import Drasil.Sections.TableOfUnits (tOfUnitDesc, tOfUnitSIName, unitTableRef)
import qualified Drasil.DocLang.SRS as SRS (appendix, dataDefn, genDefn,
  genSysDes, inModel, likeChg, unlikeChg, probDesc, reference, solCharSpec,
  stakeholder, thModel, tOfSymb, tOfUnit, userChar, offShelfSol)
import qualified Drasil.Sections.AuxiliaryConstants as AC (valsOfAuxConstantsF)
import qualified Drasil.Sections.GeneralSystDesc as GSD (genSysF, genSysIntro,
  systCon, usrCharsF, sysContxt)
import qualified Drasil.Sections.Introduction as Intro (charIntRdrF,
  introductionSection, orgSec, purposeOfDoc, scopeOfRequirements)
import qualified Drasil.Sections.Requirements as R (reqF, fReqF, nfReqF)
import qualified Drasil.Sections.SpecificSystemDescription as SSD (assumpF,
  datConF, dataDefnF, genDefnF, goalStmtF, inModelF, physSystDesc, probDescF,
  propCorSolF, solutionCharSpecIntro, specSysDescr, termDefnF, thModF)
import qualified Drasil.Sections.Stakeholders as Stk (stakehldrGeneral,
  stakeholderIntro, tClientF, tCustomerF)
import qualified Drasil.DocumentLanguage.TraceabilityMatrix as TM (traceMGF,
  generateTraceTableView)

import Data.Drasil.Concepts.Documentation (likelyChg, refmat, section_,
  software, unlikelyChg)

import Data.Function (on)
import Data.List (nub, sortBy)

-- | Creates a document from a document description and system information
mkDoc :: SRSDecl -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkDoc dd comb si@SI {_sys = sys, _kind = kind, _authors = authors, _sysinfodb = db} =
  Document (nw kind `comb` nw sys) (foldlList Comma List $ map (S . name) authors) $
  mkSections (fillTraceMaps l si) l where
    l = mkDocDesc db dd

extractUnits :: DocDesc -> ChunkDB -> [UnitDefn]
extractUnits dd cdb = collectUnits cdb $ ccss' (getDocDesc dd) (egetDocDesc dd) cdb

fillTraceMaps :: DocDesc -> SystemInformation -> SystemInformation
fillTraceMaps dd si@SI{_sysinfodb = db} = si {_sysinfodb =
  set refbyTable (generateRefbyMap tdb) $ set traceTable tdb db} where
  tdb = generateTraceMap dd

-- | Helper for creating the document sections
mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si dd = map doit dd
  where
    doit :: DocSection -> Section
    doit (RefSec rs)         = mkRefSec si dd rs
    doit (IntroSec is)       = mkIntroSec si is
    doit (StkhldrSec sts)    = mkStkhldrSec sts
    doit (SSDSec ss)         = mkSSDSec si ss
    doit (AuxConstntSec acs) = mkAuxConsSec acs 
    doit Bibliography        = mkBib (citeDB si)
    doit (GSDSec gs')        = mkGSDSec gs'
    doit (ReqrmntSec r)      = mkReqrmntSec r
    doit (LCsSec lc)         = mkLCsSec lc
    doit (UCsSec ulcs)       = mkUCsSec ulcs
    doit (TraceabilitySec t) = mkTraceabilitySec t si
    doit (AppndxSec a)       = mkAppndxSec a
    doit (OffShelfSolnsSec o) = mkOffShelfSolnSec o


-- | Helper for creating the reference section and subsections
mkRefSec :: SystemInformation -> DocDesc -> RefSec -> Section
mkRefSec si dd (RefProg c l) = section (titleize refmat) [c]
  (map (mkSubRef si) l) (makeSecRef "RefMat" "Reference Material") --DO NOT CHANGE LABEL OR THINGS WILL BREAK -- see Language.Drasil.Document.Extract
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
      tableOfAbbAndAcronyms $ nub $ map fst $ Map.elems $ termTable db

-- | table of units constructors
tunit, tunit' :: [TUIntro] -> RefTab
tunit  t = TUnits' t tOfUnitSIName
tunit' t = TUnits' t tOfUnitDesc

-- | Helper for creating the table of symbols
mkTSymb :: (Quantity e, Concept e, Eq e, MayHaveUnit e) =>
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = SRS.tOfSymb [tsIntro c,
  LlC $ table Equational
    (sortBy (compsy `on` eqSymb) $ filter (`hasStageSymbol` Equational) (nub v))
    (lf f)] 
    []
  where lf Term = atStart
        lf Defn = (^. defn)
        lf (TermExcept cs) = \x -> if (x ^. uid) `elem` map (^. uid) cs then
          x ^. defn else atStart x --Compare chunk uids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = \x -> if (x ^. uid) `elem` map (^.uid) cs then
          atStart x else x ^. defn
        lf TAD = \tDef -> titleize tDef :+: S ":" +:+ (tDef ^. defn)

-- | table of symbols constructor
tsymb, tsymb' :: [TSIntro] -> RefTab
tsymb = TSymb 
-- ^ Default Term and given intro

tsymb' = TSymb' Defn
-- ^ Default Defn and given intro

-- | Custom table of symbols constructor
tsymb'' :: [TSIntro] -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro 
-- ^ Custom function and intro.

-- | table of symbols intro builder. Used by mkRefSec
tsIntro :: [TSIntro] -> Contents
tsIntro x = mkParagraph $ foldr ((+:+) . tsI) EmptyS x

-- | table of symbols intro writer. Translates a TSIntro to a list of Sentences
tsI :: TSIntro -> Sentence
tsI (TypogConvention ts) = typogConvention ts
tsI SymbOrder = S "The symbols are listed in alphabetical order."
tsI (SymbConvention ls) = symbConvention ls
tsI TSPurpose = S "The symbols used in this document are summarized in" +:+
  Ref symbTableRef +:+. S "along with their units"
tsI VectorUnits = S "For vector quantities, the units shown are for each component of the vector."

-- | typographic convention writer. Translates a list of typographic conventions
-- to a sentence
typogConvention :: [TConvention] -> Sentence
typogConvention [] = error "No arguments given for typographic conventions"
typogConvention ts = S "Throughout the document," +:+. foldlList Comma List (map tcon ts)
  where tcon (Vector emph) = S ("symbols in " ++ show emph ++
                                " will represent vectors, and scalars otherwise")
        tcon (Verb s) = s

-- | symbolic convention writer.
symbConvention :: [Literature] -> Sentence
symbConvention [] = error "Attempting to reference no literature for SymbConvention"
symbConvention scs = S "The choice of symbols was made to be consistent with the" +:+.
                      makeSentence (map scon scs)
  where makeSentence [x,y] = x +:+ S "and with" +:+ y
        makeSentence xs    = foldlList Comma List xs
        scon (Lit x)       = phrase x +:+ S "literature"
        scon (Doc x)       = S "existing documentation for" +:+ phrase x
        scon (Doc' x)      = S "existing documentation for" +:+ plural x
        scon (Manual x)    = S "that used in the" +:+ phrase x +:+ S "manual"

-- | Table of units intro builder. Used by mkRefSec
tuIntro :: [TUIntro] -> Contents
tuIntro x = mkParagraph $ foldr ((+:+) . tuI) EmptyS x

-- | mkEnumSimple is a convenience function for converting lists into
-- Simple-type Enumerations.
mkEnumSimple :: (a -> ListTuple) -> [a] -> [Contents]
mkEnumSimple f = replicate 1 . UlC . ulcc . Enumeration . Simple . map f

-- | mkEnumSimpleD is a convenience function for transforming types which are
-- instances of the constraints Referable, HasShortName, and Definition, into
-- Simple-type Enumerations.
mkEnumSimpleD :: (Referable c, HasShortName c, Definition c) => [c] -> [Contents]
mkEnumSimpleD = mkEnumSimple $ mkListTuple (\x -> Flat $ x ^. defn)

-- | Creates a list tuple filling in the title with a ShortName and filling
-- reference information.
mkListTuple :: (Referable c, HasShortName c) => (c -> ItemType) -> c -> ListTuple
mkListTuple f x = (S . getStringSN $ shortname x, f x, Just $ refAdd x)

-- | table of units intro writer. Translates a TUIntro to a Sentence.
tuI :: TUIntro -> Sentence
tuI System  = 
  S "The unit system used throughout is SI (Système International d'Unités)."
tuI TUPurpose = 
  S "For each unit" `sC` Ref unitTableRef +:+. S "lists the symbol, a description and the SI name"
tuI Derived = 
  S "In addition to the basic units, several derived units are also used."

-- | Default table of units intro contains the
defaultTUI :: [TUIntro]
defaultTUI = [System, Derived, TUPurpose]

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

-- | Helper for making the 'Stakeholders' section
mkStkhldrSec :: StkhldrSec -> Section
mkStkhldrSec (StkhldrProg key details) = Stk.stakehldrGeneral key details
mkStkhldrSec (StkhldrProg2 l) = SRS.stakeholder [Stk.stakeholderIntro] $ map mkSubs l
  where
    mkSubs :: StkhldrSub -> Section
    mkSubs (Client kWrd details) = Stk.tClientF kWrd details
    mkSubs (Cstmr kWrd)          = Stk.tCustomerF kWrd

-- | Helper for making the 'General System Description' section
mkGSDSec :: GSDSec -> Section
mkGSDSec (GSDProg cntxt uI cnstrnts systSubSec) = GSD.genSysF cntxt uI cnstrnts systSubSec
mkGSDSec (GSDProg2 l) = SRS.genSysDes [GSD.genSysIntro] $ map mkSubs l
   where
     mkSubs :: GSDSub -> Section
     mkSubs (SysCntxt cs)            = GSD.sysContxt cs
     mkSubs (UsrChars intro)         = GSD.usrCharsF intro
     mkSubs (SystCons cntnts subsec) = GSD.systCon cntnts subsec

-- | Helper for making the 'Specific System Description' section
mkSSDSec :: SystemInformation -> SSDSec -> Section
mkSSDSec si (SSDProg l) =
  SSD.specSysDescr $ map (mkSubSSD si) l
  where
    mkSubSSD :: SystemInformation -> SSDSub -> Section
    mkSubSSD sysi (SSDProblem pd)    = mkSSDProb sysi pd
    mkSubSSD sysi (SSDSolChSpec scs) = mkSolChSpec sysi scs

mkSSDProb :: SystemInformation -> ProblemDescription -> Section
mkSSDProb _ (PDProg prob subSec subPD) = SSD.probDescF prob (subSec ++ map mkSubPD subPD)
  where mkSubPD (TermsAndDefs sen concepts) = SSD.termDefnF sen concepts
        mkSubPD (PhySysDesc prog parts dif extra) = SSD.physSystDesc prog parts dif extra
        mkSubPD (Goals ins g) = SSD.goalStmtF ins (mkEnumSimpleD g)

mkSolChSpec :: SystemInformation -> SolChSpec -> Section
mkSolChSpec si (SCSProg l) =
  SRS.solCharSpec [SSD.solutionCharSpecIntro (siSys si) imStub] $
    map (mkSubSCS si) l
  where
    mkSubSCS :: SystemInformation -> SCSSub -> Section
    mkSubSCS _ (TMs _ _ [])   = error "There are no Theoretical Models"
    mkSubSCS _ (GDs _ _ [] _) = SSD.genDefnF []
    mkSubSCS _ (DDs _ _ [] _) = error "There are no Data Definitions"
    mkSubSCS _ (IMs _ _ [] _)  = error "There are no Instance Models"
    mkSubSCS si' (TMs intro fields ts) =
      SSD.thModF (siSys si') $ map mkParagraph intro ++ map (LlC . tmodel fields si') ts
    mkSubSCS si' (DDs intro fields dds ShowDerivation) = --FIXME: need to keep track of DD intro.
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ concatMap (\x -> [LlC $ ddefn fields si' x, derivation x]) dds
    mkSubSCS si' (DDs intro fields dds _) =
      SSD.dataDefnF EmptyS $ map mkParagraph intro ++ map (LlC . ddefn fields si') dds
    mkSubSCS si' (GDs intro fields gs' ShowDerivation) =
      SSD.genDefnF $ map mkParagraph intro ++ concatMap (\x -> [LlC $ gdefn fields si' x, derivation x]) gs'
    mkSubSCS si' (GDs intro fields gs' _) =
      SSD.genDefnF $ map mkParagraph intro ++ map (LlC . gdefn fields si') gs'
    mkSubSCS si' (IMs intro fields ims ShowDerivation) =
      SSD.inModelF pdStub ddStub tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      concatMap (\x -> [LlC $ instanceModel fields si' x, derivation x]) ims
    mkSubSCS si' (IMs intro fields ims _) =
      SSD.inModelF pdStub ddStub tmStub (SRS.genDefn [] []) $ map mkParagraph intro ++
      map (LlC . instanceModel fields si') ims
    mkSubSCS si' (Assumptions ci) =
      SSD.assumpF $ mkEnumSimpleD $ map (`helperCI` si') ci
    mkSubSCS _ (Constraints end cs)  = SSD.datConF end cs
    mkSubSCS _ (CorrSolnPpties c cs) = SSD.propCorSolF c cs

helperCI :: ConceptInstance -> SystemInformation -> ConceptInstance
helperCI a c = over defn (\x -> foldlSent_ [x, refby $ helperRefs a c]) a
  where
    refby EmptyS = EmptyS
    refby sent   = sParen $ S "RefBy:" +:+. sent

{--}

-- | Section stubs for implicit referencing
tmStub, ddStub, imStub, pdStub :: Section
tmStub = SRS.thModel   [] []
ddStub = SRS.dataDefn  [] []
imStub = SRS.inModel   [] []
pdStub = SRS.probDesc  [] []

-- | Helper for making the 'Requirements' section
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsProg l) = R.reqF $ map mkSubs l
  where
    mkSubs :: ReqsSub -> Section
    mkSubs (FReqsSub frs tbs) = R.fReqF  (mkEnumSimpleD frs ++ map LlC tbs)
    mkSubs (NonFReqsSub nfrs) = R.nfReqF (mkEnumSimpleD nfrs)

{--}

-- | Helper for making the 'LikelyChanges' section
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsProg c) = SRS.likeChg (intro : mkEnumSimpleD c) []
  where intro = foldlSP [S "This", phrase section_, S "lists the",
                plural likelyChg, S "to be made to the", phrase software]

{--}

-- | Helper for making the 'UnikelyChanges' section
mkUCsSec :: UCsSec -> Section
mkUCsSec (UCsProg c) = SRS.unlikeChg (intro : mkEnumSimpleD c) []
  where intro = foldlSP [S "This", phrase section_, S "lists the",
                plural unlikelyChg, S "to be made to the", phrase software]

{--}

-- | Helper for making the 'Traceability Matrices and Graphs' section
mkTraceabilitySec :: TraceabilitySec -> SystemInformation -> Section
mkTraceabilitySec (TraceabilityProg progs) si = TM.traceMGF trace
  (map (\(TraceConfig _ pre _ _ _) -> foldlList Comma List pre) progs)
  (map LlC trace) [] where
  trace = map (\(TraceConfig u _ desc rows cols) -> TM.generateTraceTableView
    u desc rows cols si) progs

{--}

-- | Helper for making the 'Off-the-Shelf Solutions' section
mkOffShelfSolnSec :: OffShelfSolnsSec -> Section
mkOffShelfSolnSec (OffShelfSolnsProg cs) = SRS.offShelfSol cs [] 

{--}

-- | Helper for making the 'Values of Auxiliary Constants' section
mkAuxConsSec :: AuxConstntSec -> Section
mkAuxConsSec (AuxConsProg key listOfCons) = AC.valsOfAuxConstantsF key $ sortBySymbol listOfCons

{--}

-- | Helper for making the bibliography section
mkBib :: BibRef -> Section
mkBib bib = SRS.reference [UlC $ ulcc (Bib bib)] []

{--}

-- | Helper for making the 'Appendix' section
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxProg cs) = SRS.appendix cs []

{--}

-- Helper
siSys :: SystemInformation -> IdeaDict
siSys SI {_sys = sys} = nw sys
