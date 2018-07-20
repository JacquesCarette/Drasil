{-# LANGUAGE GADTs #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage where

import Drasil.DocumentLanguage.Definitions (Fields, ddefn, ddefn', derivation, 
  instanceModel, gdefn, tmodel)

import Language.Drasil hiding (Manual, Vector, Verb) -- Manual - Citation name conflict. FIXME: Move to different namespace
                                               -- Vector - Name conflict (defined in file)

import Control.Lens ((^.))
import qualified Data.Map as Map (elems)

import Drasil.Sections.TableOfAbbAndAcronyms (table_of_abb_and_acronyms)
import Drasil.Sections.TableOfSymbols (table)
import Drasil.Sections.TableOfUnits (table_of_units)
import qualified Drasil.DocLang.SRS as SRS (appendix, dataDefn, genDefn, genSysDes, 
  inModel, likeChg, unlikeChg, probDesc, reference, solCharSpec, stakeholder,
  thModel, tOfSymb, userChar,dataDefn, offShelfSol, propCorSol)
import qualified Drasil.Sections.AuxiliaryConstants as AC (valsOfAuxConstantsF)
import qualified Drasil.Sections.GeneralSystDesc as GSD (genSysF, genSysIntro,
  systCon, usrCharsF, sysContxt)
import qualified Drasil.Sections.Introduction as Intro (charIntRdrF, 
  introductionSection, orgSec, purposeOfDoc, scopeOfRequirements)
import qualified Drasil.Sections.Requirements as R (fReqF, nonFuncReqF, reqF)
import qualified Drasil.Sections.ScopeOfTheProject as SotP (scopeOfTheProjF)
import qualified Drasil.Sections.SpecificSystemDescription as SSD (assumpF,
  datConF, dataDefnF, genDefnF, inModelF, probDescF, solutionCharSpecIntro, 
  specSysDescr, thModF)
import qualified Drasil.Sections.Stakeholders as Stk (stakehldrGeneral,
  stakeholderIntro, tClientF, tCustomerF)
import qualified Drasil.Sections.TraceabilityMandGs as TMG (traceMGF)

import Data.Drasil.Concepts.Documentation (refmat)

import Data.Function (on)
import Data.List (nub, sortBy)

type System = Sentence
type DocKind = Sentence

{--}

type DocDesc = [DocSection]

-- | Document sections are either Verbatim, Reference, Introduction, or Specific
-- System Description sections (for now!)
data DocSection = Verbatim Section
                | RefSec RefSec
                | IntroSec IntroSec
                | StkhldrSec StkhldrSec
                | GSDSec GSDSec
                | ScpOfProjSec ScpOfProjSec
                | SSDSec SSDSec
                | ReqrmntSec ReqrmntSec
                | LCsSec LCsSec
                | UCsSec UCsSec
                | TraceabilitySec TraceabilitySec
                | AuxConstntSec AuxConstntSec
                | Bibliography
                | AppndxSec AppndxSec
                | ExistingSolnSec ExistingSolnSec

--FIXME: anything with 'Verb' in it should eventually go

{--}

-- | Reference section. Contents are top level followed by a list of subsections.
-- RefVerb is used for including verbatim subsections
data RefSec = RefProg Contents [RefTab] | RefVerb Section -- continue

-- | Reference subsections
data RefTab where
  TUnits :: RefTab
  TUnits' :: [TUIntro] -> RefTab -- Customized intro
  TSymb :: [TSIntro] -> RefTab
  TSymb' :: LFunc -> [TSIntro] -> RefTab
  TAandA :: RefTab
  TVerb :: Section -> RefTab
  -- add more here

-- | For creating the table of symbols intro
data TSIntro = TypogConvention [TConvention] -- ^ Typographic conventions used
             | SymbOrder -- ^ Symbol ordering (defaults to alphabetical)
             | SymbConvention [Literature] -- ^ Symbol conventions match specified literature
             | TSPurpose -- ^ Purpose of the Table of Symbols

-- | Possible typographic conventions
data TConvention = Vector Emphasis -- ^ How vectors are emphasized
                 | Verb Sentence -- ^ Verbatim for specialized conventions

-- | How to handle Emphasis
data Emphasis = Bold
              | Italics

instance Show Emphasis where
  show Bold = "bold"
  show Italics = "italics"

-- | Types of literature
data Literature = Lit Topic -- ^ literature
                | Doc Topic -- ^ existing documentation for (singular topic)
                | Doc' Topic -- ^ existing documentation for (plural of topic)
                | Manual Topic -- ^ manual

type Topic = IdeaDict

-- | For creating the table of units intro
data TUIntro = System -- ^ System of units (defaults to SI)
             | Derived -- ^ Sentence about derived units being used alongside SI
             | TUPurpose -- ^ Purpose of the table of units

-- | Lens (lookup) functions (currently for TSymb)
data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: Concept c => [c] -> LFunc
  DefnExcept :: Concept c => [c] -> LFunc
  TAD :: LFunc --Term and Definition

{--}

-- | Introduction section. Contents are top level followed by a list of
-- subsections. IntroVerb is used for including verbatim subsections.
data IntroSec = IntroProg Sentence Sentence [IntroSub]
  -- ^ Temporary, will be modified once we've figured out more about the section.
              | IntroVerb Section

-- | Introduction subsections
data IntroSub where
  IVerb    :: Section -> IntroSub
  IPurpose :: Sentence -> IntroSub
  IScope   :: Sentence -> Sentence -> IntroSub
  IChar    :: Sentence -> Sentence -> Sentence -> IntroSub
  IOrgSec  :: Sentence -> CI -> Section -> Sentence -> IntroSub

{--}

-- | Stakeholders section
data StkhldrSec = StkhldrProg CI Sentence | StkhldrProg2 [StkhldrSub] | StkhldrVerb Section

-- | Stakeholders subsections
data StkhldrSub where
  StkhldrSubVerb :: Section -> StkhldrSub
  Client :: (Idea a) => a -> Sentence -> StkhldrSub
  Cstmr  :: (Idea a) => a -> StkhldrSub

{--}

data GSDSec = GSDVerb Section
            | GSDProg [Section] Contents [Contents] [Section]
            | GSDProg2 [GSDSub]

data GSDSub where
  GSDSubVerb :: Section -> GSDSub
  SysCntxt   :: [Contents] -> GSDSub
  UsrChars   :: [Contents] -> GSDSub
  SystCons   :: [Contents] -> [Section] -> GSDSub

{--}

data ScpOfProjSec = ScpOfProjVerb Section | ScpOfProjProg Sentence Contents Contents

{--}

-- | Specific System Description section . Contains a list of subsections.
-- Verbatim sections handled by SSDVerb
data SSDSec = SSDProg [SSDSub] | SSDVerb Section

-- | Specific system description subsections
data SSDSub where
  SSDSubVerb :: Section -> SSDSub
  SSDProblem :: ProblemDescription -> SSDSub
  SSDSolChSpec :: SolChSpec -> SSDSub

-- | Problem Description section
data ProblemDescription where
  PDVerb :: Section -> ProblemDescription
  PDProg :: (Idea a) => Sentence -> a -> Sentence -> [Section] -> ProblemDescription

-- | Solution Characteristics Specification section
data SolChSpec where
  SCSVerb :: Section -> SolChSpec
  SCSProg :: [SCSSub] -> SolChSpec

-- | Solution Characteristics Specification subsections
data SCSSub where
  SCSSubVerb     :: Section -> SCSSub
  Assumptions    :: SCSSub
  TMs            :: Fields  -> [TheoryModel] -> SCSSub
  GDs            :: Fields  -> [GenDefn] -> DerivationDisplay -> SCSSub
  DDs            :: Fields  -> [QDefinition] -> DerivationDisplay -> SCSSub --FIXME: Need DD intro
  DDs'           :: Fields  -> [DataDefinition] -> DerivationDisplay -> SCSSub --FIXME: Need DD intro -- should eventually replace and be renamed to DDs
  IMs            :: Fields  -> [InstanceModel] -> DerivationDisplay -> SCSSub
  Constraints    :: Sentence -> Sentence -> Sentence -> [Contents] {-Fields  -> [UncertainWrapper] -> [ConstrainedChunk]-} -> SCSSub --FIXME: temporary definition?
--FIXME: Work in Progress ^
  CorrSolnPpties :: [Contents] -> SCSSub
data DerivationDisplay = ShowDerivation
                       | HideDerivation
{--}

data ReqrmntSec = ReqsVerb Section | ReqsProg [ReqsSub]

data ReqsSub where
  ReqsSubVerb :: Section -> ReqsSub
  FReqsSub :: [Contents] -> ReqsSub
  NonFReqsSub :: (Concept c) => [c] -> [c] -> Sentence -> Sentence -> ReqsSub

{--}

data LCsSec = LCsVerb Section | LCsProg [Contents]

{--}

data UCsSec = UCsVerb Section | UCsProg [Contents]

{--}

data TraceabilitySec = TraceabilityVerb Section | TraceabilityProg [Contents] [Sentence] [Contents] [Section]

{--}

-- | Off-The-Shelf Solutions section 
data ExistingSolnSec = ExistSolnVerb Section | ExistSolnProg [Contents]

{--}

-- | Values of Auxiliary Constants section
data AuxConstntSec = AuxConsProg CI [QDefinition] | AuxConsVerb Section

{--}

data AppndxSec = AppndxVerb Section | AppndxProg [Contents]

{--}

-- | Creates a document from a document description and system information
mkDoc :: DocDesc -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkDoc l comb si@SI {_sys = sys, _kind = kind, _authors = authors} = Document
  (nw kind `comb` nw sys) (S $ manyNames authors) (mkSections si l)

-- | Helper for creating the document sections
mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si l = map doit l
  where
    doit :: DocSection -> Section
    doit (Verbatim s)        = s
    doit (RefSec rs)         = mkRefSec si rs
    doit (IntroSec is)       = mkIntroSec si is
    doit (StkhldrSec sts)    = mkStkhldrSec sts
    doit (SSDSec ss)         = mkSSDSec si ss
    doit (AuxConstntSec acs) = mkAuxConsSec acs
    doit Bibliography        = mkBib (citeDB si)
    doit (GSDSec gs')        = mkGSDSec gs'
    doit (ScpOfProjSec sop)  = mkScpOfProjSec sop
    doit (ReqrmntSec r)      = mkReqrmntSec r
    doit (LCsSec lc')        = mkLCsSec lc'
    doit (UCsSec ulcs)       = mkUCsSec ulcs
    doit (TraceabilitySec t) = mkTraceabilitySec t
    doit (AppndxSec a)       = mkAppndxSec a
    doit (ExistingSolnSec o) = mkExistingSolnSec o


-- | Helper for creating the reference section and subsections
mkRefSec :: SystemInformation -> RefSec -> Section
mkRefSec _  (RefVerb s) = s
mkRefSec si (RefProg c l) = section'' (titleize refmat) [c]
  (map (mkSubRef si) l) "RefMat"
  where
    mkSubRef :: SystemInformation -> RefTab -> Section
    mkSubRef SI {_sysinfodb = db}  TUnits =
        table_of_units (sortBy comp_unitdefn $ Map.elems $ db ^. unitTable) (tuIntro defaultTUI)
    mkSubRef SI {_sysinfodb = db} (TUnits' con) =
        table_of_units (sortBy comp_unitdefn $ Map.elems $ db ^. unitTable) (tuIntro con)
    mkSubRef SI {_quants = v} (TSymb con) =
      SRS.tOfSymb
      [tsIntro con, table Equational (
         sortBy (compsy `on` eqSymb) $
         filter (`hasStageSymbol` Equational)
         (nub v)) at_start] []
    mkSubRef SI {_concepts = cccs} (TSymb' f con) = mkTSymb cccs f con
    mkSubRef SI {_sysinfodb = db} TAandA =
      table_of_abb_and_acronyms $ nub $ Map.elems (db ^. termTable)
    mkSubRef _              (TVerb s) = s

-- | Helper for creating the table of symbols
mkTSymb :: (Quantity e, Concept e, Eq e) =>
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = SRS.tOfSymb [tsIntro c,
  table Equational
    (sortBy (compsy `on` eqSymb) $ filter (`hasStageSymbol` Equational) (nub v))
    (lf f)] []
  where lf Term = at_start
        lf Defn = (^. defn)
        lf (TermExcept cs) = \x -> if (x ^. uid) `elem` map (^. uid) cs then
          x ^. defn else at_start x --Compare chunk uids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = \x -> if (x ^. uid) `elem` map (^.uid) cs then
          at_start x else x ^. defn
        lf TAD = \tDef -> titleize tDef :+: S ":" +:+ (tDef ^. defn)

-- | table of symbols constructor
tsymb, tsymb' :: [TSIntro] -> RefTab
tsymb intro = TSymb intro 
-- ^ Default Term and given intro

tsymb' intro = TSymb' Defn intro
-- ^ Default Defn and given intro

-- | Custom table of symbols constructor
tsymb'' :: [TSIntro] -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro 
-- ^ Custom function and intro.

-- | table of symbols intro builder. Used by mkRefSec
tsIntro :: [TSIntro] -> Contents
tsIntro x = Paragraph $ foldr ((+:+) . tsI) EmptyS x

-- | table of symbols intro writer. Translates a TSIntro to a list of Sentences
tsI :: TSIntro -> Sentence
tsI (TypogConvention ts) = typogConvention ts
tsI SymbOrder = S "The symbols are listed in alphabetical order."
tsI (SymbConvention ls) = symbConvention ls
tsI TSPurpose = S "The table that follows summarizes the symbols used in" +:+
  S "this document along with their units."

-- | typographic convention writer. Translates a list of typographic conventions
-- to a sentence
typogConvention :: [TConvention] -> Sentence
typogConvention [] = error "No arguments given for typographic conventions"
typogConvention ts = S "Throughout the document" `sC` makeSentence ts
  where makeSentence [x]     = tcon x :+: S "."
        makeSentence [x,y]   = tcon x +:+ S "and" +:+. tcon y
        makeSentence [x,y,z] = tcon x `sC` tcon y `sC` S "and" +:+. tcon z
        makeSentence (x:xs)  = tcon x `sC` makeSentence xs
        makeSentence  _      = error "How did you get here?"
        tcon (Vector emph)   = S ("symbols in " ++ show emph ++
                               " will represent vectors, and scalars otherwise")
        tcon (Verb s) = s

-- | symbolic convention writer.
symbConvention :: [Literature] -> Sentence
symbConvention [] = error "Attempting to reference no literature for SymbConvention"
symbConvention scs = S "The choice of symbols was made to be consistent with the" +:+
                      makeSentence scs
  where makeSentence [x]     = scon x :+: S "."
        makeSentence [x,y]   = scon x +:+ S "and with" +:+. scon y
        makeSentence [x,y,z] = scon x `sC` scon y `sC` S "and" +:+. scon z
        makeSentence (x:xs)  = scon x `sC` makeSentence xs
        makeSentence  _      = error "How did you get here?"
        scon (Lit x)         = phrase x +:+ S "literature"
        scon (Doc x)         = S "existing documentation for" +:+ phrase x
        scon (Doc' x)        = S "existing documentation for" +:+ plural x
        scon (Manual x)      = S "that used in the" +:+ phrase x +:+ S "manual"

-- | Table of units intro builder. Used by mkRefSec
tuIntro :: [TUIntro] -> Contents
tuIntro x = Paragraph $ foldr ((+:+) . tuI) EmptyS x

-- | table of units intro writer. Translates a TUIntro to a Sentence.
tuI :: TUIntro -> Sentence
tuI System  = 
  S "The unit system used throughout is SI (Système International d'Unités)."
tuI TUPurpose = 
  S "For each unit, the table lists the symbol, a description and the SI name."
tuI Derived = 
  S "In addition to the basic units, several derived units are also used."

-- | Default table of units intro contains the
defaultTUI :: [TUIntro]
defaultTUI = [System, Derived, TUPurpose]

mkIntroSec :: SystemInformation -> IntroSec -> Section
mkIntroSec _ (IntroVerb s) = s
mkIntroSec si (IntroProg probIntro progDefn l) =
  Intro.introductionSection probIntro progDefn $ map (mkSubIntro si) l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> Section
    mkSubIntro _ (IVerb s) = s
    mkSubIntro si' (IPurpose intro) = Intro.purposeOfDoc (getRefDB si') intro
    mkSubIntro SI {_sys = sys} (IScope main intendedPurp) =
      Intro.scopeOfRequirements main sys intendedPurp
    mkSubIntro SI {_sys = sys} (IChar know understand appStandd) =
      Intro.charIntRdrF know understand sys appStandd (SRS.userChar [] [])
    mkSubIntro _ (IOrgSec i b s t)  = Intro.orgSec i b s t
    -- FIXME: s should be "looked up" using "b" once we have all sections being generated

-- | Helper for making the 'Stakeholders' section
mkStkhldrSec :: StkhldrSec -> Section
mkStkhldrSec (StkhldrVerb s) = s
mkStkhldrSec (StkhldrProg key details) = Stk.stakehldrGeneral key details
mkStkhldrSec (StkhldrProg2 l) = SRS.stakeholder [Stk.stakeholderIntro] $ map mkSubs l
  where
    mkSubs :: StkhldrSub -> Section
    mkSubs (StkhldrSubVerb s)    = s
    mkSubs (Client kWrd details) = Stk.tClientF kWrd details
    mkSubs (Cstmr kWrd)          = Stk.tCustomerF kWrd

-- | Helper for making the 'General System Description' section
mkGSDSec :: GSDSec -> Section
mkGSDSec (GSDVerb s) = s
mkGSDSec (GSDProg cntxt uI cnstrnts systSubSec) = GSD.genSysF cntxt uI cnstrnts systSubSec
mkGSDSec (GSDProg2 l) = SRS.genSysDes [GSD.genSysIntro] $ map mkSubs l
   where
     mkSubs :: GSDSub -> Section
     mkSubs (GSDSubVerb s)           = s
     mkSubs (SysCntxt cs)            = GSD.sysContxt cs
     mkSubs (UsrChars intro)         = GSD.usrCharsF intro
     mkSubs (SystCons cntnts subsec) = GSD.systCon cntnts subsec

-- | Helper for making the 'Scope of the Project' section
mkScpOfProjSec :: ScpOfProjSec -> Section
mkScpOfProjSec (ScpOfProjVerb s) = s
mkScpOfProjSec (ScpOfProjProg kWrd uCTCntnts indCases) =
  SotP.scopeOfTheProjF kWrd uCTCntnts indCases

-- | Helper for making the 'Specific System Description' section
mkSSDSec :: SystemInformation -> SSDSec -> Section
mkSSDSec _ (SSDVerb s) = s
mkSSDSec si (SSDProg l) =
  SSD.specSysDescr (siSys si) $ map (mkSubSSD si) l
  where
    mkSubSSD :: SystemInformation -> SSDSub -> Section
    mkSubSSD _ (SSDSubVerb s)        = s
    mkSubSSD sysi (SSDProblem pd)    = mkSSDProb sysi pd
    mkSubSSD sysi (SSDSolChSpec scs) = mkSolChSpec sysi scs

mkSSDProb :: SystemInformation -> ProblemDescription -> Section
mkSSDProb _ (PDVerb s) = s
mkSSDProb _ (PDProg start progName end subSec) =
  SSD.probDescF start progName end subSec

mkSolChSpec :: SystemInformation -> SolChSpec -> Section
mkSolChSpec _ (SCSVerb s) = s
mkSolChSpec si (SCSProg l) =
  SRS.solCharSpec [SSD.solutionCharSpecIntro (siSys si) inModSec] $
    map (mkSubSCS si) l
  where
    mkSubSCS :: SystemInformation -> SCSSub -> Section
    mkSubSCS _ (SCSSubVerb s)  = s
    mkSubSCS _ (TMs _ [])   = error "There are no Theoretical Models"
    mkSubSCS _ (GDs _ [] _) = SSD.genDefnF []
    mkSubSCS _ (DDs _ [] _) = error "There are no Data Definitions" 
    mkSubSCS _ (DDs' _ [] _) = error "There are no Data Definitions" --FIXME: temporary duplicate 
    mkSubSCS _ (IMs _ [] _)  = error "There are no Instance Models"
    mkSubSCS si' (TMs fields ts) =
      SSD.thModF (siSys si') (map (tmodel fields (_sysinfodb si')) ts)
    mkSubSCS si' (DDs fields dds ShowDerivation) = --FIXME: need to keep track of DD intro.
      SSD.dataDefnF EmptyS (concatMap (\x -> ddefn fields (_sysinfodb si') x : derivation x) dds)
    mkSubSCS si' (DDs fields dds _) =
      SSD.dataDefnF EmptyS (map (ddefn fields (_sysinfodb si')) dds)
    mkSubSCS si' (DDs' fields dds ShowDerivation) = --FIXME: need to keep track of DD intro. --FIXME: temporary duplicate
      SSD.dataDefnF EmptyS (concatMap (\x -> ddefn' fields (_sysinfodb si') x : derivation x) dds)
    mkSubSCS si' (DDs' fields dds _) = --FIXME: temporary duplicate
      SSD.dataDefnF EmptyS (map (ddefn' fields (_sysinfodb si')) dds)
    mkSubSCS si' (GDs fields gs' ShowDerivation) =
      SSD.genDefnF (concatMap (\x -> gdefn fields (_sysinfodb si') x : derivation x) gs')
    mkSubSCS si' (GDs fields gs' _) =
      SSD.genDefnF (map (gdefn fields (_sysinfodb si')) gs')
    mkSubSCS si' (IMs fields ims ShowDerivation) = 
      SSD.inModelF pdStub ddStub tmStub gdStub (concatMap (\x -> instanceModel fields (_sysinfodb si') x : derivation x) ims)
    mkSubSCS si' (IMs fields ims _)= 
      SSD.inModelF pdStub ddStub tmStub gdStub (map (instanceModel fields (_sysinfodb si')) ims)
    mkSubSCS SI {_refdb = db} Assumptions =
      SSD.assumpF tmStub gdStub ddStub imStub lcStub ucStub
      (map Assumption $ assumptionsFromDB (db ^. assumpRefTable))
    mkSubSCS _ (CorrSolnPpties cs)   = SRS.propCorSol cs []
    mkSubSCS _ (Constraints a b c d) = SSD.datConF a b c d 
    inModSec = SRS.inModel [Paragraph EmptyS] []
    --FIXME: inModSec should be replaced with a walk
    -- over the SCSProg and generate a relevant intro.
    -- Could start with just a quick check of whether or not IM is included and
    -- then error out if necessary.

{--}

-- | Section stubs for implicit referencing
tmStub, gdStub, ddStub, imStub, lcStub, ucStub, pdStub:: Section
tmStub = SRS.thModel   [] []
gdStub = SRS.genDefn   [] []
ddStub = SRS.dataDefn  [] []
imStub = SRS.inModel   [] []
lcStub = SRS.likeChg   [] []
ucStub = SRS.unlikeChg [] []
pdStub = SRS.probDesc  [] []

-- | Helper for making the 'Requirements' section
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsVerb s) = s
mkReqrmntSec (ReqsProg l) = R.reqF $ map mkSubs l
  where
    mkSubs :: ReqsSub -> Section
    mkSubs (ReqsSubVerb s) = s
    mkSubs (FReqsSub reqs) = R.fReqF reqs
    mkSubs (NonFReqsSub noPrrty prrty rsn explain) = R.nonFuncReqF noPrrty prrty rsn explain

{--}

-- | Helper for making the 'LikelyChanges' section
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsVerb s) = s
mkLCsSec (LCsProg c) = SRS.likeChg c []

{--}

-- | Helper for making the 'UnikelyChanges' section
mkUCsSec :: UCsSec -> Section
mkUCsSec (UCsVerb s) = s
mkUCsSec (UCsProg c) = SRS.unlikeChg c []

{--}

-- | Helper for making the 'Traceability Matrices and Graphs' section
mkTraceabilitySec :: TraceabilitySec -> Section
mkTraceabilitySec (TraceabilityVerb s) = s
mkTraceabilitySec (TraceabilityProg refs trailing otherContents subSec) =
  TMG.traceMGF refs trailing otherContents subSec

{--}

-- | Helper for making the 'Off-the-Shelf Solutions' section
mkExistingSolnSec :: ExistingSolnSec -> Section
mkExistingSolnSec (ExistSolnVerb s) = s
mkExistingSolnSec (ExistSolnProg cs) = SRS.offShelfSol cs [] 

{--}

-- | Helper for making the 'Values of Auxiliary Constants' section
mkAuxConsSec :: AuxConstntSec -> Section
mkAuxConsSec (AuxConsVerb s) = s
mkAuxConsSec (AuxConsProg key listOfCons) = AC.valsOfAuxConstantsF key listOfCons

{--}

-- | Helper for making the bibliography section
mkBib :: BibRef -> Section
mkBib bib = SRS.reference [Bib bib] []

{--}

-- | Helper for making the 'Appendix' section
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxVerb s)  = s
mkAppndxSec (AppndxProg cs) = SRS.appendix cs []

{--}

-- Helper
siSys :: SystemInformation -> IdeaDict
siSys SI {_sys = sys} = nw sys

--BELOW IS IN THIS FILE TEMPORARILY--
--Creates Contents using an uid and description (passed in as a Sentence).
-- mkAssump :: String -> Sentence -> Contents
-- mkAssump i desc = Assumption $ ac' i desc

mkRequirement :: String -> Sentence -> String -> Contents
mkRequirement i desc shrtn = Requirement $ frc i desc (shortname' shrtn)

mkLklyChnk :: String -> Sentence -> String -> Contents
mkLklyChnk i desc shrtn = Change $ lc i desc (shortname' shrtn)

mkUnLklyChnk :: String -> Sentence -> String -> Contents
mkUnLklyChnk i desc shrtn = Change $ ulc i desc (shortname' shrtn)
