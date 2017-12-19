{-# LANGUAGE GADTs #-}
---------------------------------------------------------------------------
-- | Start the process of moving away from Document as the main internal
-- representation of information, to something more informative.
-- Over time, we'll want to have a cleaner separation, but doing that
-- all at once would break too much for too long.  So we start here
-- instead.
module Drasil.DocumentLanguage where

import Drasil.DocumentLanguage.Definitions
import Drasil.DocumentLanguage.Chunk.GenDefn
import Drasil.DocumentLanguage.Chunk.InstanceModel

import Language.Drasil

import Control.Lens ((^.))

import Drasil.Sections.TableOfUnits (table_of_units)
import Drasil.Sections.TableOfSymbols (table)
import Drasil.Sections.TableOfAbbAndAcronyms (table_of_abb_and_acronyms)
import qualified Drasil.SRS as SRS
import qualified Drasil.Sections.Introduction as Intro
import qualified Drasil.Sections.SpecificSystemDescription as SSD
import qualified Drasil.Sections.Stakeholders as Stk
import qualified Drasil.Sections.AuxiliaryConstants as AC
import qualified Drasil.Sections.ScopeOfTheProject as SotP
import qualified Drasil.Sections.TraceabilityMandGs as TMG
import qualified Drasil.Sections.GeneralSystDesc as GSD
import qualified Drasil.Sections.Requirements as R

import Data.Drasil.Concepts.Documentation (refmat, tOfSymb, reference)

import Data.Maybe (isJust)
import Data.List (sort, nub)
import Prelude hiding (id)

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
                | TraceabilitySec TraceabilitySec
                | AuxConstntSec AuxConstntSec
                | Bibliography BibRef
                | AppndxSec AppndxSec

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

type Topic = NWrapper

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

--FIXME: This needs to be updated for the requisite information in introductionF
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
  Client :: (NamedIdea a) => a -> Sentence -> StkhldrSub
  Cstmr  :: (NamedIdea a) => a -> StkhldrSub

{--}

data GSDSec = GSDVerb Section 
            | GSDProg [Section] Contents [Contents] [Section]
            | GSDProg2 [GSDSub]

data GSDSub where
  GSDSubVerb :: Section -> GSDSub
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
  PDProg :: (NamedIdea a) => Sentence -> a -> Sentence -> [Section] -> ProblemDescription
  
-- | Solution Characteristics Specification section
data SolChSpec where
  SCSVerb :: Section -> SolChSpec
  SCSProg :: [SCSSub] -> SolChSpec
  
-- | Solution Characteristics Specification subsections
data SCSSub where
  SCSSubVerb  :: Section -> SCSSub
  Assumptions :: {-Fields  ->-} Section -> Section -> Section -> Section -> Section -> [Contents] -> SCSSub --FIXME: temporary definition?
  TMs         :: Fields  -> [TheoryModel] -> SCSSub
  GDs         :: Fields  -> [GenDefn] -> DerivationDisplay -> SCSSub
  DDs         :: Fields  -> [QDefinition] -> DerivationDisplay -> SCSSub --FIXME: Need DD intro
  IMs         :: Fields  -> [InstanceModel] -> DerivationDisplay -> SCSSub
  Constraints :: Sentence -> Sentence -> Sentence -> [Contents] {-Fields  -> [UncertainWrapper] -> [ConstrainedChunk]-} -> SCSSub --FIXME: temporary definition?
--FIXME: Work in Progress ^
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

data TraceabilitySec = TraceabilityVerb Section | TraceabilityProg [Contents] [Sentence] [Contents] [Section]

{--}

-- | Values of Auxiliary Constants section
data AuxConstntSec = AuxConsProg CI [QDefinition] | AuxConsVerb Section

{--}

data AppndxSec = AppndxVerb Section | AppndxProg [Contents]

{--}

-- | Creates a document from a document description and system information
mkDoc :: DocDesc -> (NWrapper -> NWrapper -> Sentence) -> SystemInformation -> Document
mkDoc l comb si@(SI {_sys = sys, _kind = kind, _authors = authors}) = Document 
  ((nw kind) `comb` (nw sys)) (manyNames authors) (mkSections si l)

-- | Helper for creating the document sections
mkSections :: SystemInformation -> DocDesc -> [Section]
mkSections si l = foldr doit [] l
  where
    doit :: DocSection -> [Section] -> [Section]
    doit (Verbatim s)        ls = s : ls
    doit (RefSec rs)         ls = mkRefSec si rs : ls
    doit (IntroSec is)       ls = mkIntroSec si is : ls
    doit (StkhldrSec sts)    ls = mkStkhldrSec sts : ls
    doit (SSDSec ss)         ls = mkSSDSec si ss : ls
    doit (AuxConstntSec acs) ls = mkAuxConsSec acs : ls
    doit (Bibliography bib)  ls = mkBib bib : ls
    doit (GSDSec gs)         ls = mkGSDSec gs : ls 
    doit (ScpOfProjSec sop)  ls = mkScpOfProjSec sop : ls
    doit (ReqrmntSec r)      ls = mkReqrmntSec r : ls
    doit (LCsSec lc)         ls = mkLCsSec lc : ls
    doit (TraceabilitySec t) ls = mkTraceabilitySec t : ls
    doit (AppndxSec a)       ls = mkAppndxSec a : ls


-- | Helper for creating the reference section and subsections
mkRefSec :: SystemInformation -> RefSec -> Section
mkRefSec _  (RefVerb s) = s
mkRefSec si (RefProg c l) = section (titleize refmat) [c] (foldr (mkSubRef si) [] l)
  where
    mkSubRef :: SystemInformation -> RefTab -> [Section] -> [Section]
    mkSubRef (SI {_units = u})  TUnits   l' = table_of_units u (tuIntro defaultTUI) : l'
    mkSubRef (SI {_units = u}) (TUnits' con) l' = table_of_units u (tuIntro con) : l'
    mkSubRef (SI {_quants = v}) (TSymb con) l' = 
      (Section (titleize tOfSymb) 
      (map Con [tsIntro con, (table Equational (sort $ filter (hasStageSymbol Equational . getStagedS) (nub v)) at_start)])) : l'
    mkSubRef (SI {_concepts = cccs}) (TSymb' f con) l' = (mkTSymb cccs f con) : l'
    mkSubRef (SI {_sysinfodb = db}) TAandA l' = 
      (table_of_abb_and_acronyms $ sort $
      filter (isJust . getA) (nub $ elements (db ^. termTable))) : l'
    mkSubRef _              (TVerb s) l' = s : l'

-- | Helper for creating the table of symbols
mkTSymb :: (Quantity e, Concept e, Ord e) => 
  [e] -> LFunc -> [TSIntro] -> Section
mkTSymb v f c = Section (titleize tOfSymb) (map Con [tsIntro c, 
  table Equational (sort $ filter (hasStageSymbol Equational . getStagedS) (nub v)) (lf f)])
  where lf Term = at_start
        lf Defn = (^. defn)
        lf (TermExcept cs) = (\x -> if (x ^. id) `elem` (map (^. id) cs) then
          (x ^. defn) else (at_start x)) --Compare chunk ids, since we don't
          --actually care about the chunks themselves in LFunc.
        lf (DefnExcept cs) = (\x -> if (x ^. id) `elem` (map (^.id) cs) then
          (at_start x) else (x ^. defn))
        lf TAD = (\tDef -> titleize tDef :+: S ":" +:+ (tDef ^. defn))

-- | table of symbols constructor
tsymb, tsymb' :: [TSIntro] -> RefTab
tsymb intro = TSymb intro                -- ^ Default Term and given intro
tsymb' intro = TSymb' Defn intro         -- ^ Default Defn and given intro

-- | Custom table of symbols constructor
tsymb'' :: [TSIntro] -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro -- ^ Custom function and intro.

-- | table of symbols intro builder. Used by mkRefSec
tsIntro :: [TSIntro] -> Contents
tsIntro x = Paragraph $ foldr (+:+) (EmptyS) (map tsI x)

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
typogConvention ts = S "Throughout the document" `sC` (makeSentence ts)
  where makeSentence (x:[]) = tcon x :+: S "."
        makeSentence (x:y:[]) = tcon x +:+ S "and" +:+. tcon y
        makeSentence (x:y:z:[]) = tcon x `sC` tcon y `sC` S "and" +:+. tcon z
        makeSentence (x:xs) = tcon x `sC` makeSentence xs
        makeSentence _ = error "How did you get here?"
        tcon (Vector emph) = S ("symbols in " ++ show emph ++ 
                                " will represent vectors, and scalars otherwise")
        tcon (Verb s) = s

-- | symbolic convention writer.
symbConvention :: [Literature] -> Sentence
symbConvention [] = error "Attempting to reference no literature for SymbConvention"
symbConvention scs = S "The choice of symbols was made to be consistent with the" +:+
                      makeSentence scs
  where makeSentence (x:[]) = scon x :+: S "."
        makeSentence (x:y:[]) = scon x +:+ S "and with" +:+. scon y
        makeSentence (x:y:z:[]) = scon x `sC` scon y `sC` S "and" +:+. scon z
        makeSentence (x:xs) = scon x `sC` makeSentence xs
        makeSentence _ = error "How did you get here?"
        scon (Lit x) = phrase x +:+ S "literature"
        scon (Doc x) = S "existing documentation for" +:+ (phrase x)
        scon (Doc' x)   = S "existing documentation for" +:+ (plural x)
        scon (Manual x) = S "that used in the" +:+ (phrase x) +:+ S "manual"

-- | Table of units intro builder. Used by mkRefSec
tuIntro :: [TUIntro] -> Contents
tuIntro x = Paragraph $ foldr (+:+) (EmptyS) (map tuI x)

-- | table of units intro writer. Translates a TUIntro to a Sentence.
tuI :: TUIntro -> Sentence
tuI System  = (S "The unit system used throughout is SI (Syst" :+: 
  (F Grave 'e') :+: S "me International d'Unit" :+: (F Acute 'e') :+: S "s).")
tuI TUPurpose = S "For each unit, the table lists the symbol," +:+
  S "a description and the SI name."
tuI Derived = S "In addition to the basic units, several derived units are" +:+ 
  S "also used."

-- | Default table of units intro contains the 
defaultTUI :: [TUIntro]
defaultTUI = [System, Derived, TUPurpose]

mkIntroSec :: SystemInformation -> IntroSec -> Section
mkIntroSec _ (IntroVerb s) = s
mkIntroSec si (IntroProg probIntro progDefn l) = 
  Intro.introductionSection probIntro progDefn $ foldr (mkSubIntro si) [] l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> [Section] -> [Section]
    mkSubIntro _ (IVerb s) l' = s : l'
    mkSubIntro _ (IPurpose intro) l' = Intro.purposeOfDoc intro : l'
    mkSubIntro (SI {_sys = sys}) (IScope main intendedPurp) l' = 
      Intro.scopeOfRequirements main sys intendedPurp : l'
    mkSubIntro (SI {_sys = sys}) (IChar know understand appStandd) l' =
      Intro.charIntRdrF know understand sys appStandd (SRS.userChar [] []) : l'
    mkSubIntro _ (IOrgSec i b s t) l' = Intro.orgSec i b s t : l'
    -- FIXME: s should be "looked up" using "b" once we have all sections being generated

-- | Helper for making the 'Stakeholders' section
mkStkhldrSec :: StkhldrSec -> Section
mkStkhldrSec (StkhldrVerb s) = s
mkStkhldrSec (StkhldrProg key details) = (Stk.stakehldrGeneral key details) 
mkStkhldrSec (StkhldrProg2 l) = SRS.stakeholder [Stk.stakeholderIntro] $ foldr (mkSubs) [] l
  where
    mkSubs :: StkhldrSub -> [Section] -> [Section]
    mkSubs (StkhldrSubVerb s) l' = s : l'
    mkSubs (Client kWrd details) l' = (Stk.tClientF kWrd details) : l'
    mkSubs (Cstmr kWrd) l'          = (Stk.tCustomerF kWrd) : l'

-- | Helper for making the 'General System Description' section
mkGSDSec :: GSDSec -> Section
mkGSDSec (GSDVerb s) = s
mkGSDSec (GSDProg cntxt uI cnstrnts systSubSec) = GSD.genSysF cntxt uI cnstrnts systSubSec
mkGSDSec (GSDProg2 l) = SRS.genSysDes [GSD.genSysIntro] $ foldr (mkSubs) [] l
   where
     mkSubs :: GSDSub -> [Section] -> [Section]
     mkSubs (GSDSubVerb s) l' = s : l'
     mkSubs (UsrChars intro) l'   = (GSD.usrCharsF intro) : l'
     mkSubs (SystCons cntnts subsec) l' = (GSD.systCon cntnts subsec) : l'

-- | Helper for making the 'Scope of the Project' section
mkScpOfProjSec :: ScpOfProjSec -> Section
mkScpOfProjSec (ScpOfProjVerb s) = s
mkScpOfProjSec (ScpOfProjProg kWrd uCTCntnts indCases) = 
  SotP.scopeOfTheProjF kWrd uCTCntnts indCases

-- | Helper for making the 'Specific System Description' section
mkSSDSec :: SystemInformation -> SSDSec -> Section
mkSSDSec _ (SSDVerb s) = s
mkSSDSec si (SSDProg l) = 
  SSD.specSysDescr (siSys si) $ foldr (mkSubSSD si) [] l
  where
    mkSubSSD :: SystemInformation -> SSDSub -> [Section] -> [Section]
    mkSubSSD _ (SSDSubVerb s) l'        = s : l'
    mkSubSSD sysi (SSDProblem pd) l'    = mkSSDProb sysi pd : l'
    mkSubSSD sysi (SSDSolChSpec scs) l' = mkSolChSpec sysi scs : l'

mkSSDProb :: SystemInformation -> ProblemDescription -> Section
mkSSDProb _ (PDVerb s) = s
mkSSDProb _ (PDProg start progName end subSec) = 
  SSD.probDescF start progName end subSec

mkSolChSpec :: SystemInformation -> SolChSpec -> Section
mkSolChSpec _ (SCSVerb s) = s
mkSolChSpec si (SCSProg l) = 
  SRS.solCharSpec [SSD.solutionCharSpecIntro (siSys si) inModSec] $ 
    foldr (mkSubSCS si) [] l
  where
    mkSubSCS :: SystemInformation -> SCSSub -> [Section] -> [Section]
    mkSubSCS _ (SCSSubVerb s) l' = s : l'
    mkSubSCS _ (TMs _ []) _   = error "There are no Theoretical Models"
    mkSubSCS _ (GDs _ [] _) l' = SSD.genDefnF [] : l'
    mkSubSCS _ (DDs _ [] _) _ = error "There are no Data Definitions"
    mkSubSCS _ (IMs _ [] _) _   = error "There are no Instance Models"
    mkSubSCS si' (TMs fields ts) l' = 
      SSD.thModF (siSys si') (map (tmodel fields (_sysinfodb si')) ts) : l'
    mkSubSCS si' (DDs fields dds ShowDerivation) l' = --FIXME: need to keep track of DD intro.
      SSD.dataDefnF EmptyS (concat (map (\x -> ddefn fields (_sysinfodb si') x : derivation x) dds)) : l'
    mkSubSCS si' (DDs fields dds _) l' =
      SSD.dataDefnF EmptyS (map (ddefn fields (_sysinfodb si')) dds) : l'
    mkSubSCS si' (GDs fields gs ShowDerivation) l' = 
      SSD.genDefnF (concat (map (\x -> gdefn fields (_sysinfodb si') x : derivation x) gs)) : l'
    mkSubSCS si' (GDs fields gs _) l' = 
      SSD.genDefnF (map (gdefn fields (_sysinfodb si')) gs) : l'
    mkSubSCS si' (IMs fields ims ShowDerivation) l' = 
      SRS.inModel (concat (map (\x -> instanceModel fields (_sysinfodb si') x : derivation x) ims)) [] : l'
    mkSubSCS si' (IMs fields ims _) l' = SRS.inModel 
      (map (instanceModel fields (_sysinfodb si')) ims) [] : l'
    mkSubSCS _ (Assumptions r1 r2 r3 r4 r5 o) l' = (SSD.assumpF r1 r2 r3 r4 r5 o) : l'
    mkSubSCS _ (Constraints a b c d) l' = (SSD.datConF a b c d) : l'
    inModSec = (SRS.inModel [Paragraph EmptyS] []) 
    --FIXME: inModSec should be replaced with a walk
    -- over the SCSProg and generate a relevant intro.
    -- Could start with just a quick check of whether or not IM is included and 
    -- then error out if necessary.
    
{--}

-- | Helper for making the 'Requirements' section
mkReqrmntSec :: ReqrmntSec -> Section
mkReqrmntSec (ReqsVerb s) = s
mkReqrmntSec (ReqsProg l) = R.reqF $ foldr (mkSubs) [] l
  where
    mkSubs :: ReqsSub -> [Section] -> [Section]
    mkSubs (ReqsSubVerb s) l' = s : l'
    mkSubs (FReqsSub reqs) l'   = (R.fReqF reqs) : l'
    mkSubs (NonFReqsSub noPrrty prrty rsn explain) l' = 
      (R.nonFuncReqF noPrrty prrty rsn explain) : l'

{--}
 
-- | Helper for making the 'LikelyChanges' section
mkLCsSec :: LCsSec -> Section
mkLCsSec (LCsVerb s) = s
mkLCsSec (LCsProg c) = SRS.likeChg c []

{--}

-- | Helper for making the 'Traceability Matrices and Graphs' section
mkTraceabilitySec :: TraceabilitySec -> Section
mkTraceabilitySec (TraceabilityVerb s) = s
mkTraceabilitySec (TraceabilityProg refs trailing otherContents subSec) = 
  TMG.traceMGF refs trailing otherContents subSec

{--}
  
-- | Helper for making the 'Values of Auxiliary Constants' section
mkAuxConsSec :: AuxConstntSec -> Section
mkAuxConsSec (AuxConsVerb s) = s
mkAuxConsSec (AuxConsProg key listOfCons) = (AC.valsOfAuxConstantsF key listOfCons)

{--}

-- | Helper for making the bibliography section
mkBib :: BibRef -> Section
mkBib bib = section (titleize' reference) [Bib bib] []

{--}

-- | Helper for making the 'Appendix' section
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxVerb s)  = s
mkAppndxSec (AppndxProg cs) = SRS.appendix cs []

{--}

-- Helper
siSys :: SystemInformation -> NWrapper
siSys (SI {_sys = sys}) = nw sys

--BELOW IS IN THIS FILE TEMPORARILY--
--Creates Contents using an id and description (passed in as a Sentence).
mkAssump :: String -> Sentence -> Contents
mkAssump i desc = Assumption $ nw $ npnc i (nounPhraseSent desc)

mkAssumpCustom :: String -> Sentence -> String -> Contents
mkAssumpCustom i desc enid = Assumption $ nw $ npnc' i (nounPhraseSent desc) enid

mkRequirement :: String -> Sentence -> Contents
mkRequirement i desc = Requirement $ nw $ npnc i $ nounPhraseSent desc

mkRequirementCustom :: String -> Sentence -> String -> Contents
mkRequirementCustom i desc enid = Requirement $ nw $ npnc' i (nounPhraseSent desc) enid

mkLklyChnk :: String -> Sentence -> Contents
mkLklyChnk i desc = LikelyChange $ nw $ npnc i $ nounPhraseSent desc

mkLklyChnkCustom :: String -> Sentence -> String -> Contents
mkLklyChnkCustom i desc enid = LikelyChange $ nw $ npnc' i (nounPhraseSent desc) enid
