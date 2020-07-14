{-# LANGUAGE GADTs #-}
module Drasil.DocumentLanguage.Core where

import Drasil.DocumentLanguage.Definitions (Fields)
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat)
import Language.Drasil hiding (Manual, Vector, Verb) -- Manual - Citation name conflict. FIXME: Move to different namespace
                                                     -- Vector - Name conflict (defined in file)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)


import Data.Generics.Multiplate (Multiplate(multiplate, mkPlate))

type System = Sentence
type DocKind = Sentence

{--}

type DocDesc = [DocSection]

-- | Document sections are either Reference, Introduction, or Specific
-- System Description sections (for now!)
data DocSection = TableOfContents
                | RefSec RefSec
                | IntroSec IntroSec
                | StkhldrSec StkhldrSec
                | GSDSec GSDSec
                | SSDSec SSDSec
                | ReqrmntSec ReqrmntSec
                | LCsSec LCsSec
                | UCsSec UCsSec
                | TraceabilitySec TraceabilitySec
                | AuxConstntSec AuxConstntSec
                | Bibliography
                | AppndxSec AppndxSec
                | OffShelfSolnsSec OffShelfSolnsSec

{--}

-- | Reference section. Contents are top level followed by a list of subsections.
data RefSec = RefProg Contents [RefTab]

-- | Reference subsections
data RefTab where
  TUnits :: RefTab
  TUnits' :: [TUIntro] -> ([UnitDefn] -> LabelledContent) -> RefTab -- Customized intro
  TSymb :: [TSIntro] -> RefTab
  TSymb' :: LFunc -> [TSIntro] -> RefTab
  TAandA :: RefTab
  -- add more here

-- | For creating the table of symbols intro
data TSIntro = TypogConvention [TConvention] -- ^ Typographic conventions used
             | SymbOrder -- ^ Symbol ordering (defaults to alphabetical)
             | SymbConvention [Literature] -- ^ Symbol conventions match specified literature
             | TSPurpose -- ^ Purpose of the Table of Symbols
             | VectorUnits -- ^ Definition of vector components

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
  TermExcept :: [DefinedQuantityDict] -> LFunc
  DefnExcept :: [DefinedQuantityDict] -> LFunc
  TAD :: LFunc --Term and Definition

{--}

-- | Introduction section. Contents are top level followed by a list of
-- subsections.
data IntroSec = IntroProg Sentence Sentence [IntroSub]
  -- ^ Temporary, will be modified once we've figured out more about the section.

-- | Introduction subsections
data IntroSub where
  IPurpose :: [Sentence] -> IntroSub
  IScope   :: Sentence -> IntroSub
  IChar   :: [Sentence] -> [Sentence] -> [Sentence] -> IntroSub
  IOrgSec  :: Sentence -> CI -> Section -> Sentence -> IntroSub

{--}

-- | Stakeholders section
newtype StkhldrSec = StkhldrProg [StkhldrSub]

-- | Stakeholders subsections
data StkhldrSub where
  Client :: CI -> Sentence -> StkhldrSub
  Cstmr  :: CI -> StkhldrSub

{--}

newtype GSDSec = GSDProg [GSDSub]

data GSDSub where
  SysCntxt   :: [Contents] -> GSDSub --FIXME: partially automate
  UsrChars   :: [Contents] -> GSDSub
  SystCons   :: [Contents] -> [Section] -> GSDSub

{--}

-- | Specific System Description section . Contains a list of subsections.
newtype SSDSec = SSDProg [SSDSub]

-- | Specific system description subsections
data SSDSub where
  SSDProblem :: ProblemDescription -> SSDSub
  SSDSolChSpec :: SolChSpec -> SSDSub

-- | Problem Description section
data ProblemDescription where
  PDProg :: Sentence -> [Section] -> [PDSub] -> ProblemDescription

-- | Problem Description subsections
data PDSub where
  TermsAndDefs :: Concept c => Maybe Sentence -> [c] -> PDSub
  PhySysDesc :: Idea a => a -> [Sentence] -> LabelledContent -> [Contents] -> PDSub
  Goals :: [Sentence] -> [ConceptInstance] -> PDSub

-- | Solution Characteristics Specification section
data SolChSpec where
  SCSProg :: [SCSSub] -> SolChSpec

-- | Solution Characteristics Specification subsections
data SCSSub where
  Assumptions    :: [ConceptInstance] -> SCSSub
  TMs            :: [Sentence] -> Fields  -> [TheoryModel] -> SCSSub
  GDs            :: [Sentence] -> Fields  -> [GenDefn] -> DerivationDisplay -> SCSSub
  DDs            :: [Sentence] -> Fields  -> [DataDefinition] -> DerivationDisplay -> SCSSub --FIXME: Need DD intro
  IMs            :: [Sentence] -> Fields  -> [InstanceModel] -> DerivationDisplay -> SCSSub
  Constraints    :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => Sentence -> [c] -> SCSSub
--                  Sentence -> [LabelledContent] Fields  -> [UncertainWrapper] -> [ConstrainedChunk] -> SCSSub --FIXME: temporary definition?
--FIXME: Work in Progress ^
  CorrSolnPpties :: (Quantity c, Constrained c) => [c] -> [Contents] -> SCSSub
data DerivationDisplay = ShowDerivation
                       | HideDerivation
{--}

newtype ReqrmntSec = ReqsProg [ReqsSub]

data ReqsSub where
  FReqsSub'   :: [ConceptInstance] -> [LabelledContent] -> ReqsSub -- LabelledContent for tables
  FReqsSub    :: [ConceptInstance] -> [LabelledContent] -> ReqsSub -- LabelledContent for tables
  NonFReqsSub :: [ConceptInstance] -> ReqsSub

{--}

newtype LCsSec = LCsProg [ConceptInstance]

{--}

newtype UCsSec = UCsProg [ConceptInstance]

{--}

newtype TraceabilitySec = TraceabilityProg [TraceConfig]

data TraceConfig = TraceConfig UID [Sentence] Sentence [TraceViewCat] [TraceViewCat]

{--}

-- | Off-The-Shelf Solutions section 
newtype OffShelfSolnsSec = OffShelfSolnsProg [Contents]

{--}

-- | Values of Auxiliary Constants section
data AuxConstntSec = AuxConsProg CI [QDefinition]

{--}

newtype AppndxSec = AppndxProg [Contents]

{--}

data DLPlate f = DLPlate {
  docSec :: DocSection -> f DocSection,
  refSec :: RefSec -> f RefSec,
  introSec :: IntroSec -> f IntroSec,
  introSub :: IntroSub -> f IntroSub,
  stkSec :: StkhldrSec -> f StkhldrSec,
  stkSub :: StkhldrSub -> f StkhldrSub,
  gsdSec :: GSDSec -> f GSDSec,
  gsdSub :: GSDSub -> f GSDSub,
  ssdSec :: SSDSec -> f SSDSec,
  ssdSub :: SSDSub -> f SSDSub,
  pdSec :: ProblemDescription -> f ProblemDescription,
  pdSub :: PDSub -> f PDSub,
  scsSub :: SCSSub -> f SCSSub,
  reqSec :: ReqrmntSec -> f ReqrmntSec,
  reqSub :: ReqsSub -> f ReqsSub,
  lcsSec :: LCsSec -> f LCsSec,
  ucsSec :: UCsSec -> f UCsSec,
  traceSec :: TraceabilitySec -> f TraceabilitySec,
  offShelfSec :: OffShelfSolnsSec -> f OffShelfSolnsSec,
  auxConsSec :: AuxConstntSec -> f AuxConstntSec,
  appendSec :: AppndxSec -> f AppndxSec
}

instance Multiplate DLPlate where
  multiplate p = DLPlate ds res intro intro' stk stk' gs gs' ss ss' pd pd' sc
    rs rs' lcp ucp ts es acs aps where
    ds TableOfContents = pure TableOfContents
    ds (RefSec x) = RefSec <$> refSec p x
    ds (IntroSec x) = IntroSec <$> introSec p x
    ds (StkhldrSec x) = StkhldrSec <$> stkSec p x
    ds (GSDSec x) = GSDSec <$> gsdSec p x
    ds (SSDSec x) = SSDSec <$> ssdSec p x
    ds (ReqrmntSec x) = ReqrmntSec <$> reqSec p x
    ds (LCsSec x) = LCsSec <$> lcsSec p x
    ds (UCsSec x) = UCsSec <$> ucsSec p x
    ds (TraceabilitySec x) = TraceabilitySec <$> traceSec p x
    ds (OffShelfSolnsSec x) = OffShelfSolnsSec <$> offShelfSec p x
    ds (AuxConstntSec x) = AuxConstntSec <$> auxConsSec p x
    ds (AppndxSec x) = AppndxSec <$> appendSec p x
    ds Bibliography = pure Bibliography

    res (RefProg c x) = pure $ RefProg c x
    intro (IntroProg s1 s2 progs) = IntroProg s1 s2 <$>
      traverse (introSub p) progs
    intro' (IPurpose s) = pure $ IPurpose s
    intro' (IScope s) = pure $ IScope s
    intro' (IChar s1 s2 s3) = pure $ IChar s1 s2 s3
    intro' (IOrgSec s1 c sect s2) = pure $ IOrgSec s1 c sect s2
    stk (StkhldrProg progs) = StkhldrProg <$> traverse (stkSub p) progs
    stk' (Client c s) = pure $ Client c s
    stk' (Cstmr c) = pure (Cstmr c)
    gs (GSDProg x) = GSDProg <$> traverse (gsdSub p) x
    gs' (SysCntxt c) = pure $ SysCntxt c
    gs' (UsrChars c) = pure $ UsrChars c
    gs' (SystCons c s) = pure $ SystCons c s
    ss (SSDProg progs) = SSDProg <$> traverse (ssdSub p) progs
    ss' (SSDProblem prog) = SSDProblem <$> pdSec p prog
    ss' (SSDSolChSpec (SCSProg spec)) = SSDSolChSpec . SCSProg <$> traverse (scsSub p) spec
    pd (PDProg s sect progs) = PDProg s sect <$> traverse (pdSub p) progs
    pd' (TermsAndDefs s cs) = pure $ TermsAndDefs s cs
    pd' (Goals s ci) = pure $ Goals s ci
    pd' (PhySysDesc nm s lc c) = pure $ PhySysDesc nm s lc c
    sc (Assumptions c) = pure (Assumptions c)
    sc (TMs s f t) = pure $ TMs s f t
    sc (GDs s f g d) = pure $ GDs s f g d
    sc (DDs s f dd d) = pure $ DDs s f dd d
    sc (IMs s f i d) = pure $ IMs s f i d 
    sc (Constraints s c) = pure $ Constraints s c
    sc (CorrSolnPpties c cs) = pure $ CorrSolnPpties c cs
    rs (ReqsProg reqs) = ReqsProg <$> traverse (reqSub p) reqs
    rs' (FReqsSub ci con) = pure $ FReqsSub ci con
    rs' (FReqsSub' ci con) = pure $ FReqsSub' ci con
    rs' (NonFReqsSub c) = pure $ NonFReqsSub c
    lcp (LCsProg c) = pure $ LCsProg c
    ucp (UCsProg c) = pure $ UCsProg c
    ts (TraceabilityProg progs) = pure $ TraceabilityProg progs
    es (OffShelfSolnsProg contents) = pure $ OffShelfSolnsProg contents
    acs (AuxConsProg ci qdef) = pure $ AuxConsProg ci qdef
    aps (AppndxProg con) = pure $ AppndxProg con
  mkPlate b = DLPlate (b docSec) (b refSec) (b introSec) (b introSub) (b stkSec)
    (b stkSub) (b gsdSec) (b gsdSub) (b ssdSec) (b ssdSub) (b pdSec) (b pdSub)
    (b scsSub) (b reqSec) (b reqSub) (b lcsSec) (b ucsSec)
    (b traceSec) (b offShelfSec) (b auxConsSec) (b appendSec)
