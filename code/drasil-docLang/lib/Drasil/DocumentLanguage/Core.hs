{-# LANGUAGE GADTs #-}
-- | Defines core types for use with the Drasil document language ("Drasil.DocumentLanguage").
module Drasil.DocumentLanguage.Core where

import Drasil.DocumentLanguage.Definitions (Fields)
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat)
import Language.Drasil hiding (Manual, Verb) -- Manual - Citation name conflict. FIXME: Move to different namespace
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)


import Data.Generics.Multiplate (Multiplate(multiplate, mkPlate))

-- | Type synonym for clarity.
type System = Sentence
-- | Type synonym for clarity.
type DocKind = Sentence

-- * Document Types

-- | A document description is made up of document sections.
type DocDesc = [DocSection]

-- | Document sections are either Reference, Introduction, or Specific
-- System Description sections (for now!).
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

-- ** Reference Material Section

-- | Reference section. Contents are top level followed by a list of subsections.
data RefSec = RefProg Contents [RefTab]

-- | Reference subsections (tables) made out of units or symbols (can be customized).
data RefTab where
  -- | Default table of units.
  TUnits :: RefTab
  -- | Customized introduction.
  TUnits' :: [TUIntro] -> ([UnitDefn] -> LabelledContent) -> RefTab
  -- | Adds an introduction for a table of symbols.
  TSymb :: [TSIntro] -> RefTab
  -- | Allows Lens functions in addition to an introduction for a table of symbols.
  TSymb' :: LFunc -> [TSIntro] -> RefTab
  -- | Default.
  TAandA :: RefTab
  -- add more here

-- | For creating a table of symbols introduction
data TSIntro = TypogConvention [TConvention] -- ^ Typographic conventions used.
             | SymbOrder -- ^ Symbol ordering (defaults to alphabetical).
             | SymbConvention [Literature] -- ^ Symbol conventions match specified literature.
             | TSPurpose -- ^ Purpose of the Table of Symbols.
             | VectorUnits -- ^ Definition of vector components.

-- | Possible typographic conventions.
data TConvention = Vector Emphasis -- ^ How vectors are emphasized.
                 | Verb Sentence -- ^ Verbatim for specialized conventions.

-- | How to handle emphasis of words.
data Emphasis = Bold
              | Italics

instance Show Emphasis where
  show Bold = "bold"
  show Italics = "italics"

-- | Types of literature.
data Literature = Lit Topic -- ^ Literature (with a Topic).
                | Doc Topic -- ^ Existing documentation for (singular topic).
                | Doc' Topic -- ^ Existing documentation for (plural version of topic).
                | Manual Topic -- ^ Manual.

-- | Type synonym for clarity.
type Topic = IdeaDict

-- | For creating the table of units introduction.
data TUIntro = System -- ^ System of units (defaults to SI).
             | Derived -- ^ Sentence about derived units being used alongside SI.
             | TUPurpose -- ^ Purpose of the table of units.

-- | Lens (lookup) functions (currently for TSymb).
data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: [DefinedQuantityDict] -> LFunc
  DefnExcept :: [DefinedQuantityDict] -> LFunc
  TAD :: LFunc -- ^ Term and Definition.

-- ** Introduction Section

-- | Introduction section. Contents are top level followed by a list of
-- subsections.
data IntroSec = IntroProg Sentence Sentence [IntroSub]
  -- ^ Temporary, will be modified once we've figured out more about the section.

-- | Introduction subsections.
data IntroSub where
  -- | Describes purpose of the system.
  IPurpose :: [Sentence] -> IntroSub
  -- | Describes scope of the system.
  IScope   :: Sentence -> IntroSub
  -- | Describes characteristics of the system.
  IChar   :: [Sentence] -> [Sentence] -> [Sentence] -> IntroSub
  -- | Organises the section.
  IOrgSec  :: CI -> Section -> Maybe Sentence -> IntroSub

-- ** Stakeholders Section

-- | Stakeholders section (wraps stakeholders subsections 'StkhldrSub').
newtype StkhldrSec = StkhldrProg [StkhldrSub]

-- | Stakeholders subsections.
data StkhldrSub where
  -- | May have a client.
  Client :: CI -> Sentence -> StkhldrSub
  -- | May have a customer.
  Cstmr  :: CI -> StkhldrSub

-- ** General System Description Section

-- | General System Description section (wraps 'GSDSub' subsections).
newtype GSDSec = GSDProg [GSDSub]

-- | General System Description subsections.
data GSDSub where
  -- | System context.
  SysCntxt   :: [Contents] -> GSDSub --FIXME: partially automate
  -- | User characteristics.
  UsrChars   :: [Contents] -> GSDSub
  -- | System constraints.
  SystCons   :: [Contents] -> [Section] -> GSDSub

-- ** Specific System Description Section

-- | Specific System Description section. Contains a list of subsections ('SSDSub').
newtype SSDSec = SSDProg [SSDSub]

-- | Specific System Description subsections.
data SSDSub where
  -- | System description problems.
  SSDProblem :: ProblemDescription -> SSDSub
  -- | Solution characteristics specification.
  SSDSolChSpec :: SolChSpec -> SSDSub

-- | Problem Description section. Contains an intro or title,
-- 'Section's, and problem description subsections ('PDSub').
data ProblemDescription where
  PDProg :: Sentence -> [Section] -> [PDSub] -> ProblemDescription

-- | Problem Description subsections.
data PDSub where
  -- | Terms and definitions.
  TermsAndDefs :: Concept c => Maybe Sentence -> [c] -> PDSub
  -- | Physical system description.
  PhySysDesc :: Idea a => a -> [Sentence] -> LabelledContent -> [Contents] -> PDSub
  -- | Goals.
  Goals :: [Sentence] -> [ConceptInstance] -> PDSub

-- | Solution Characteristics Specification section. Contains a list of subsections ('SCSSub').
data SolChSpec where
  SCSProg :: [SCSSub] -> SolChSpec

-- | Solution Characteristics Specification subsections.
data SCSSub where
  -- | Assumptions.
  Assumptions    :: [ConceptInstance] -> SCSSub
  -- | Theory Models.
  TMs            :: [Sentence] -> Fields  -> [TheoryModel] -> SCSSub
  -- | General Definitions.
  GDs            :: [Sentence] -> Fields  -> [GenDefn] -> DerivationDisplay -> SCSSub
  -- | Data Definitions.
  DDs            :: [Sentence] -> Fields  -> [DataDefinition] -> DerivationDisplay -> SCSSub -- (FIXME: Need DD intro).
  -- | Instance Models.
  IMs            :: [Sentence] -> Fields  -> [InstanceModel] -> DerivationDisplay -> SCSSub
  -- | Constraints.
  Constraints    :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => Sentence -> [c] -> SCSSub
  --                  Sentence -> [LabelledContent] Fields  -> [UncertainWrapper] -> [ConstrainedChunk] -> SCSSub --FIXME: temporary definition?
  --FIXME: Work in Progress ^
  -- | Properties of a correct solution.
  CorrSolnPpties :: (Quantity c, Constrained c) => [c] -> [Contents] -> SCSSub

-- | Choose whether to show or hide the derivation of an expression.
data DerivationDisplay = ShowDerivation
                       | HideDerivation

-- ** Requirements Section

-- | Requirements section. Contains a list of subsections ('ReqsSub').
newtype ReqrmntSec = ReqsProg [ReqsSub]

-- | Requirements subsections.
data ReqsSub where
  -- | Functional requirements. LabelledContent needed for tables.
  FReqsSub'   :: [ConceptInstance] -> [LabelledContent] -> ReqsSub
  -- | Functional requirements. LabelledContent needed for tables.
  FReqsSub    :: [ConceptInstance] -> [LabelledContent] -> ReqsSub
  -- | Non-functional requirements.
  NonFReqsSub :: [ConceptInstance] -> ReqsSub

-- ** Likely Changes Section

-- | Likely Changes section.
newtype LCsSec = LCsProg [ConceptInstance]

-- ** Unlikely Changes Section

-- | Unlikely Changes section.
newtype UCsSec = UCsProg [ConceptInstance]

-- ** Traceability Section

-- | Traceability Matices and Graphs section. Contains configurations ('TraceConfig').
newtype TraceabilitySec = TraceabilityProg [TraceConfig]

-- | Traceability Matices and Graphs configurations.
data TraceConfig = TraceConfig UID [Sentence] Sentence [TraceViewCat] [TraceViewCat]

getTraceConfigUID :: TraceConfig -> UID
getTraceConfigUID (TraceConfig a _ _ _ _) = a

-- ** Off-The-Shelf Solutions Section

-- | Off-The-Shelf Solutions section.
newtype OffShelfSolnsSec = OffShelfSolnsProg [Contents]

-- ** Values of Auxiliary Constants Section

-- | Values of Auxiliary Constants section.
data AuxConstntSec = AuxConsProg CI [ConstQDef]

-- ** Appendix Section

-- | Appendix section.
newtype AppndxSec = AppndxProg [Contents]

-- * Multiplate Definition and Type

-- | Holds all of the different kinds of sections. Defines as a plate with an applicative functor.
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

-- | Holds boilerplate code to make getting sections easier.
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
    intro' (IOrgSec c sect s2) = pure $ IOrgSec c sect s2
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
