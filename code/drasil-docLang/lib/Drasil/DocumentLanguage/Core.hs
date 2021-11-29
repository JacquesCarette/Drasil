{-# LANGUAGE GADTs #-}
-- | Defines core types for use with the Drasil document language ("Drasil.DocumentLanguage").
module Drasil.DocumentLanguage.Core where

import Drasil.DocumentLanguage.Definitions (Fields)
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat)
import Language.Drasil hiding (Manual, Verb, constraints) -- Manual - Citation name conflict. FIXME: Move to different namespace
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
                | IPurposeSub IPurposeSub
                | IScopeSub IScopeSub
                | ICharSub ICharSub
                | IOrgSub IOrgSub
                | StkhldrSec StkhldrSec
                | ClientSub ClientSub 
                | CstmrSub CstmrSub
                | GSDSec GSDSec
                | SysCntxtSub SysCntxtSub
                | UsrCharsSub UsrCharsSub
                | SystConsSub SystConsSub
                | SSDSec SSDSec
                | ProblemDescription ProblemDescription 
                | TermsAndDefs TermsAndDefs
                | PhySysDesc PhySysDesc
                | Goals Goals                
                | SolChSpec SolChSpec 
                | Assumptions Assumptions
                | TMs TMs
                | GDs GDs
                | DDs DDs
                | IMs IMs
                | Constraints Constraints
                | CorrSolnPpties CorrSolnPpties
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
data IntroSec = IntroProg Sentence Sentence
  -- ^ Temporary, will be modified once we've figured out more about the section.

-- | Introduction subsections.
-- | Describes purpose of the system.
newtype IPurposeSub = IPurposeProg [Sentence] 
-- | Describes scope of the system.
newtype IScopeSub = IScopeProg Sentence 
-- | Describes characteristics of the system.
data ICharSub = ICharProg [Sentence] [Sentence] [Sentence]
-- | Organises the section.
data IOrgSub = IOrgProg Sentence CI Section Sentence

-- ** Stakeholders Section

-- | Stakeholders section (wraps stakeholders subsections 'StkhldrSub').
newtype StkhldrSec = StkhldrProg Sentence

-- | Stakeholders subsections.
-- | May have a client.
data ClientSub = ClientProg CI Sentence
-- | May have a customer.
newtype CstmrSub = CstmrProg CI 

-- ** General System Description Section

-- | General System Description section (wraps 'GSDSub' subsections).
newtype GSDSec = GSDProg Sentence

-- | General System Description subsections.
-- | System context.
newtype SysCntxtSub = SysCntxtProg [Contents] --FIXME: partially automate
-- | User characteristics.
newtype UsrCharsSub = UsrCharsProg [Contents]  
-- | System constraints. **used to be [Contents] [Section] 
newtype SystConsSub = SystConsProg [Contents] 
-- | General System Description subsections.


-- ** Specific System Description Section

-- | Specific System Description section. Contains a list of subsections ('SSDSub').
newtype SSDSec = SSDProg Sentence

-- | Problem Description section. Contains an intro or title,
-- 'Section's, and problem description subsections ('PDSub').
newtype ProblemDescription = PDProg Sentence

-- | Problem Description subsections.
-- | Terms and definitions.
data TermsAndDefs where 
  TDProg :: Concept c => Maybe Sentence -> [c] -> TermsAndDefs
-- | Physical system description.
data PhySysDesc where
  PSDProg :: Idea a => a -> [Sentence] -> LabelledContent -> [Contents] -> PhySysDesc
-- | Goals.
data Goals = GProg [Sentence] [ConceptInstance] 

-- | Solution Characteristics Specification section. Contains a list of subsections ('SCSSub').
newtype SolChSpec = SCSProg Sentence

-- | Solution Characteristics Specification subsections.
-- | Assumptions.
newtype Assumptions = AssumpProg [ConceptInstance]
-- | Theory Models.
data TMs = TMProg [Sentence] Fields [TheoryModel] 
-- | General Definitions.
data GDs = GDProg [Sentence] Fields [GenDefn] DerivationDisplay
-- | Data Definitions.
data DDs = DDProg [Sentence] Fields [DataDefinition] DerivationDisplay -- (FIXME: Need DD intro).
-- | Instance Models.
data IMs = IMProg [Sentence] Fields [InstanceModel] DerivationDisplay
-- | Constraints.
data Constraints where
  ConstProg :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => Sentence -> [c] -> Constraints 
--Sentence -> [LabelledContent] Fields  -> [UncertainWrapper] -> [ConstrainedChunk] -> SCSSub --FIXME: temporary definition?
--FIXME: Work in Progress ^
-- | Properties of a correct solution.
data CorrSolnPpties where
  CorrSolProg :: (Quantity c, Constrained c) => [c] -> [Contents] -> CorrSolnPpties

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
data AuxConstntSec = AuxConsProg CI [SimpleQDef]

-- ** Appendix Section

-- | Appendix section.
newtype AppndxSec = AppndxProg [Contents]

-- * Multiplate Definition and Type

-- | Holds all of the different kinds of sections. Defines as a plate with an applicative functor.
data DLPlate f = DLPlate {
  docSec :: DocSection -> f DocSection,
  refSec :: RefSec -> f RefSec,
  introSec :: IntroSec -> f IntroSec,
  iPurposeSub :: IPurposeSub -> f IPurposeSub,
  iScopeSub :: IScopeSub -> f IScopeSub,
  iCharSub :: ICharSub -> f ICharSub,
  iOrgSub :: IOrgSub -> f IOrgSub,
  stkSec :: StkhldrSec -> f StkhldrSec,
  clientSub :: ClientSub -> f ClientSub,
  cstmrSub :: CstmrSub -> f CstmrSub,
  gsdSec :: GSDSec -> f GSDSec,
  sysCntxtSub :: SysCntxtSub -> f SysCntxtSub,
  usrCharsSub :: UsrCharsSub -> f UsrCharsSub,
  systConsSub :: SystConsSub -> f SystConsSub,
  ssdSec :: SSDSec -> f SSDSec,
  problemDescription  :: ProblemDescription  -> f ProblemDescription ,
  termsAndDefs :: TermsAndDefs -> f TermsAndDefs,
  phySysDesc :: PhySysDesc -> f PhySysDesc,
  goals :: Goals -> f Goals,
  solChSpec :: SolChSpec -> f SolChSpec,
  assumptions :: Assumptions -> f Assumptions,
  tMs :: TMs -> f TMs,
  gDs :: GDs -> f GDs,
  dDs :: DDs -> f DDs,
  iMs :: IMs -> f IMs,
  constraints :: Constraints -> f Constraints,
  corrSolnPpties :: CorrSolnPpties -> f CorrSolnPpties,
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
  multiplate p = DLPlate ds res intro ipurp iscope ichar iorg stk client cstmr gs syscnt uschr syscon
    ss pd td psd gl sc a tm gd dd im ct csp rs rs' lcp ucp ts es acs aps where
    ds TableOfContents = pure TableOfContents
    ds (RefSec x) = RefSec <$> refSec p x
    ds (IntroSec x) = IntroSec <$> introSec p x
    ds (IPurposeSub x) = IPurposeSub <$> iPurposeSub p x
    ds (IScopeSub x) = IScopeSub <$> iScopeSub p x
    ds (ICharSub x) = ICharSub <$> iCharSub p x
    ds (IOrgSub x) = IOrgSub <$> iOrgSub p x
    ds (StkhldrSec x) = StkhldrSec <$> stkSec p x
    ds (ClientSub x) = ClientSub <$> clientSub p x
    ds (CstmrSub x) = CstmrSub <$> cstmrSub p x
    ds (GSDSec x) = GSDSec <$> gsdSec p x
    ds (SysCntxtSub x) = SysCntxtSub <$> sysCntxtSub p x
    ds (UsrCharsSub x) = UsrCharsSub <$> usrCharsSub p x
    ds (SystConsSub x) = SystConsSub <$> systConsSub p x
    ds (SSDSec x) = SSDSec <$> ssdSec p x
    ds (ProblemDescription  x) = ProblemDescription  <$> problemDescription  p x
    ds (TermsAndDefs x) = TermsAndDefs <$> termsAndDefs p x
    ds (PhySysDesc x) = PhySysDesc <$> phySysDesc p x
    ds (Goals x) = Goals <$> goals p x
    ds (SolChSpec x) = SolChSpec <$> solChSpec p x
    ds (Assumptions x) = Assumptions <$> assumptions p x
    ds (TMs x) = TMs <$> tMs p x
    ds (GDs x) = GDs <$> gDs p x
    ds (DDs x) = DDs <$> dDs p x
    ds (IMs x) = IMs <$> iMs p x
    ds (Constraints x) = Constraints <$> constraints p x
    ds (CorrSolnPpties x) = CorrSolnPpties <$> corrSolnPpties p x
    ds (ReqrmntSec x) = ReqrmntSec <$> reqSec p x
    ds (LCsSec x) = LCsSec <$> lcsSec p x
    ds (UCsSec x) = UCsSec <$> ucsSec p x
    ds (TraceabilitySec x) = TraceabilitySec <$> traceSec p x
    ds (OffShelfSolnsSec x) = OffShelfSolnsSec <$> offShelfSec p x
    ds (AuxConstntSec x) = AuxConstntSec <$> auxConsSec p x
    ds (AppndxSec x) = AppndxSec <$> appendSec p x
    ds Bibliography = pure Bibliography

    res (RefProg c x) = pure $ RefProg c x
    intro (IntroProg s1 s2) = pure $ IntroProg s1 s2 
    ipurp (IPurposeProg s) = pure $ IPurposeProg s
    iscope (IScopeProg s) = pure $ IScopeProg s
    ichar (ICharProg s1 s2 s3) = pure $ ICharProg s1 s2 s3
    iorg (IOrgProg s1 c sect s2) = pure $ IOrgProg s1 c sect s2
    stk (StkhldrProg s) = pure $ StkhldrProg s
    client (ClientProg c s) = pure $ ClientProg c s
    cstmr (CstmrProg c) = pure (CstmrProg c)
    gs (GSDProg s) = pure $ GSDProg s
    syscnt (SysCntxtProg c) = pure $ SysCntxtProg c
    uschr (UsrCharsProg c) = pure $ UsrCharsProg c
    syscon (SystConsProg c) = pure $ SystConsProg c
    ss (SSDProg s) = pure $ SSDProg s
    pd (PDProg s) = pure $ PDProg s
    td (TDProg s cs) = pure $ TDProg s cs
    psd (PSDProg nm s lc c) = pure $ PSDProg nm s lc c
    gl (GProg s ci) = pure $ GProg s ci
    sc (SCSProg s) = pure $ SCSProg s
    a (AssumpProg c) = pure $ AssumpProg c
    tm (TMProg s f t) = pure $ TMProg s f t
    gd (GDProg s f g d) = pure $ GDProg s f g d
    dd (DDProg s f dd d) = pure $ DDProg s f dd d
    im (IMProg s f i d) = pure $ IMProg s f i d 
    ct (ConstProg s c) = pure $ ConstProg s c
    csp (CorrSolProg c cs) = pure $ CorrSolProg c cs
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
  mkPlate b = DLPlate (b docSec) (b refSec) (b introSec) (b iPurposeSub) (b iScopeSub)
    (b iCharSub) (b iOrgSub) (b stkSec) (b clientSub) (b cstmrSub)
    (b gsdSec) (b sysCntxtSub) (b usrCharsSub) (b systConsSub) 
    (b ssdSec) (b problemDescription) (b termsAndDefs) (b phySysDesc) (b goals) 
    (b solChSpec) (b assumptions) (b tMs) (b gDs) (b dDs) (b iMs) (b constraints) (b corrSolnPpties)
    (b reqSec) (b reqSub) (b lcsSec) (b ucsSec)
    (b traceSec) (b offShelfSec) (b auxConsSec) (b appendSec)
