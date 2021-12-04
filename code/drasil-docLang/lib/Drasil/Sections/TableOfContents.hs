{-# Language TupleSections #-}
-- | Standard code to make a table of contents.
module Drasil.Sections.TableOfContents (toToC, findToC) where

import Language.Drasil
import Drasil.DocumentLanguage.Core
import Utils.Drasil.Concepts
import qualified Drasil.DocLang.SRS as SRS
import qualified Data.Drasil.Concepts.Documentation as Doc
import qualified Data.Drasil.TheoryConcepts as Doc (dataDefn, genDefn, inModel, thModel)

{- Layout for Table of Contents in SRS documents:
Table of Contents
Reference Material
  Table of Units
  Table of Symbols
  Table of Abbreviations and Acronyms
Instroduction
  Purpose of Document
  Scope of Requirements
  Characteristics of Intended Reader
  Organization of Document
Stakeholders
  The Client
  The Customer
General System Description
  System Context
  User Characteristics
  System Constraints
Specific System Description
  Problem Description
    Termonology and definitions
    Physical System Description
    Goal Statements
  Solution Characteristics Specification
    Assumptions
    Theoretical Models
    General Definitions
    Data Definitions
    Instance Models
    Data Constraints
    Properties of a Correct Solution
Requirements
  Functional Requirements
  Non-Functional Requirements
Likely Changes
Unlikely Changes
Traceability Matrices and Graphs
Values of Auxiliary Constants
References
Appendix

Other refs not used but still defined:
product use case
scope of the project
use case table
off the shelf solutions
-}

-- TODO: Use DLPlate for this.
-- | Finds all possible sections and subsections to make a Table of Contents.
toToC :: DocSection -> ItemType
toToC TableOfContents      = mktToCSec
toToC (RefSec rs)          = mktRefSec rs
toToC (IntroSec i)         = mktIntroSec i
toToC (StkhldrSec sts)     = mktStkhldrSec sts
toToC (GSDSec gs')         = mktGSDSec gs'
toToC (SSDSec ss)          = mktSSDSec ss
toToC (ReqrmntSec r)       = mktReqrmntSec r
toToC (LCsSec lc)          = mktLCsSec lc
toToC (UCsSec ulcs)        = mktUCsSec ulcs
toToC (TraceabilitySec t)  = mktTraceabilitySec t
toToC (AuxConstntSec acs)  = mktAuxConsSec acs
toToC Bibliography         = mktBib
toToC (AppndxSec a)        = mktAppndxSec a
toToC (OffShelfSolnsSec o) = mktOffShelfSolnSec o

mkHeaderItem :: Sentence -> [Sentence] -> ItemType
mkHeaderItem hdr itm = Nested hdr $ Bullet $ map (\x -> (Flat x, Nothing)) itm
mkHeaderItem' :: Sentence -> [ItemType] -> ItemType
mkHeaderItem' hdr itm = Nested hdr $ Bullet $ map (, Nothing) itm

-- | Helper for creating the 'Table of Contents' section ToC entry
mktToCSec :: ItemType
mktToCSec = Flat $ namedRef SRS.tOfContLabel $ titleize' Doc.tOfCont

-- | Helper for creating the 'Reference Material' section ToC entry
mktRefSec :: RefSec -> ItemType
mktRefSec (RefProg _) =
  mkHeaderItem (namedRef SRS.refMatLabel $ titleize Doc.refMat) []

mktTUnits :: TUnits -> Sentence
mktTUnits TUProg = namedRef SRS.tOfUnitLabel $ titleize' Doc.tOfUnit

mktTUnits' :: TUnits' -> Sentence
mktTUnits' (TUProg' _ _) = namedRef SRS.tOfUnitLabel $ titleize' Doc.tOfUnit

mktTSymb :: TSymb -> Sentence
mktTSymb (TSProg _) = namedRef SRS.tOfSymbLabel $ titleize' Doc.tOfSymb

mktTSymb' :: TSymb' -> Sentence
mktTSymb' (TSProg' _ _) = namedRef SRS.tOfSymbLabel $ titleize' Doc.tOfSymb

mktTAandA :: TAandA -> Sentence
mktTAandA TAAProg = namedRef SRS.tOfAbbAccLabel $ titleize' Doc.abbAcc

-- | Helper for creating the 'Introduction' section ToC entry
mktIntroSec :: IntroSec -> ItemType
mktIntroSec (IntroProg _ _) =
  mkHeaderItem (namedRef SRS.introLabel $ titleize Doc.introduction) []

mktIPurpSub :: IPurposeSub -> Sentence  
mktIPurpSub (IPurposeProg _) = namedRef SRS.docPurposeLabel $ titleize Doc.prpsOfDoc

mktIScopeSub :: IScopeSub -> Sentence  
mktIScopeSub (IScopeProg _) = namedRef SRS.reqsScopeLabel $ titleize' Doc.scpOfReq

mktICharSub :: ICharSub -> Sentence  
mktICharSub ICharProg {} = namedRef SRS.readerCharsLabel $ titleize' Doc.charOfIR

mktIOrgSub :: IOrgSub -> Sentence  
mktIOrgSub IOrgProg {} = namedRef SRS.docOrgLabel $ titleize Doc.orgOfDoc

-- | Helper for creating the 'Stakeholders' section ToC entry
mktStkhldrSec:: StkhldrSec -> ItemType
mktStkhldrSec (StkhldrProg _) =
  mkHeaderItem (namedRef SRS.stakeholderLabel $ titleize' Doc.stakeholder) []

mktClientSub :: ClientSub -> Sentence
mktClientSub (ClientProg _ _) = namedRef SRS.customerLabel $ titleizeNP $ the Doc.customer

mktCstmrSub :: CstmrSub -> Sentence
mktCstmrSub (CstmrProg _) = namedRef SRS.clientLabel $ titleizeNP $ the Doc.client

-- | Helper for creating the 'General System Description' section ToC entry
mktGSDSec :: GSDSec -> ItemType
mktGSDSec (GSDProg l) =
  mkHeaderItem (namedRef SRS.genSysDescLabel $ titleize Doc.generalSystemDescription) []

mktSysCntxt :: SysCntxt -> Sentence
mktSysCntxt (SysCntxtProg _) = namedRef SRS.sysContextLabel $ titleize  Doc.sysCont

mktUsrChars :: UsrChars -> Sentence
mktUsrChars (UsrCharsProg _) = namedRef SRS.userCharsLabel $ titleize' Doc.userCharacteristic

mktSystCons :: SystCons -> Sentence
mktSystCons (SystConsProg _) = namedRef SRS.sysConstraintsLabel $ titleize' Doc.systemConstraint

-- | Helper for creating the 'Specific System Description' section ToC entry
mktSSDSec :: SSDSec -> ItemType
mktSSDSec (SSDProg _) =
  mkHeaderItem' (namedRef SRS.specSystDescLabel $ titleize Doc.specificsystemdescription) []

mktSSDProblem :: ProblemDescription -> ItemType
mktSSDProblem PDProg {} = mkHeaderItem (namedRef SRS.probDescLabel $ titleize Doc.problemDescription) []

mktSSDSolChSpec :: SolChSpec -> ItemType
mktSSDSolChSpec SCSProg {} = mkHeaderItem (namedRef SRS.solCharSpecLabel $ titleize Doc.solutionCharSpec) []

mktTermsAndDefs :: TermsAndDefs -> Sentence
mktTermsAndDefs TDProg {} = namedRef SRS.termDefsLabel $ titleize' Doc.termAndDef

mktPhySysDesc :: PhySysDesc -> Sentence
mktPhySysDesc PSDProg {} = namedRef SRS.physSystLabel $ titleize  Doc.physSyst

mktGoals :: Goals -> Sentence
mktGoals GProg {} = namedRef SRS.goalStmtLabel $ titleize' Doc.goalStmt

mktAssumptions :: Assumptions -> Sentence
mktAssumptions AssumpProg {} = namedRef SRS.assumptLabel $ titleize' Doc.assumption

mktTMs :: TMs -> Sentence
mktTMs TMProg {} = namedRef SRS.thModelLabel $ titleize' Doc.thModel

mktGDs :: GDs -> Sentence
mktGDs GDProg {} = namedRef SRS.genDefnLabel $ titleize' Doc.genDefn

mktDDs :: DDs -> Sentence
mktDDs DDProg {} = namedRef SRS.dataDefnLabel $ titleize' Doc.dataDefn

mktIMs :: IMs -> Sentence
mktIMs IMProg {} = namedRef SRS.inModelLabel $ titleize' Doc.inModel

mktConstraints :: Constraints -> Sentence
mktConstraints ConstProg {} = namedRef SRS.datConLabel $ titleize' Doc.datumConstraint

mktCorrSolnPpties :: CorrSolnPpties -> Sentence
mktCorrSolnPpties CorrSolProg {} = namedRef SRS.corSolPropsLabel $ titleize' Doc.propOfCorSol

-- | Helper for creating the 'Requirements' section ToC entry
mktReqrmntSec :: ReqrmntSec -> ItemType
mktReqrmntSec (ReqsProg _) =
  mkHeaderItem (namedRef SRS.requirementsLabel $ titleize' Doc.requirement) []

mktFReqsSub' :: FReqsSub' -> Sentence
mktFReqsSub' FReqsProg'{} = namedRef SRS.funcReqLabel $ titleize' Doc.functionalRequirement

mktFReqsSub :: FReqsSub -> Sentence
mktFReqsSub FReqsProg {}  = namedRef SRS.funcReqLabel $ titleize' Doc.functionalRequirement

mktNonFReqsSub :: NonFReqsSub -> Sentence
mktNonFReqsSub NonFReqsProg {} = namedRef SRS.nonfuncReqLabel $ titleize' Doc.nonfunctionalRequirement

-- | Helper for creating the 'Likely Changes' section ToC entry
mktLCsSec :: LCsSec -> ItemType
mktLCsSec (LCsProg _) = Flat $ namedRef SRS.likeChgLabel $ titleize' Doc.likelyChg

-- | Helper for creating the 'Unlikely Changes' section ToC entry
mktUCsSec :: UCsSec -> ItemType
mktUCsSec (UCsProg _) = Flat $ namedRef SRS.unlikeChgLabel $ titleize' Doc.unlikelyChg

-- | Helper for creating the 'Traceability Matrices and Graphs' section ToC entry
mktTraceabilitySec :: TraceabilitySec -> ItemType
mktTraceabilitySec (TraceabilityProg _) = Flat $ namedRef SRS.traceMatricesLabel $ titleize' Doc.traceyMandG

-- | Helper for creating the 'Values of Auxiliary Constants' section ToC entry
mktAuxConsSec :: AuxConstntSec -> ItemType
mktAuxConsSec (AuxConsProg _ _) = Flat $ namedRef SRS.valsOfAuxConsLabel $ titleize  Doc.consVals

-- | Helper for creating the 'References' section ToC entry
mktBib :: ItemType
mktBib = Flat $ namedRef SRS.referenceLabel $ titleize' Doc.reference 

-- | Helper for creating the 'Appendix' section ToC entry
mktAppndxSec :: AppndxSec -> ItemType
mktAppndxSec (AppndxProg _) = Flat $ namedRef SRS.appendixLabel $ titleize  Doc.appendix

-- | Helper for creating the 'Off-The-Shelf Solutions' section ToC entry
mktOffShelfSolnSec :: OffShelfSolnsSec -> ItemType
mktOffShelfSolnSec (OffShelfSolnsProg _) = Flat $ namedRef SRS.offShelfSolnsLabel $ titleize' Doc.offShelfSolution

-- Find more concise way to do this
-- | Finds whether the Table of Contents is in a SRSDecl.
findToC :: [DocSection] -> ShowTableOfContents
findToC [] = NoToC
findToC (TableOfContents:_) = ToC
findToC (_:dds) = findToC dds
