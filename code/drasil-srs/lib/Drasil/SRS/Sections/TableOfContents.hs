{-# Language TupleSections #-}
-- | Standard code to make a table of contents.
module Drasil.SRS.Sections.TableOfContents (toToC) where

-- General Drasil
import Language.Drasil
import Language.Drasil.Document
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D

-- Vocabulary
import Drasil.Metadata.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Drasil.Metadata.Documentation (refMat, requirement, tOfCont)
import qualified Drasil.Metadata.Documentation as Doc

-- Other docLang
import qualified Drasil.SRS.Concepts as SRS
import Drasil.SRS.DocumentLanguage.Core

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
mkHeaderItem hdr itm = Nested hdr $ Bullet $ (\x -> (Flat x, Nothing)) <$> itm
mkHeaderItem' :: Sentence -> [ItemType] -> ItemType
mkHeaderItem' hdr itm = Nested hdr $ Bullet $ (, Nothing) <$> itm

-- | Links to the standard SRS section made by the given section constructor.
secRef :: ([Contents] -> [Section] -> Section) -> Sentence -> Sentence
secRef mkSec = namedRef (mkSec [] [])

-- | Helper for creating the 'Table of Contents' section ToC entry
mktToCSec :: ItemType
mktToCSec = Flat $ secRef SRS.tOfCont $ titleize' tOfCont

-- | Helper for creating the 'Reference Material' section ToC entry
mktRefSec :: RefSec -> ItemType
mktRefSec (RefProg _ l) =
  mkHeaderItem (secRef SRS.refMat $ titleize refMat) $ mktSubRef <$> l
  where
    mktSubRef :: RefTab -> Sentence
    mktSubRef TUnits        = secRef SRS.tOfUnit   $ titleize' Doc.tOfUnit
    mktSubRef (TUnits' _ _) = secRef SRS.tOfUnit   $ titleize' Doc.tOfUnit
    mktSubRef (TSymb _)     = secRef SRS.tOfSymb   $ titleize' Doc.tOfSymb
    mktSubRef (TSymb' _ _)  = secRef SRS.tOfSymb   $ titleize' Doc.tOfSymb
    mktSubRef TAandA        = secRef SRS.tOfAbbAcc $ titleize' Doc.abbAcc

-- | Helper for creating the 'Introduction' section ToC entry
mktIntroSec :: IntroSec -> ItemType
mktIntroSec (IntroProg _ _ l) =
  mkHeaderItem (secRef SRS.intro $ titleize Doc.introduction) $ mktSubIntro <$> l
  where
    mktSubIntro :: IntroSub -> Sentence
    mktSubIntro (IPurpose _) = secRef SRS.prpsOfDoc $ titleize Doc.prpsOfDoc
    mktSubIntro (IScope _)   = secRef SRS.scpOfReq  $ titleize' Doc.scpOfReq
    mktSubIntro IChar {}     = secRef SRS.charOfIR  $ titleize' Doc.charOfIR
    mktSubIntro IOrgSec {}   = secRef SRS.orgOfDoc  $ titleize Doc.orgOfDoc

-- | Helper for creating the 'Stakeholders' section ToC entry
mktStkhldrSec:: StkhldrSec -> ItemType
mktStkhldrSec (StkhldrProg l) =
  mkHeaderItem (secRef SRS.stakeholder $ titleize' Doc.stakeholder) $ mktSub <$> l
  where
    mktSub :: StkhldrSub -> Sentence
    mktSub (Client _) = secRef SRS.theCustomer $ D.toSent $ titleizeNP $ the Doc.customer
    mktSub Cstmr      = secRef SRS.theClient   $ D.toSent $ titleizeNP $ the Doc.client

-- | Helper for creating the 'General System Description' section ToC entry
mktGSDSec :: GSDSec -> ItemType
mktGSDSec (GSDProg l) =
  mkHeaderItem (secRef SRS.genSysDes $ titleize Doc.generalSystemDescription) $ mktSub <$> l
  where
    mktSub :: GSDSub -> Sentence
    mktSub (SysCntxt _)   = secRef SRS.sysCont  $ titleize  Doc.sysCont
    mktSub (UsrChars _)   = secRef SRS.userChar $ titleize' Doc.userCharacteristic
    mktSub (SystCons _ _) = secRef SRS.sysCon   $ titleize' Doc.systemConstraint

-- | Helper for creating the 'Specific System Description' section ToC entry
mktSSDSec :: SSDSec -> ItemType
mktSSDSec (SSDProg l) =
  mkHeaderItem' (secRef SRS.specSysDes $ titleize Doc.specificsystemdescription) $ mktSubSSD <$> l
  where
    mktSubSSD :: SSDSub -> ItemType
    mktSubSSD (SSDProblem (PDProg _ _ sl1)) = mkHeaderItem (secRef SRS.probDesc $ titleize Doc.problemDescription) $ mktSubPD <$> sl1
    mktSubSSD (SSDSolChSpec (SCSProg sl2))  = mkHeaderItem (secRef SRS.solCharSpec $ titleize Doc.solutionCharSpec) $ mktSubSCS <$> sl2

    mktSubPD :: PDSub -> Sentence
    mktSubPD (TermsAndDefs _ _) = secRef SRS.termAndDefn $ titleize' Doc.termAndDef
    mktSubPD PhySysDesc {}      = secRef SRS.physSyst $ titleize  Doc.physSyst
    mktSubPD (Goals _ _)        = secRef SRS.goalStmt $ titleize' Doc.goalStmt

    mktSubSCS :: SCSSub -> Sentence
    mktSubSCS (Assumptions _)      = secRef SRS.assumpt    $ titleize' Doc.assumption
    mktSubSCS TMs {}               = secRef SRS.thModel    $ titleize' thModel
    mktSubSCS GDs {}               = secRef SRS.genDefn    $ titleize' genDefn
    mktSubSCS DDs {}               = secRef SRS.dataDefn   $ titleize' dataDefn
    mktSubSCS IMs {}               = secRef SRS.inModel    $ titleize' inModel
    mktSubSCS (Constraints _ _)    = secRef SRS.datCon     $ titleize' Doc.datumConstraint
    mktSubSCS (CorrSolnPpties _ _) = secRef SRS.propCorSol $ titleize' Doc.propOfCorSol

-- | Helper for creating the 'Requirements' section ToC entry
mktReqrmntSec :: ReqrmntSec -> ItemType
mktReqrmntSec (ReqsProg l) =
  mkHeaderItem (secRef SRS.require $ titleize' requirement) $ mktSubs <$> l
  where
    mktSubs :: ReqsSub -> Sentence
    mktSubs (FReqsSub _ _)  = secRef SRS.funcReq    $ titleize' Doc.functionalRequirement
    mktSubs (NonFReqsSub _) = secRef SRS.nonfuncReq $ titleize' Doc.nonfunctionalRequirement

-- | Helper for creating the 'Likely Changes' section ToC entry
mktLCsSec :: LCsSec -> ItemType
mktLCsSec (LCsProg _) = Flat $ secRef SRS.likeChg $ titleize' Doc.likelyChg

-- | Helper for creating the 'Unlikely Changes' section ToC entry
mktUCsSec :: UCsSec -> ItemType
mktUCsSec (UCsProg _) = Flat $ secRef SRS.unlikeChg $ titleize' Doc.unlikelyChg

-- | Helper for creating the 'Traceability Matrices and Graphs' section ToC entry
mktTraceabilitySec :: TraceabilitySec -> ItemType
mktTraceabilitySec (TraceabilityProg _) = Flat $ secRef SRS.traceyMandG $ titleize' Doc.traceyMandG

-- | Helper for creating the 'Values of Auxiliary Constants' section ToC entry
mktAuxConsSec :: AuxConstntSec -> ItemType
mktAuxConsSec (AuxConsProg _) = Flat $ secRef SRS.valsOfAuxCons $ titleize  Doc.consVals

-- | Helper for creating the 'References' section ToC entry
mktBib :: ItemType
mktBib = Flat $ secRef SRS.reference $ titleize' Doc.reference

-- | Helper for creating the 'Appendix' section ToC entry
mktAppndxSec :: AppndxSec -> ItemType
mktAppndxSec (AppndxProg _) = Flat $ secRef SRS.appendix $ titleize  Doc.appendix

-- | Helper for creating the 'Off-The-Shelf Solutions' section ToC entry
mktOffShelfSolnSec :: OffShelfSolnsSec -> ItemType
mktOffShelfSolnSec (OffShelfSolnsProg _) = Flat $ secRef SRS.offShelfSol $ titleize' Doc.offShelfSolution
