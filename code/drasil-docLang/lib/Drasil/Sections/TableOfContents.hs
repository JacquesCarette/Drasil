{-# Language TupleSections #-}
-- | Standard code to make a table of contents.
module Drasil.Sections.TableOfContents (toToC, findToC) where

import Language.Drasil
import Drasil.DocumentLanguage.Core
import Language.Drasil.Chunk.Concept.NamedCombinators
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
toToC :: SRSSection -> ItemType
toToC TableContents = mktToCSec
toToC (Ref' rs)     = mktRefSec rs
toToC (Intro i)     = mktIntroSec i
toToC (Stkhldr sts) = mktStkhldrSec sts
toToC (GSD gs')     = mktGSDSec gs'
toToC (SSD ss)      = mktSSDSec ss
toToC (Reqrmnt r)   = mktReqrmntSec r
toToC LCSec         = mktLCsSec 
toToC UCSec         = mktUCsSec 
toToC Traceability  = mktTraceabilitySec 
toToC AuxConstnt    = mktAuxConsSec 
toToC Bibliography' = mktBib
toToC Appndx        = mktAppndxSec 
toToC OffShelfSolns = mktOffShelfSolnSec

mkHeaderItem :: Sentence -> [Sentence] -> ItemType
mkHeaderItem hdr itm = Nested hdr $ Bullet $ map (\x -> (Flat x, Nothing)) itm
mkHeaderItem' :: Sentence -> [ItemType] -> ItemType
mkHeaderItem' hdr itm = Nested hdr $ Bullet $ map (, Nothing) itm

-- | Helper for creating the 'Table of Contents' section ToC entry
mktToCSec :: ItemType
mktToCSec = Flat $ namedRef SRS.tOfContLabel $ titleize' Doc.tOfCont

-- | Helper for creating the 'Reference Material' section ToC entry
mktRefSec :: [RefTab] -> ItemType
mktRefSec l =
  mkHeaderItem (namedRef SRS.refMatLabel $ titleize Doc.refMat) $ map mktSubRef l
  where
    mktSubRef :: RefTab -> Sentence
    mktSubRef TU  = namedRef SRS.tOfUnitLabel   $ titleize' Doc.tOfUnit
    mktSubRef TS  = namedRef SRS.tOfSymbLabel   $ titleize' Doc.tOfSymb
    mktSubRef TAA = namedRef SRS.tOfAbbAccLabel $ titleize' Doc.abbAcc

-- | Helper for creating the 'Introduction' section ToC entry
mktIntroSec ::  [IntroSub] -> ItemType
mktIntroSec l =
  mkHeaderItem (namedRef SRS.introLabel $ titleize Doc.introduction) $ map mktIntroSub l
  where
    mktIntroSub :: IntroSub -> Sentence
    mktIntroSub IPurpose = namedRef SRS.docPurposeLabel $ titleize Doc.prpsOfDoc
    mktIntroSub IScope   = namedRef SRS.reqsScopeLabel $ titleize' Doc.scpOfReq
    mktIntroSub IChar    = namedRef SRS.readerCharsLabel $ titleize' Doc.charOfIR
    mktIntroSub IOrg     = namedRef SRS.docOrgLabel $ titleize Doc.orgOfDoc

-- | Helper for creating the 'Stakeholders' section ToC entry
mktStkhldrSec ::  [StkhldrSub] -> ItemType
mktStkhldrSec l =
  mkHeaderItem (namedRef SRS.stakeholderLabel $ titleize' Doc.stakeholder) $ map mktStkhldrSub l
  where
    mktStkhldrSub :: StkhldrSub -> Sentence
    mktStkhldrSub Client = namedRef SRS.customerLabel $ titleizeNP $ the Doc.customer
    mktStkhldrSub Cstmr  = namedRef SRS.clientLabel $ titleizeNP $ the Doc.client

-- | Helper for creating the 'General System Description' section ToC entry
mktGSDSec ::  [GSDSub] -> ItemType
mktGSDSec l =
  mkHeaderItem (namedRef SRS.genSysDescLabel $ titleize Doc.generalSystemDescription) $ map mktGSDSub l
  where
    mktGSDSub :: GSDSub -> Sentence
    mktGSDSub SysCntxt' = namedRef SRS.sysContextLabel $ titleize  Doc.sysCont
    mktGSDSub UsrChars' = namedRef SRS.userCharsLabel $ titleize' Doc.userCharacteristic
    mktGSDSub SystCons' =  namedRef SRS.sysConstraintsLabel $ titleize' Doc.systemConstraint

-- | Helper for creating the 'Specific System Description' section ToC entry
mktSSDSec ::  [SSDSub] -> ItemType
mktSSDSec l =
  mkHeaderItem' (namedRef SRS.specSystDescLabel $ titleize Doc.specificsystemdescription) $ map mktSubSSD l
  where
    mktSubSSD :: SSDSub -> ItemType
    mktSubSSD (ProblemDescription' sl1) = mkHeaderItem (namedRef SRS.probDescLabel $ titleize Doc.problemDescription) $ map mktSubPD sl1
    mktSubSSD (SolChSpec' sl2) = mkHeaderItem (namedRef SRS.solCharSpecLabel $ titleize Doc.solutionCharSpec) $ map mktSubSCS sl2

    mktSubPD :: PDSub -> Sentence
    mktSubPD TermsAndDefs' = namedRef SRS.termDefsLabel $ titleize' Doc.termAndDef
    mktSubPD PhySysDesc'   = namedRef SRS.physSystLabel $ titleize  Doc.physSyst
    mktSubPD Goals'        = namedRef SRS.goalStmtLabel $ titleize' Doc.goalStmt

    mktSubSCS :: SCSSub -> Sentence
    mktSubSCS Assumpt   = namedRef SRS.assumptLabel     $ titleize' Doc.assumption
    mktSubSCS TM        = namedRef SRS.thModelLabel     $ titleize' Doc.thModel
    mktSubSCS GD        = namedRef SRS.genDefnLabel     $ titleize' Doc.genDefn
    mktSubSCS DD        = namedRef SRS.dataDefnLabel    $ titleize' Doc.dataDefn
    mktSubSCS IM        = namedRef SRS.inModelLabel     $ titleize' Doc.inModel
    mktSubSCS Consts    = namedRef SRS.datConLabel      $ titleize' Doc.datumConstraint
    mktSubSCS CorSolPpt = namedRef SRS.corSolPropsLabel $ titleize' Doc.propOfCorSol

-- | Helper for creating the 'Requirements' section ToC entry
mktReqrmntSec ::  [ReqSub] -> ItemType
mktReqrmntSec l =
  mkHeaderItem (namedRef SRS.requirementsLabel $ titleize' Doc.requirement) $ map mktSubs l
  where
    mktSubs :: ReqSub -> Sentence
    mktSubs FReqs    = namedRef SRS.funcReqLabel    $ titleize' Doc.functionalRequirement
    mktSubs NonFReqs = namedRef SRS.nonfuncReqLabel $ titleize' Doc.nonfunctionalRequirement

-- | Helper for creating the 'Likely Changes' section ToC entry
mktLCsSec :: ItemType
mktLCsSec = Flat $ namedRef SRS.likeChgLabel $ titleize' Doc.likelyChg

-- | Helper for creating the 'Unlikely Changes' section ToC entry
mktUCsSec :: ItemType
mktUCsSec = Flat $ namedRef SRS.unlikeChgLabel $ titleize' Doc.unlikelyChg

-- | Helper for creating the 'Traceability Matrices and Graphs' section ToC entry
mktTraceabilitySec :: ItemType
mktTraceabilitySec = Flat $ namedRef SRS.traceMatricesLabel $ titleize' Doc.traceyMandG

-- | Helper for creating the 'Values of Auxiliary Constants' section ToC entry
mktAuxConsSec :: ItemType
mktAuxConsSec = Flat $ namedRef SRS.valsOfAuxConsLabel $ titleize  Doc.consVals

-- | Helper for creating the 'References' section ToC entry
mktBib :: ItemType
mktBib = Flat $ namedRef SRS.referenceLabel $ titleize' Doc.reference 

-- | Helper for creating the 'Appendix' section ToC entry
mktAppndxSec :: ItemType
mktAppndxSec = Flat $ namedRef SRS.appendixLabel $ titleize  Doc.appendix

-- | Helper for creating the 'Off-The-Shelf Solutions' section ToC entry
mktOffShelfSolnSec :: ItemType
mktOffShelfSolnSec = Flat $ namedRef SRS.offShelfSolnsLabel $ titleize' Doc.offShelfSolution

{-
drawTree' :: Tree D.SRSSection -> Contents
drawTree' st = UlC $ ulcc $ Enumeration $ Bullet $ map (, Nothing) (draw st)

--drawTree' :: Tree D.SRSSection -> Contents
--drawTree' = foldlSP_ .draw

draw :: Tree D.SRSSection -> [ItemType]
draw [] = []
draw (Node t []) = Flat $ toSent t
draw (Node t ts) = Nested (toSent t) $ Bullet $ map (, Nothing) draw ts

  --draw (Node x ts0) = Flat (toSent x) : drawSubTrees ts0
    --drawSubTrees [] = []
    --drawSubTrees [t] = draw t
    --drawSubTrees (t:ts) = draw t ++ drawSubTrees ts

--traverseDF :: Tree D.SRSSection -> [String]
--traverseDF Empty        = []
--traverseDF (Node a [c]) = a : traverseDF c

toSent :: D.SRSSection -> Sentence
toSent D.TableOfContents    = mktToCSec
toSent D.RefSec             = mktRefSec 
toSent D.TUnits             = mktTUnits 
toSent D.TSymb              = mktTSymb 
toSent D.TAandA             = mktTAandA 
toSent D.IntroSec           = mktIntroSec 
toSent D.IPurposeSub        = mktIPurposeSub 
toSent D.IScopeSub          = mktIScopeSub 
toSent D.ICharSub           = mktICharSub 
toSent D.IOrgSub            = mktIOrgSub
toSent D.StkhldrSec         = mktStkhldrSec 
toSent D.ClientSub          = mktClientSub 
toSent D.CstmrSub           = mktCstmrSub 
toSent D.GSDSec             = mktGSDSec 
toSent D.SysCntxt           = mktSysCntxt 
toSent D.UsrChars           = mktUsrChars
toSent D.SystCons           = mktSystCons 
toSent D.SSDSec             = mktSSDSec 
toSent D.ProblemDescription = mktSSDProblem 
toSent D.TermsAndDefs       = mktTermsAndDefs
toSent D.PhySysDesc         = mktPhySysDesc
toSent D.Goals              = mktGoals 
toSent D.SolChSpec          = mktSSDSolChSpec 
toSent D.Assumptions        = mktAssumptions
toSent D.TMs                = mktTMs
toSent D.GDs                = mktGDs
toSent D.DDs                = mktDDs
toSent D.IMs                = mktIMs
toSent D.Constraints        = mktConstraints 
toSent D.CorrSolnPpties     = mktCorrSolnPpties 
toSent D.ReqrmntSec         = mktReqrmntSec 
toSent D.FReqsSub           = mktFReqsSub
toSent D.NonFReqsSub        = mktNonFReqsSub
toSent D.LCsSec             = mktLCsSec 
toSent D.UCsSec             = mktUCsSec 
toSent D.TraceabilitySec    = mktTraceabilitySec 
toSent D.AuxConstntSec      = mktAuxConsSec 
toSent D.Bibliography       = mktBib
toSent D.AppndxSec          = mktAppndxSec 
toSent D.OffShelfSolnsSec   = mktOffShelfSolnSec 

-- | Helper for creating the 'Table of Contents' section ToC entry
mktToCSec :: Sentence
mktToCSec = namedRef SRS.tOfContLabel $ titleize' Doc.tOfCont
--mktToCSec = Flat $ namedRef SRS.tOfContLabel $ titleize' Doc.tOfCont

-- | Helper for creating the 'Reference Material' section ToC entry
mktRefSec :: Sentence
mktRefSec = namedRef SRS.refMatLabel $ titleize Doc.refMat

mktTUnits :: Sentence
mktTUnits = namedRef SRS.tOfUnitLabel $ titleize' Doc.tOfUnit

mktTSymb :: Sentence
mktTSymb = namedRef SRS.tOfSymbLabel $ titleize' Doc.tOfSymb

mktTAandA :: Sentence
mktTAandA = namedRef SRS.tOfAbbAccLabel $ titleize' Doc.abbAcc

-- | Helper for creating the 'Introduction' section ToC entry
mktIntroSec :: Sentence
mktIntroSec = namedRef SRS.introLabel $ titleize Doc.introduction

mktIPurposeSub :: Sentence  
mktIPurposeSub = namedRef SRS.docPurposeLabel $ titleize Doc.prpsOfDoc

mktIScopeSub :: Sentence  
mktIScopeSub = namedRef SRS.reqsScopeLabel $ titleize' Doc.scpOfReq

mktICharSub :: Sentence  
mktICharSub = namedRef SRS.readerCharsLabel $ titleize' Doc.charOfIR

mktIOrgSub :: Sentence  
mktIOrgSub = namedRef SRS.docOrgLabel $ titleize Doc.orgOfDoc

-- | Helper for creating the 'Stakeholders' section ToC entry
mktStkhldrSec:: Sentence
mktStkhldrSec = namedRef SRS.stakeholderLabel $ titleize' Doc.stakeholder

mktClientSub :: Sentence
mktClientSub = namedRef SRS.customerLabel $ titleizeNP $ the Doc.customer

mktCstmrSub :: Sentence
mktCstmrSub = namedRef SRS.clientLabel $ titleizeNP $ the Doc.client

-- | Helper for creating the 'General System Description' section ToC entry
mktGSDSec ::  Sentence
mktGSDSec = namedRef SRS.genSysDescLabel $ titleize Doc.generalSystemDescription

mktSysCntxt :: Sentence
mktSysCntxt = namedRef SRS.sysContextLabel $ titleize  Doc.sysCont

mktUsrChars :: Sentence
mktUsrChars = namedRef SRS.userCharsLabel $ titleize' Doc.userCharacteristic

mktSystCons :: Sentence
mktSystCons = namedRef SRS.sysConstraintsLabel $ titleize' Doc.systemConstraint

-- | Helper for creating the 'Specific System Description' section ToC entry
mktSSDSec :: Sentence
mktSSDSec = namedRef SRS.specSystDescLabel $ titleize Doc.specificsystemdescription

mktSSDProblem :: Sentence
mktSSDProblem = namedRef SRS.probDescLabel $ titleize Doc.problemDescription

mktSSDSolChSpec :: Sentence
mktSSDSolChSpec = namedRef SRS.solCharSpecLabel $ titleize Doc.solutionCharSpec

mktTermsAndDefs :: Sentence
mktTermsAndDefs = namedRef SRS.termDefsLabel $ titleize' Doc.termAndDef

mktPhySysDesc :: Sentence
mktPhySysDesc = namedRef SRS.physSystLabel $ titleize  Doc.physSyst

mktGoals :: Sentence
mktGoals = namedRef SRS.goalStmtLabel $ titleize' Doc.goalStmt

mktAssumptions :: Sentence
mktAssumptions = namedRef SRS.assumptLabel $ titleize' Doc.assumption

mktTMs :: Sentence
mktTMs = namedRef SRS.thModelLabel $ titleize' Doc.thModel

mktGDs :: Sentence
mktGDs = namedRef SRS.genDefnLabel $ titleize' Doc.genDefn

mktDDs :: Sentence
mktDDs = namedRef SRS.dataDefnLabel $ titleize' Doc.dataDefn

mktIMs :: Sentence
mktIMs = namedRef SRS.inModelLabel $ titleize' Doc.inModel

mktConstraints :: Sentence
mktConstraints = namedRef SRS.datConLabel $ titleize' Doc.datumConstraint

mktCorrSolnPpties :: Sentence
mktCorrSolnPpties = namedRef SRS.corSolPropsLabel $ titleize' Doc.propOfCorSol

-- | Helper for creating the 'Requirements' section ToC entry
mktReqrmntSec :: Sentence
mktReqrmntSec = namedRef SRS.requirementsLabel $ titleize' Doc.requirement

mktFReqsSub :: Sentence
mktFReqsSub = namedRef SRS.funcReqLabel $ titleize' Doc.functionalRequirement

mktNonFReqsSub :: Sentence
mktNonFReqsSub = namedRef SRS.nonfuncReqLabel $ titleize' Doc.nonfunctionalRequirement

-- | Helper for creating the 'Likely Changes' section ToC entry
mktLCsSec :: Sentence
mktLCsSec = namedRef SRS.likeChgLabel $ titleize' Doc.likelyChg

-- | Helper for creating the 'Unlikely Changes' section ToC entry
mktUCsSec :: Sentence
mktUCsSec = namedRef SRS.unlikeChgLabel $ titleize' Doc.unlikelyChg

-- | Helper for creating the 'Traceability Matrices and Graphs' section ToC entry
mktTraceabilitySec :: Sentence
mktTraceabilitySec =  namedRef SRS.traceMatricesLabel $ titleize' Doc.traceyMandG

-- | Helper for creating the 'Values of Auxiliary Constants' section ToC entry
mktAuxConsSec :: Sentence
mktAuxConsSec = namedRef SRS.valsOfAuxConsLabel $ titleize  Doc.consVals

-- | Helper for creating the 'References' section ToC entry
mktBib :: Sentence
mktBib = namedRef SRS.referenceLabel $ titleize' Doc.reference 

-- | Helper for creating the 'Appendix' section ToC entry
mktAppndxSec :: Sentence
mktAppndxSec = namedRef SRS.appendixLabel $ titleize  Doc.appendix

-- | Helper for creating the 'Off-The-Shelf Solutions' section ToC entry
mktOffShelfSolnSec :: Sentence
mktOffShelfSolnSec = namedRef SRS.offShelfSolnsLabel $ titleize' Doc.offShelfSolution
-}

-- Find more concise way to do this
-- | Finds whether the Table of Contents is in a SRSDecl.
findToC :: [DocSection] -> ShowTableOfContents
findToC [] = NoToC
findToC (TableOfContents _:_) = ToC
findToC (_:dds) = findToC dds
