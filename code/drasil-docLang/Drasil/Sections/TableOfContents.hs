-- Standard code to make a table of contents.
module Drasil.Sections.TableOfContents (toToC) where
  {-(contToCRef, contRefMatRef, contToURef,
  contToSRef, contToAARef, contIntroRef, contPoDRef, contSoRRef, contCIRRef,
  contOoDRef, contStkhldrRef, contClntRef, contCstmrRef, contGSDRef,
  contSysCtxtRef, contUsrChrRef, contSysConRef, contSSDRef, contPrbDescRef,
  contSCSRef, contTaDRef, contPSDRef, contGlStRef, contAsmpRef, contTMRef,
  contGDRef, contDDRef, contIMRef, contDtCnstrRef, contCSPRef, contRqmtRef,
  contFReqsRef, contNFReqsRef, contLCsRef, contUCsRef, contTMaGRef, contVACRef,
  contBibRef, contApndxRef, contOtSSRef)-} 

import Language.Drasil
import Drasil.DocumentLanguage.Core
import qualified Drasil.DocLang.SRS as SRS
import qualified Drasil.Data.Concepts.Documentation as Doc
{- Which parts of the SRS do we have sections for?
Table of Contents ^
Reference Material ----
  Table of Units ^
  Table of Symbols ^
  Table of Abbreviations and Acronyms ----
Instroduction ^
  Purpose of Document ^
  Scope of Requirements ^
  Characteristics of Intended Reader ^
  Organization of Document ^
Stakeholders ^
  The Client ^
  The Customer ^
General System Description ^
  System Context ^
  User Characteristics ^
  System Constraints ^
Specific System Description ^
  Problem Description ^
    -- MISSING: Termonology and definitions ^
    Physical System Description ^
    Goal Statements ^
  Solution Characteristics Specification ^
    Assumptions ^
    Theoretical Models ^
    General Definitions ^
    Data Definitions ^
    Instance Models ^
    Data Constraints ^
    Properties of a Correct Solution ^
Requirements ^
  Functional Requirements ^
  Non-Functional Requirements ^
Likely Changes ^
Unlikely Changes ^
Traceability Matrices and Graphs ^
Values of Auxiliary Constants ^
References ^
Appendix ^

Other refs not used but still defined:
product use case
scope of the project
use case table
off the shelf solutions
-}
-- Finds all possible sections and subsections. Then places them in a list format.
-- remove list from type?
toToC :: DocSection -> [ItemType]
toToC TableOfContents      = [mktToCSec]
toToC (RefSec rs)          = [mktRefSec rs]
toToC (IntroSec is)        = [mktIntroSec is]
toToC (StkhldrSec sts)     = [mktStkhldrSec sts]
toToC (GSDSec gs')         = [mktGSDSec gs']
toToC (SSDSec ss)          = [mktSSDSec ss]
toToC (ReqrmntSec r)       = [mktReqrmntSec r]
toToC (LCsSec lc)          = [mktLCsSec lc]
toToC (UCsSec ulcs)        = [mktUCsSec ulcs]
toToC (TraceabilitySec t)  = [mktTraceabilitySec t]
toToC (AuxConstntSec acs)  = [mktAuxConsSec acs]
toToC Bibliography         = [mktBib]
toToC (AppndxSec a)        = [mktAppndxSec a]
toToC (OffShelfSolnsSec o) = [mktOffShelfSolnSec o]

mkHeaderItem :: Sentence -> [Sentence] -> ItemType
mkHeaderItem hdr itm = Nested hdr $ Bullet $ map (\x -> (Flat x, Nothing)) itm

-- | Helper for creating the 'Table of Contents' section ToC entry
mktToCSec :: ItemType
mktToCSec = Flat $ namedRef SRS.tOfContLabel $ titleize' Doc.tOfCont

-- | Helper for creating the 'Reference Material' section ToC entry
mktRefSec :: RefSec -> ItemType
mktRefSec (RefProg _ l) =
  mkHeaderItem (namedRef SRS.refMatLabel $ titleize Doc.refmat) $ map mktSubRef l
  where
    mktSubRef :: RefTab -> Sentence
    mktSubRef TUnits        = namedRef SRS.tOfUnitLabel   $ titleize' Doc.tOfUnit
    mktSubRef (TUnits' _ _) = namedRef SRS.tOfUnitLabel   $ titleize' Doc.tOfUnit
    mktSubRef (TSymb _)     = namedRef SRS.tOfSymbLabel   $ titleize' Doc.tOfSymb
    mktSubRef (TSymb' _ _)  = namedRef SRS.tOfSymbLabel   $ titleize' Doc.tOfSymb
    mktSubRef TAandA        = namedRef SRS.tOfAbbAccLabel $ titleize' Doc.abbAcc

-- | Helper for creating the 'Introduction' section ToC entry
mktIntroSec :: IntroSec -> ItemType
mktIntroSec (IntroProg _ _ l) =
  mkHeaderItem (namedRef SRS.introLabel $ titleize Doc.introduction) $ map mktSubIntro l
  where
    mktSubIntro :: IntroSub -> Sentence
    mktSubIntro (IPurpose _) = namedRef SRS.docPurposeLabel  $ titleize Doc.prpsOfDoc
    mktSubIntro (IScope _)   = namedRef SRS.reqsScopeLabel   $ titleize' Doc.scpOfReq
    mktSubIntro IChar {}     = namedRef SRS.readerCharsLabel $ titleize' Doc.charOfIR
    mktSubIntro IOrgSec {}   = namedRef SRS.docOrgLabel      $ titleize Doc.orgOfDoc

-- | Helper for creating the 'Stakeholders' section ToC entry
mktStkhldrSec:: StkhldrSec -> ItemType
mktStkhldrSec (StkhldrProg l) =
  mkHeaderItem (namedRef SRS.stakeholderLabel $ titleize' Doc.stakeholder) $ map mktSub l
  where
    mktSub :: StkhldrSub -> Sentence
    mktSub (Client _ _) = namedRef SRS.customerLabel $ titleizeNP $ the Doc.customer
    mktSub (Cstmr _)    = namedRef SRS.clientLabel   $ titleizeNP $ the Doc.client

-- | Helper for creating the 'General System Description' section ToC entry
mktGSDSec :: GSDSec -> ItemType
mktGSDSec (GSDProg l) =
  mkHeaderItem (namedRef SRS.genSysDescLabel $ titleize Doc.generalSystemDescription) $ map mktSub l
  where
    mktSub :: GSDSub -> Sentence
    mktSub (SysCntxt _)   = namedRef SRS.sysContextLabel     $ titleize  Doc.sysCont
    mktSub (UsrChars _)   = namedRef SRS.userCharsLabel      $ titleize' Doc.userCharacteristic
    mktSub (SystCons _ _) = namedRef SRS.sysConstraintsLabel $ titleize' Doc.systemConstraint

-- | Helper for creating the 'Specific System Description' section ToC entry
mktSSDSec :: SSDSec -> ItemType
mktSSDSec (SSDProg l) =
  mkHeaderItem (namedRef SRS.specSystDescLabel $ titleize Doc.specificsystemdescription) map mktSubSSD l
  where
    mktSubSSD :: SSDSub -> ItemType
    mktSubSSD (SSDProblem (PDProg _ _ sl1)) = Nested (Ref contPrbDescRef) (Bullet (map mktSubPD sl1))
    mktSubSSD (SSDSolChSpec (SCSProg sl2)) = Nested (Ref contSCSRef) (Bullet (map mktSubSCS sl2))

    mktSubPD :: PDSub -> (ItemType,Maybe String)
    mktSubPD (TermsAndDefs _ _) = (Flat (Ref contTaDRef),Nothing)
    mktSubPD PhySysDesc {} = (Flat (Ref contPSDRef),Nothing)
    mktSubPD (Goals _ _) = (Flat (Ref contGlStRef),Nothing)

    mktSubSCS :: SCSSub -> (ItemType,Maybe String)
    mktSubSCS (Assumptions _) = (Flat (Ref contAsmpRef),Nothing)
    mktSubSCS TMs {} = (Flat (Ref contTMRef),Nothing)
    mktSubSCS GDs {} = (Flat (Ref contGDRef),Nothing)
    mktSubSCS DDs {} = (Flat (Ref contDDRef),Nothing)
    mktSubSCS IMs {} = (Flat (Ref contIMRef),Nothing)
    mktSubSCS (Constraints _ _) = (Flat (Ref contDtCnstrRef),Nothing)
    mktSubSCS (CorrSolnPpties _ _) = (Flat (Ref contCSPRef),Nothing)

-- | Helper for creating the 'Requirements' section ToC entry
mktReqrmntSec :: ReqrmntSec -> [(ItemType,Maybe String)]
mktReqrmntSec (ReqsProg l) =
  mkTEList [(Ref contRqmtRef,map mktSubs l)]
  where
    mktSubs :: ReqsSub -> Sentence
    mktSubs (FReqsSub' _ _) = Ref contFReqsRef
    mktSubs (FReqsSub _ _) = Ref contFReqsRef
    mktSubs (NonFReqsSub _) = Ref contNFReqsRef

-- | Helper for creating the 'Likely Changes' section ToC entry
mktLCsSec :: LCsSec -> [(ItemType,Maybe String)]
mktLCsSec (LCsProg _) = [(Flat (Ref contLCsRef),Nothing)]

-- | Helper for creating the 'Unlikely Changes' section ToC entry
mktUCsSec :: UCsSec -> [(ItemType,Maybe String)]
mktUCsSec (UCsProg _) = [(Flat (Ref contUCsRef),Nothing)]

-- | Helper for creating the 'Traceability Matrices and Graphs' section ToC entry
mktTraceabilitySec :: TraceabilitySec -> [(ItemType,Maybe String)]
mktTraceabilitySec (TraceabilityProg _) = [(Flat (Ref contTMaGRef),Nothing)]

-- | Helper for creating the 'Values of Auxiliary Constants' section ToC entry
mktAuxConsSec :: AuxConstntSec -> [(ItemType,Maybe String)]
mktAuxConsSec (AuxConsProg _ _) = [(Flat (Ref contVACRef),Nothing)]

-- | Helper for creating the 'References' section ToC entry
mktBib :: [(ItemType,Maybe String)]
mktBib = [(Flat (Ref contBibRef),Nothing)]

-- | Helper for creating the 'Appendix' section ToC entry
mktAppndxSec :: AppndxSec -> [(ItemType,Maybe String)]
mktAppndxSec (AppndxProg _) = [(Flat (Ref contApndxRef),Nothing)]

-- | Helper for creating the 'Off-The-Shelf Solutions' section ToC entry
mktOffShelfSolnSec :: OffShelfSolnsSec -> [(ItemType,Maybe String)]
mktOffShelfSolnSec (OffShelfSolnsProg _) = [(Flat (Ref contOtSSRef),Nothing)]










{-- Table of Contents section entry
contToCRef :: Reference
contToCRef = makeSecRef "ToC" "Table of Contents"

-- Reference Material section entries
contRefMatRef :: Reference
contRefMatRef = makeSecRef "RefMat" "Reference Material"

contToURef :: Reference
contToURef = makeSecRef "ToU" "Table of Units"

contToSRef :: Reference
contToSRef = makeSecRef "ToS" "Table of Symbols"

contToAARef :: Reference
contToAARef = makeSecRef "TAbbAcc" "Table of Abbreviations and Acronyms"

-- Introduction section entries
contIntroRef :: Reference
contIntroRef = makeSecRef "Intro" "Instroduction"

contPoDRef :: Reference
contPoDRef = makeSecRef "DocPurpose" "Purpose of Document"

contSoRRef :: Reference
contSoRRef = makeSecRef "ReqsScope" "Scope of Requirements"

contCIRRef :: Reference
contCIRRef = makeSecRef "ReaderChars" "Characteristics of Intended Reader"

contOoDRef :: Reference
contOoDRef = makeSecRef "DocOrg" "Organization of Document"

-- Stakeholders section entries
contStkhldrRef :: Reference
contStkhldrRef = makeSecRef "Stakeholder" "Stakeholders"

contClntRef :: Reference
contClntRef = makeSecRef "Client" "The Client"

contCstmrRef :: Reference
contCstmrRef = makeSecRef "Customer" "The Customer"

-- General System Description section entries
contGSDRef :: Reference
contGSDRef = makeSecRef "GenSysDesc" "General System Description"

contSysCtxtRef :: Reference
contSysCtxtRef = makeSecRef "SysContext" "System Context"

contUsrChrRef :: Reference
contUsrChrRef = makeSecRef "UserChars" "User Characteristics"

contSysConRef :: Reference
contSysConRef = makeSecRef "SysConstraints" "System Constraints"

-- Specific System Description section entries
contSSDRef :: Reference
contSSDRef = makeSecRef "SpecSystDesc" "Specific System Description"

contPrbDescRef :: Reference
contPrbDescRef = makeSecRef "ProbDesc" "Problem Description"

contSCSRef :: Reference
contSCSRef = makeSecRef "SolCharSpec" "Solution Characteristics Specification"

contTaDRef :: Reference
contTaDRef = makeSecRef "TermDefs" "Terminology and Definitions"

contPSDRef :: Reference
contPSDRef = makeSecRef "PhysSyst" "Physical System Description"

contGlStRef :: Reference
contGlStRef = makeSecRef "GoalStmt" "Goal Statements"

contAsmpRef :: Reference
contAsmpRef = makeSecRef "Assumps" "Assumptions"

contTMRef :: Reference
contTMRef = makeSecRef "TMs" "Theoretical Models"

contGDRef :: Reference
contGDRef = makeSecRef "GDs" "General Definitions"

contDDRef :: Reference
contDDRef = makeSecRef "DDs" "Data Definitions"

contIMRef :: Reference
contIMRef = makeSecRef "IMs" "Instance Models"

contDtCnstrRef :: Reference
contDtCnstrRef = makeSecRef "DataConstraints" "Data Constraints"

contCSPRef :: Reference
contCSPRef = makeSecRef "CorSolProps" "Properties of a Correct Solution"

-- Requirements section entries
contRqmtRef :: Reference
contRqmtRef = makeSecRef "Requirements" "Requirements"

contFReqsRef :: Reference
contFReqsRef = makeSecRef "FRs" "Functional Requirements"

contNFReqsRef :: Reference
contNFReqsRef = makeSecRef "NFRs" "Non-Functional Requirements"

-- Likely Changes section entry
contLCsRef :: Reference
contLCsRef = makeSecRef "LCs" "Likely Changes"

-- Unlikely Changes section entry
contUCsRef :: Reference
contUCsRef = makeSecRef "UCs" "Unlikely Changes"

-- Traceability Matrices and Graphs section entry
contTMaGRef :: Reference
contTMaGRef = makeSecRef "TraceMatrices" "Traceability Matrices and Graphs"

-- Values of Auxiliary Constants section entry
contVACRef :: Reference
contVACRef = makeSecRef "AuxConstants" "Values of Auxiliary Constants"

-- References section entry
contBibRef :: Reference
contBibRef = makeSecRef "References" "References"

-- Appendix section entry
contApndxRef :: Reference
contApndxRef = makeSecRef "Appendix" "Appendix"

-- Off-The-Shelf Solutions section entry
contOtSSRef :: Reference
contOtSSRef = makeSecRef "offShelfSolns" "Off-The-Shelf Solutions"-}