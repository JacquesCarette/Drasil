module Drasil.DocLang.References (secRefs) where --, fillTraceSI, fillcdbSRS, fillReferences) where

import Drasil.DocLang.SRS
import Drasil.DocumentLanguage.Core -- (getTraceConfigUID)
import Drasil.DocDecl (SRSDecl, mkDocDesc)
import Drasil.TraceTable (generateTraceMap)

import Drasil.Sections.TableOfAbbAndAcronyms (tableAbbAccRef)
import Drasil.Sections.TableOfSymbols (symbTableRef)
import Drasil.Sections.TableOfUnits (unitTableRef)
import Drasil.Sections.TraceabilityMandGs (traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement, traceMatStandard)
import Drasil.Sections.Requirements (reqInputsRef)
import Drasil.Sections.AuxiliaryConstants (tableOfConstantsRef)
import Drasil.Sections.SpecificSystemDescription (tInDataCstRef, tOutDataCstRef)
import Drasil.DocumentLanguage.TraceabilityGraph (traceyGraphGetRefs)

import Language.Drasil
import Database.Drasil
{-
{-# Language TupleSections #-}
import Language.Drasil
import Drasil.DocumentLanguage.Core
import Utils.Drasil.Concepts
import qualified Drasil.DocLang.SRS as SRS
import qualified Data.Drasil.Concepts.Documentation as Doc
import qualified Data.Drasil.TheoryConcepts as Doc (dataDefn, genDefn, inModel, thModel)
-}{-
import Data.Drasil.Concepts.Computation (compcon, compcon')
import Data.Drasil.Concepts.Documentation (doccon, doccon')
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (physicCon, physicCon')
import Data.Drasil.Concepts.Software (softwarecon)
import Data.Drasil.Concepts.SolidMechanics (solidcon)
import Data.Drasil.Concepts.Thermodynamics (thermocon)
-}
import Data.List (nub, sortOn)
import qualified Data.Map as Map
import Control.Lens (set, (^.))

-- | All section references used in creating a Software Requirements Specification (SRS).
secRefs :: [Reference]
secRefs = sectionReferences ++ [tableAbbAccRef, reqInputsRef, symbTableRef,
  unitTableRef, tableOfConstantsRef, tInDataCstRef, tOutDataCstRef]
  ++ map (ref.makeTabRef.getTraceConfigUID) [traceMatAssumpAssump,
  traceMatAssumpOther, traceMatRefinement]

{-
-- Assuming ChunkDB with no traces and minimal references, fill in for rest of system information.
fillcdbSRS :: SRSDecl -> SystemInformation -> SystemInformation
fillcdbSRS srsDec si = fillReferences srsDec $ fillTraceSI srsDec si

{-Don't want to manually add concepts here, Drasil should do it automatically
fillConcepts :: SystemInformation -> SystemInformation
fillConcepts si = si2
  where
    si2 = set sysinfodb chkdb2 si
    chkdb = si ^. sysinfodb
    tmtbl = termTable chkdb
    chkdb2 = chkdb{termTable = termMap $ nub (map nw doccon ++ map nw doccon' ++ map nw softwarecon ++ map nw physicCon ++ map nw physicCon' ++ map nw physicalcon ++ map nw educon ++ map nw mathcon ++ map nw mathcon' ++ map nw compcon ++ map nw compcon' ++ map nw solidcon ++ map nw thermocon ++ (map (fst.snd) $ Map.assocs tmtbl))}
-}

-- | Gets references from definitions and models.
dRefToRef :: HasDecRef a => a -> [Reference]
dRefToRef r = map ref $ r ^. getDecRefs

-- | Takes in existing information from the Chunk database to construct a database of references.
fillReferences :: SystemInformation -> SystemInformation
fillReferences dd si = si2
  where
    si2 = set sysinfodb chkdb2 si
    chkdb = si ^. sysinfodb
    rfdb = refdb si
    chkdb2 = set refTable (idMap $ nub (refsFromSRS ++ inRefs ++ map (ref.makeTabRef.getTraceConfigUID) (traceMatStandard si) ++ traceyGraphGetRefs (si ^. folderPath) ++ secRefs ++ map ref cites ++ map ref conins ++ map ref ddefs ++ map ref gdefs ++ map ref imods ++ map ref tmods ++ map ref concIns ++ map ref secs ++ map ref lblCon ++ refs)) chkdb 
    inRefs = concatMap dRefToRef ddefs ++ concatMap dRefToRef gdefs ++ concatMap dRefToRef imods ++ concatMap dRefToRef tmods
    ddefs   = map (fst.snd) $ Map.assocs $ chkdb ^. dataDefnTable
    gdefs   = map (fst.snd) $ Map.assocs $ chkdb ^. gendefTable
    imods   = map (fst.snd) $ Map.assocs $ chkdb ^. insmodelTable
    tmods   = map (fst.snd) $ Map.assocs $ chkdb ^. theoryModelTable
    concIns = map (fst.snd) $ Map.assocs $ chkdb ^. conceptinsTable
    secs    = map (fst.snd) $ Map.assocs $ chkdb ^. sectionTable
    lblCon  = map (fst.snd) $ Map.assocs $ chkdb ^. labelledcontentTable
    refs    = map (fst.snd) $ Map.assocs $ chkdb ^. refTable
    cites   = map (fst.snd) $ Map.assocs $ rfdb  ^. citationDB
    conins  = map (fst.snd) $ Map.assocs $ rfdb  ^. conceptDB
    refsFromSRS = findRefsFromSRS l si
    l = mkDocDesc si dd

-- | Helper for filling in the traceability matrix and graph information into the system.
fillTraceSI :: SRSDecl -> SystemInformation -> SystemInformation
fillTraceSI dd si = fillTraceMaps l $ fillReqs l si
  where
    l = mkDocDesc si dd

-- | Fills in the traceabiliy matrix and graphs section of the system information using the document description.
fillTraceMaps :: DocDesc -> SystemInformation -> SystemInformation
fillTraceMaps dd si@SI{_sysinfodb = db} = si {_sysinfodb =
  set refbyTable (generateRefbyMap tdb) $ set traceTable tdb db} where
  tdb = generateTraceMap dd

-- | Fills in the requirements section of the system information using the document description.
fillReqs :: DocDesc -> SystemInformation -> SystemInformation
fillReqs [] si = si
fillReqs (ReqrmntSec (ReqsProg x):_) si@SI{_sysinfodb = db} = genReqs x
  where
    genReqs [] = si
    genReqs (FReqsSub c _:_) = si {_sysinfodb = set conceptinsTable newCI db} where
        newCI = idMap $ nub $ c ++ map fst (sortOn snd $ map snd $ Map.toList $ db ^. conceptinsTable)
    genReqs (_:xs) = genReqs xs
fillReqs (_:xs) si = fillReqs xs si
-}{-
-- TODO: Use DLPlate for this.
findRefsFromSRS :: DocDesc -> SystemInformation -> [Reference]
findRefsFromSRS dd si = concatMap separateSec
-- | Finds all possible sections and subsections to make a list of references.
separateSec :: DocSection -> [Reference]
separateSec TableOfContents      = [mktToCSec]
separateSec (RefSec rs)          = mktRefSec rs
separateSec (IntroSec i)         = mktIntroSec i
separateSec (StkhldrSec sts)     = mktStkhldrSec sts
separateSec (GSDSec gs')         = mktGSDSec gs'
separateSec (SSDSec ss)          = mktSSDSec ss
separateSec (ReqrmntSec r)       = mktReqrmntSec r
separateSec (LCsSec lc)          = mktLCsSec lc
separateSec (UCsSec ulcs)        = mktUCsSec ulcs
separateSec (TraceabilitySec t)  = mktTraceabilitySec t
separateSec (AuxConstntSec acs)  = mktAuxConsSec acs
separateSec Bibliography         = mktBib
separateSec (AppndxSec a)        = mktAppndxSec a
separateSec (OffShelfSolnsSec o) = mktOffShelfSolnSec o

-- | Helper for creating the 'Table of Contents' section Reference.
mktToCSec :: Reference
mktToCSec = SRS.tOfContLabel

getLblContent :: Contents -> [Reference]
getLblContent (LlC r _) = [r]
getLblContent _ = []

-- | Helper for creating the 'Reference Material' section ToC entry
mktRefSec :: RefSec -> [Reference]
mktRefSec (RefProg c l) = SRS.refMatLabel ++ ref (getLblContent c) ++ map mktSubRef l
  where
    mktSubRef :: RefTab -> Reference
    mktSubRef TUnits        = SRS.tOfUnitLabel   
    mktSubRef (TUnits' _ _) = SRS.tOfUnitLabel   
    mktSubRef (TSymb _)     = SRS.tOfSymbLabel   
    mktSubRef (TSymb' _ _)  = SRS.tOfSymbLabel   
    mktSubRef TAandA        = SRS.tOfAbbAccLabel 

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
  mkHeaderItem' (namedRef SRS.specSystDescLabel $ titleize Doc.specificsystemdescription) $ map mktSubSSD l
  where
    mktSubSSD :: SSDSub -> ItemType
    mktSubSSD (SSDProblem (PDProg _ _ sl1)) = mkHeaderItem (namedRef SRS.probDescLabel $ titleize Doc.problemDescription) $ map mktSubPD sl1
    mktSubSSD (SSDSolChSpec (SCSProg sl2))  = mkHeaderItem (namedRef SRS.solCharSpecLabel $ titleize Doc.solutionCharSpec) $ map mktSubSCS sl2

    mktSubPD :: PDSub -> Sentence
    mktSubPD (TermsAndDefs _ _) = namedRef SRS.termDefsLabel $ titleize' Doc.termAndDef
    mktSubPD PhySysDesc {}      = namedRef SRS.physSystLabel $ titleize  Doc.physSyst
    mktSubPD (Goals _ _)        = namedRef SRS.goalStmtLabel $ titleize' Doc.goalStmt

    mktSubSCS :: SCSSub -> Sentence
    mktSubSCS (Assumptions _)      = namedRef SRS.assumptLabel     $ titleize' Doc.assumption
    mktSubSCS TMs {}               = namedRef SRS.thModelLabel     $ titleize' Doc.thModel
    mktSubSCS GDs {}               = namedRef SRS.genDefnLabel     $ titleize' Doc.genDefn
    mktSubSCS DDs {}               = namedRef SRS.dataDefnLabel    $ titleize' Doc.dataDefn
    mktSubSCS IMs {}               = namedRef SRS.inModelLabel     $ titleize' Doc.inModel
    mktSubSCS (Constraints _ _)    = namedRef SRS.datConLabel      $ titleize' Doc.datumConstraint
    mktSubSCS (CorrSolnPpties _ _) = namedRef SRS.corSolPropsLabel $ titleize' Doc.propOfCorSol

-- | Helper for creating the 'Requirements' section ToC entry
mktReqrmntSec :: ReqrmntSec -> ItemType
mktReqrmntSec (ReqsProg l) =
  mkHeaderItem (namedRef SRS.requirementsLabel $ titleize' Doc.requirement) $ map mktSubs l
  where
    mktSubs :: ReqsSub -> Sentence
    mktSubs (FReqsSub' _ _) = namedRef SRS.funcReqLabel    $ titleize' Doc.functionalRequirement
    mktSubs (FReqsSub _ _)  = namedRef SRS.funcReqLabel    $ titleize' Doc.functionalRequirement
    mktSubs (NonFReqsSub _) = namedRef SRS.nonfuncReqLabel $ titleize' Doc.nonfunctionalRequirement

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
-}
