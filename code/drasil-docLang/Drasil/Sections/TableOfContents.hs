-- Standard code to make a table of contents.
module Drasil.Sections.TableOfContents (contToCRef, contRefMatRef, contToURef,
  contToSRef, contToAARef, contIntroRef, contPoDRef, contSoRRef, contCIRRef,
  contOoDRef, contStkhldrRef, contClntRef, contCstmrRef, contGSDRef,
  contSysCtxtRef, contUsrChrRef, contSysConRef, contSSDRef, contPrbDescRef,
  contSCSRef, contTaDRef, contPSDRef, contGlStRef, contAsmpRef, contTMRef,
  contGDRef, contDDRef, contIMRef, contDtCnstrRef, contCSPRef, contRqmtRef,
  contFReqsRef, contNFReqsRef, contLCsRef, contUCsRef, contTMaGRef, contVACRef,
  contBibRef, contApndxRef, contOtSSRef) where

import Language.Drasil
{- Which parts of the SRS do we have sections for?
Table of Contents ^
Reference Material
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
-- Table of Contents section entry
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
contOtSSRef = makeSecRef "offShelfSolns" "Off-The-Shelf Solutions"