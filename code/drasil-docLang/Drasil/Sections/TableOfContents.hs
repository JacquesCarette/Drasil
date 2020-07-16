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

-- Table of Contents section entry
contToCRef :: Reference
contToCRef = makeToCRef "ToC" "Table of Contents"

-- Reference Material section entries
contRefMatRef :: Reference
contRefMatRef = makeToCRef "RefMat" "Reference Material"

contToURef :: Reference
contToURef = makeToCRef "ToU" "Table of Units"

contToSRef :: Reference
contToSRef = makeToCRef "ToS" "Table of Symbols"

contToAARef :: Reference
contToAARef = makeToCRef "TAbbAcc" "Table of Abbreviations and Acronyms"

-- Introduction section entries
contIntroRef :: Reference
contIntroRef = makeToCRef "Intro" "Instroduction"

contPoDRef :: Reference
contPoDRef = makeToCRef "DocPurpose" "Purpose of Document"

contSoRRef :: Reference
contSoRRef = makeToCRef "ReqsScope" "Scope of Requirements"

contCIRRef :: Reference
contCIRRef = makeToCRef "ReaderChars" "Characteristics of Intended Reader"

contOoDRef :: Reference
contOoDRef = makeToCRef "DocOrg" "Organization of Document"

-- Stakeholders section entries
contStkhldrRef :: Reference
contStkhldrRef = makeToCRef "Stakeholder" "Stakeholders"

contClntRef :: Reference
contClntRef = makeToCRef "Client" "The Client"

contCstmrRef :: Reference
contCstmrRef = makeToCRef "Customer" "The Customer"

-- General System Description section entries
contGSDRef :: Reference
contGSDRef = makeToCRef "GenSysDesc" "General System Description"

contSysCtxtRef :: Reference
contSysCtxtRef = makeToCRef "SysContext" "System Context"

contUsrChrRef :: Reference
contUsrChrRef = makeToCRef "UserChars" "User Characteristics"

contSysConRef :: Reference
contSysConRef = makeToCRef "SysConstraints" "System Constraints"

-- Specific System Description section entries
contSSDRef :: Reference
contSSDRef = makeToCRef "SpecSystDesc" "Specific System Description"

contPrbDescRef :: Reference
contPrbDescRef = makeToCRef "ProbDesc" "Problem Description"

contSCSRef :: Reference
contSCSRef = makeToCRef "SolCharSpec" "Solution Characteristics Specification"

contTaDRef :: Reference
contTaDRef = makeToCRef "TermDefs" "Terminology and Definitions"

contPSDRef :: Reference
contPSDRef = makeToCRef "PhysSyst" "Physical System Description"

contGlStRef :: Reference
contGlStRef = makeToCRef "GoalStmt" "Goal Statements"

contAsmpRef :: Reference
contAsmpRef = makeToCRef "Assumps" "Assumptions"

contTMRef :: Reference
contTMRef = makeToCRef "TMs" "Theoretical Models"

contGDRef :: Reference
contGDRef = makeToCRef "GDs" "General Definitions"

contDDRef :: Reference
contDDRef = makeToCRef "DDs" "Data Definitions"

contIMRef :: Reference
contIMRef = makeToCRef "IMs" "Instance Models"

contDtCnstrRef :: Reference
contDtCnstrRef = makeToCRef "DataConstraints" "Data Constraints"

contCSPRef :: Reference
contCSPRef = makeToCRef "CorSolProps" "Properties of a Correct Solution"

-- Requirements section entries
contRqmtRef :: Reference
contRqmtRef = makeToCRef "Requirements" "Requirements"

contFReqsRef :: Reference
contFReqsRef = makeToCRef "FRs" "Functional Requirements"

contNFReqsRef :: Reference
contNFReqsRef = makeToCRef "NFRs" "Non-Functional Requirements"

-- Likely Changes section entry
contLCsRef :: Reference
contLCsRef = makeToCRef "LCs" "Likely Changes"

-- Unlikely Changes section entry
contUCsRef :: Reference
contUCsRef = makeToCRef "UCs" "Unlikely Changes"

-- Traceability Matrices and Graphs section entry
contTMaGRef :: Reference
contTMaGRef = makeToCRef "TraceMatrices" "Traceability Matrices and Graphs"

-- Values of Auxiliary Constants section entry
contVACRef :: Reference
contVACRef = makeToCRef "AuxConstants" "Values of Auxiliary Constants"

-- References section entry
contBibRef :: Reference
contBibRef = makeToCRef "References" "References"

-- Appendix section entry
contApndxRef :: Reference
contApndxRef = makeToCRef "Appendix" "Appendix"

-- Off-The-Shelf Solutions section entry
contOtSSRef :: Reference
contOtSSRef = makeToCRef "offShelfSolns" "Off-The-Shelf Solutions"