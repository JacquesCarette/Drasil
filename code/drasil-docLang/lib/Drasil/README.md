--------------------------------------------------
### Summary of Folder Structure and File Contents
Last updated: July 19, 2018
--------------------------------------------------

**DocLang**
  - Contains SRS.hs, with functions for building a Software Requirements Specification, and Notebook.hs for building Lesson Plans.

**DocumentLanguage**
  - Contains helpers for generating documents.

**Sections**
  - Contains the files used to auto-generate the paragraphs of each case study.

DocDecl.hs
  - Document declaration functions for generating Software Requirement Specifications.

DocLang.hs
  - Re-export document language types and functions for easy use in other packages.

DocumentLanguage.hs
  - Contains the current implementation of the document *recipe* language.

ExtractDocDesc.hs
  - Contains plates to extract sentences and exprs from a document.

README.md
  - This file

SRSDocument.hs
  - Re-export DocLang functions for easy use in examples.

TraceTable.hs
  - Defines a DLPlate for tracability between pieces of information.

