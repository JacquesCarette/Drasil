## Introduction

The purpose of this page is to explain some of the words we use to mean specific
Drasil-related words. These all have a general meaning that hints at how we use
them in Drasil, but we often have a more specific meaning in mind
whenever we use them in the context of Drasil.

## Artifacts

- "Artifact": any and all files associated with a software project. These can
include the code, SRS, makefile, README, Doxygen config files, sample inputs,
dot files, etc. Implicitly, they are anything that Drasil can (or should be able
to) generate.
- "Software Dossier": artifacts directly related to the code, that are not
themselves code. Examples include Doxygen config files, READMEs, sample inputs,
and makefiles.
- "Code": What it sounds like - executable code. Currently we support C++, C#,
Java, Swift, Python, and Julia.
  - Note that `drasil-code` does not currently follow this convention: it
  contains both code- and Software Dossier-related functionality. We are working
  on refactoring it to improve its naming consistency.