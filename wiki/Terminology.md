## Introduction

The purpose of this page is to explain some of the words we use to mean specific
Drasil-related words. These all have a general meaning that hints at how we use
them in Drasil, but we often have a more specific meaning in mind
whenever we use them in the context of Drasil.

## Glossary

- "Artifact": any and all files associated with a software project. These can
include the code, SRS, makefile, README, Doxygen config files, sample inputs,
dot files, etc. Implicitly, they are anything that Drasil can (or should be able
to) generate. Note that the 'file' part is not actually important, it merely
corresponds to the most common representation of artifacts in current use.
Use with caution, as it is not necessarily meaningful as it is too broad.
- "Software Dossier": artifacts directly related to the code, that are not
themselves code. Examples include Doxygen config files, READMEs, sample inputs,
and makefiles.
- "Code": What it sounds like - source code. Currently we support C++, C#,
Java, Swift, Python, and Julia.
- "Chunk": a small, unique nugget of reusable knowledge.
- "Theory": a collection of types and domains, function symbols,
relation symbols, function and relation definitions, axioms (and
potentially theorems). We eventually want to view an ontology as a special
case of a Theory.

*Note* that `drasil-code` does not currently follow this convention: it
  contains both code- and Software Dossier-related functionality. We are working
  on refactoring it to improve its naming consistency.
