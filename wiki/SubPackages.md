This wiki page consists of a breakdown of what changed after the split of Drasil into subpackages and acts as an FAQ section. Questions can be asked by creating issues and assigning them to @samm82.

# Packages Overview
Drasil is compiled using many different sub-packages that have evolved constantly over the years. Thus, noting the packages in a list requires constant maintenance. Please visit the Drasil website for [updated dependency graphs](https://jacquescarette.github.io/Drasil/#Sec:Documentation). This list has been updated as of July 9, 2021.

| Package | Description | Dependencies on Sub-packages |
|---|---|---|
| `drasil-build`     | Outlines basic types and functions needed to generate Makefiles and compile generated code. |  <ul><li>`drasil-lang`</li></ul> |
| `drasil-code`      | Holds the language for types, classes, and functions related to generating code. | <ul><li>`drasil-build`</li><li>`drasil-code-base`</li><li>`drasil-database`</li><li>`drasil-gool`</li><li>`drasil-lang`</li><li>`drasil-printers`</li><li>`drasil-theory`</li><li>`drasil-utils`</li></ul> |
| `drasil-code-base` | Proxy package between `drasil-printers` and `drasil-code`, for the bits of code in `drasil-printers` that relies on bits of `drasil-code` (`CodeExpr` specifically). | <ul><li>`drasil-database`</li><li>`drasil-lang`</li><li>`drasil-utils`</li></ul> |
| `drasil-data`      | Fundamental knowledge that is shareable amongst many pieces of software. Includes common concepts in mathematics, physics, and computer science. | <ul><li>`drasil-lang`</li><li>`drasil-metadata`</li><li>`drasil-theory`</li><li>`drasil-utils`</li></ul> |
| `drasil-database`  | Database aspects of information collection. Contains the `ChunkDB` and `SystemInformation` types which are central to document generation. | <ul><li>`drasil-lang`</li><li>`drasil-theory`</li></ul> |
| `drasil-docLang`   | Drasil language related to generated document structure. | <ul><li>`drasil-lang`</li><li>`drasil-data`</li><li>`drasil-database`</li><li>`drasil-printers`</li><li>`drasil-theory`</li><li>`drasil-utils`</li></ul> |
| `drasil-example`   | Examples of Drasil use. Generates code, SRS documents, and Traceability Graphs. | <ul><li>`drasil-code`</li><li>`drasil-code-base`</li><li>`drasil-data`</li><li>`drasil-database`</li><li>`drasil-docLang`</li><li>`drasil-gen`</li><li>`drasil-gool`</li><li>`drasil-lang`</li><li>`drasil-metadata`</li><li>`drasil-printers`</li><li>`drasil-theory`</li><li>`drasil-utils`</li></ul> |
| `drasil-gen`       | For generating all artifacts. | <ul><li>`drasil-lang`</li><li>`drasil-gool`</li><li>`drasil-build`</li><li>`drasil-code`</li><li>`drasil-printers`</li></ul> |
| `drasil-gool`      | Renderer for generating code in object oriented languages. | <ul><li>`drasil-utils`</li></ul> |
| `drasil-lang`      | Representation language for Drasil, often with Chunks. |  |
| `drasil-metadata`  | Metadata used throughout Drasil. Currently just domains. |<ul><li>`drasil-lang`</li></ul>|
| `drasil-printers`  | Document renderer. Can print for `.dot`, `.tex`, `.html`, and `.md` files | <ul><li>`drasil-code-base`</li><li>`drasil-database`</li><li>`drasil-lang`</li><li>`drasil-utils`</li></ul> |
| `drasil-theory`    | General requirements knowledge for generating SRS with Definitions and Models | <ul><li>`drasil-lang`</li><li>`drasil-metadata`</li></ul>|
| `drasil-utils`     | Various utility functions. | <ul><li>`drasil-lang`</ul></li> |
| `drasil-website`   | Website generator for Drasil. | <ul><li>`drasil-data`</li><li>`drasil-database`</li><li>`drasil-docLang`</li><li>`drasil-gen`</li><li>`drasil-lang`</li><li>`drasil-printers`</li><li>`drasil-theory`</li><li>`drasil-utils`</li></ul> |


## Older Versions/Significant Changes
As of June 2019, Language.Drasil got split, mostly into `drasil-lang` and some into `drasil-code`. This means that anything not in `drasil-lang` that needs even just a part of it, must `import Language.Drasil`, and likewise for `Language.Drasil.Code`. The following splits were made for the smaller packages:

| The package ... | ... which is responsible for ... | ... was split from this one. |
|---|---|---|
| `drasil-lang` | representation language for two levels (basic information and chunks) | (present in old structure) |
| `drasil-code` | code generation | `drasil-lang` |
| `drasil-gen`  | generation of all artifacts | `drasil-code` |
| `drasil-printers` | document generation | `drasil-lang`|
| `drasil-build` | building system languages and renderers | `drasil-printers` |
| `drasil-database` | database aspects of information collection | `drasil-lang` |
| `drasil-theory` | general requirements knowledge | `drasil-lang` |
| `drasil-data` | fundamental knowledge that is shareable amongst many pieces of software | (present in old structure) |
| `drasil-utils` | various utility functions | `drasil-lang` and `-data` |
| `drasil-example` | examples of Drasil use | (present in old structure) |
| `drasil-docLang` | document language routines | `drasil-example` |

`drasil-lang` involves a "submodule" (if you will) called Language.Drasil.Development. This includes some developer tools, and has the potential to be expanded upon/pulled into its own subpackage in the future. `drasil-docLang` also has a "submodule" called Drasil.SRS - this allows for the needed qualified imports and consists of only the one file.

### Dependencies

Before, Drasil could be built all at once, starting with the "smallest" part and building up via imports. Now, specific packages need to be built before others. These dependencies are enforced in the following sub-changes. The following is a dependency tree of the subpackages, with arrows pointing towards what a package is dependent on.

<img style="padding: 0 15px; float: left;" src="https://user-images.githubusercontent.com/35857611/59066606-7236e300-887d-11e9-8b6c-cc5276701e89.png" alt="dependency_tree" width="500">

<!-- Source code:
digraph Drasil {
  "code"     -> "lang"
  "data"     -> "lang"
  "docLang"  -> "lang"
  "example"  -> "lang"
  "gen"      -> "lang"
  "printers" -> "lang"
  "build"    -> "lang"
  "database" -> "lang"
  "theory"   -> "lang"
  "utils"    -> "lang"
  "example"  -> "code"
  "gen"      -> "code"
  "docLang"  -> "data"
  "example"  -> "data"
  "example"  -> "docLang"
  "example"  -> "gen"
  "example"  -> "printers"
  "gen"      -> "printers"
  "code"     -> "build"
  "gen"      -> "build"
  "printers" -> "build"
  "code"     -> "database"
  "docLang"  -> "database"
  "printers" -> "database"
  "example"  -> "database"
  "data"     -> "theory"
  "database" -> "theory"
  "docLang"  -> "theory"
  "example"  -> "theory"
  "utils"    -> "theory"
  "data"     -> "utils"
  "docLang"  -> "utils"
  "example"  -> "utils"
  "printers" -> "utils"
}
-->

### Cabal Files

As part of this reorganization, the one drasil.cabal file in code/ got split into multiple .cabal files, one for each package with the same name as the package itself, and moved into their corresponding directories. Each file has a list of dependencies, which includes the Drasil subpackages, following the above hierarchy. Thus, a change to `drasil-lang` might require the version number to be bumped up (both in drasil-lang.cabal's header and in its superpackages' dependency lists) in order for changes to be recompiled.

### Makefile

The Makefile used to build the Drasil framework all at once, but again, that process has changed. It now builds them following the order of dependencies described above.

`make clean` has also been updated to (in addition to deleting build and logs) run `stack clean` in every directory required, which will help with some compilation issues.

# FAQ

**Q:** My code is there but an example isn't building properly/ I'm getting an import error when everything should work/ The Travis CI build of `master` passes, but my local machine has errors.

**A:** First try running `make clean`, then running `make` again. This forces your machine to recompile, using the packages on your local branch.