# Shallow File Analysis

## [`drasil-makefile`](../../../code/drasil-makefile)

### [`drasil-makefile/lib/Drasil/Makefile.hs`](../../../code/drasil-makefile/lib/Drasil/Makefile.hs)

Re-exports smart constructors for building a Makefile from `Drasil.Makefile.Make.*`.

1. [ ] The API should be straightforward enough such that we can immediately re-export entire modules rather than each import individually.
2. [X] ~~Should be renamed to `Drasil.Build` (as per #1211).~~ Renamed project to `drasil-makefile` and module to `Drasil.Makefile` instead. [#4976](https://github.com/JacquesCarette/Drasil/pull/4976)

### [`drasil-makefile/lib/Drasil/Makefile/Make/`](../../../code/drasil-makefile/lib/Drasil/Makefile/Make)

Namespace carrying an AST of a `Makefile` as well as helper functions, a DSL for Makefile expressions/variables (`MakeString`), and a `Doc`-renderer for the AST.

1. [X] ~~Should be renamed to `Drasil.Build.Makefile`.~~ Moved under `Drasil.Makefile.Make` in `drasil-makefile`. [#4976](https://github.com/JacquesCarette/Drasil/pull/4976)

#### [`drasil-makefile/lib/Drasil/Makefile/Make/AST.hs`](../../../code/drasil-makefile/lib/Drasil/Makefile/Make/AST.hs)

Declares types for building a `Makefile`, declaring it as a list of `Rule`s.

1. [ ] `type Annotation = [String]` is really a "comment" and is referred to as a comment everywhere else. This alias should just be renamed to `Comment`. Furthermore, instead of manually splitting annotations/comments up across lines (which is what the list bit is for), we should have the pretty-printer deal with automatically hard-wrapping.
2. [ ] There are 3 constructors with unconventional names: `M`, `R`, and `C`. These should be renamed to things more appropriate (I would prefer `Makefile`, `Rule`, and `Cmd`, respectively).
3. [ ] The Makefile encoding should contain a `NonEmpty` list of `Rule`s, not a simple `[Rule]` which admits empty rule sets. The head of this list should also be understood as the _default target_. However, see (4).
4. [ ] The Makefile encoding does not admit extra whitespace, comments, _assignments_, nor imports. A `Makefile` should really be understood as a list of these (along with `Rule`s, of course).
5. [ ] Variables do not appear in the AST, but are pulled out later from targets to create portable variables (see below code snippet). This is a leaky abstraction.
    ```Makefile
    ifeq "$(OS)" "Windows_NT"
        TARGET_EXTENSION=.exe
    else
        UNAME_S := $(shell uname -s)
        ifeq ($(UNAME_S), Linux)
            TARGET_EXTENSION=
        endif
        ifeq ($(UNAME_S), Darwin)
            TARGET_EXTENSION=
        endif
    endif
    ```
6. [ ] `CommandOpts` is an incomplete enumeration. Commands can also be silenced (`@`) or forced to always be run (`+`, ignoring dry run).
7. [ ] `Annotation` is only 'attached' to other rules. We should be able to write arbitrary comments within a `Makefile` (i.e., we need to recognize a `Makefile` as a sequence of something more than just rules). Annotations on individual shell commands are helpful, however. We should add that and remove attaching annotations (descriptive comments) before rules.
8. [ ] We should rename `.AST` to `.Core`.

Overall, the Makefile AST is incomplete. It works fine for our current purposes, but the variables issue is a leaky abstraction that should be fixed.

#### [`drasil-makefile/lib/Drasil/Makefile/Make/Helpers.hs`](../../../code/drasil-makefile/lib/Drasil/Makefile/Make/Helpers.hs)

Only 3 functions are necessary to export:

1. `addCommonFeatures`: Despite the name, does not add common features. It prepends a `Doc` with a list of variables with OS-specific values. However, this is a leaky abstraction:
  1. It does not consider duplicate variables (actually, it just naively filters them out -- not good), for example.
  2. It is the only way to write global variables in a `Makefile` with this AST.
  3. It requires that each variable have a value set for the major 3 operating systems (and restricts itself to the major 3).
  4. It pulls out variables from `Rule`s!!!! This means that if we have side-effects in the assignments, unintended Makefile rules be partially applied!
2. `tab`: represents a "tab" character in `pretty`'s `Doc` format.
3. `msIndent`: nests a `Doc` 4 spaces to the right.

1. [ ] `addCommonFeatures` needs to be rebuilt.
2. [ ] `tab` and `msIndent` are good, but are very generic functions. We should consider moving them elsewhere.

We should consider creating another package (e.g., `drasil-doc` or `drasil-artifacts` / `drasil-file-handling` that exports a bunch of features atop `pretty`), such as `tab` and `msIndent`. We can also take this as an opportunity to slowly move packages towards `prettyprinter` (the modern, `Text`-based document writer). Doing so would mean contributing to #4475. Two birds (rebuilding the Makefile AST/printer and beginning the move towards `prettyprinter`) with one stone.

#### `drasil-makefile/lib/Drasil/Makefile/Make/Import.hs` (Deleted)

Formerly `Build.Drasil.Import` (or `Build.Drasil.Make.Import`).

Exported:

1. `RuleTransformer(..)`: A typeclass used to capture how arbitrary things can be translated into a list of `Rule`s.
2. `toMake`: A function that used `RuleTransformer` to build a `Makefile`.

`RuleTransformer` was previously instantiated for two things:

1. `DocSpec`: This was very bad because there was a lot of hard-coded discussion of specific target formats and document categories (the SRS!).
  ```haskell
  -- | Allows the creation of Makefiles for documents that use LaTeX.
  instance RuleTransformer DocSpec where
    makeRule (DocSpec (DC [TeX]) fn) = [
      mkRule [watermark] (makeS "srs") [pdfName] [],
      mkFile [] pdfName [makeS $ fn ++ ".tex"] $
        map ($ fn) [lualatex, bibtex, lualatex, lualatex]] where
          lualatex, bibtex :: String -> Command
          lualatex = mkCheckedCommand . (+:+) (makeS "lualatex" +:+ mkFreeVar "TEXFLAGS") . makeS
          bibtex = mkCommand . (+:+) (makeS "bibtex" +:+ mkFreeVar "BIBTEXFLAGS") . makeS
          pdfName = makeS $ fn ++ ".pdf"
    makeRule (DocSpec (DC [MDBook]) _) = [
      mkRule [watermark] (makeS "build")  [] [build],
      mkRule [] (makeS "server") [] [server]]
      where
        build = mkCheckedCommand $ makeS "mdbook build"
        server = mkCheckedCommand $ makeS "mdbook serve --open"
    makeRule _ = []
  ```
2. `CodeHarness`: Similarly, did not expose any configuration options. For Python-based projects for example, no `build` step is necessary, but this code always built it.
  ```haskell
  -- | Transforms information in 'CodeHarness' into a list of Makefile rules.
  instance RuleTransformer CodeHarness where
    makeRule (Ch b r s m d) = maybe [mkRule (openingComments m) buildTarget [] []]
      (\(BuildConfig comp onm anm bt) ->
      let outnm = maybe (asFragment "") (renderBuildName s m nameOpts) onm
          addnm = maybe (asFragment "") (renderBuildName s m nameOpts) anm
      in [
      mkRule (openingComments m) buildTarget [outnm] [],
      mkFile [] outnm (map (makeS . filePath) (progMods m)) $
        map (mkCheckedCommand . foldr (+:+) mempty) $
          comp (getCompilerInput bt s m) outnm addnm
      ]) b ++ maybe [] (\(Runnable nm no ty) -> [
      mkRule [] (makeS "run") [buildTarget] [
        mkCheckedCommand $ buildRunTarget (renderBuildName s m no nm) ty +:+
        mkFreeVar "RUNARGS"
        ]
      ]) r ++ maybe [] (\(DocConfig dps cmds) -> [
        mkRule [] (makeS "doc") (dps ++ getCommentedFiles s) cmds
      ]) d where
        buildTarget = makeS "build"
  ```

1. [X] ~~`.Import` is not a name I quite like. Is it importing something else or is something importing it? It defines an interface for generating things into `Makefile`s. Either way, there's probably a better name: `Builder`. Alternatively, this code can be merged into `.AST`/`.Core`. It's unclear why this file separation is strictly necessary.~~ Removed in [e9bf2e1218](https://github.com/JacquesCarette/Drasil/commit/e9bf2e1218435bb98edfdb8fe9c57cf09ce49877).
2. [X] ~~The `RuleTransformer` does not support any sort of configuration. Is this really necessary? This typeclass seems like an over-engineering attempt. It doesn't appear to add anything of value and it is currently the only way to form a `Makefile` (not just a list of `Rule`s) -- why? I think we can (should) remove it.~~ Removed in [e9bf2e1218](https://github.com/JacquesCarette/Drasil/commit/e9bf2e1218435bb98edfdb8fe9c57cf09ce49877). Makefile generation for `DocSpec` was moved to `drasil-gen` (`Drasil.Generator.Formats.buildMakefile`) and for `CodeHarness` (imperative build configs) to `drasil-code` (`Language.Drasil.Code.Imperative.Build.Import.buildMakefile`).

#### [`drasil-makefile/lib/Drasil/Makefile/Make/MakeString.hs`](../../../code/drasil-makefile/lib/Drasil/Makefile/Make/MakeString.hs)

Contains a language for building Makefile expressions and declaring variables.

This language should be audited properly. It looks like it would work for our very specific usecase of `Makefile`s in Drasil, but be very limiting for everything else.

#### [`drasil-makefile/lib/Drasil/Makefile/Make/Print.hs`](../../../code/drasil-makefile/lib/Drasil/Makefile/Make/Print.hs)

There isn't much to this file. It only contains a renderer for `Makefile`s to `Doc`s. Very standard. However, it would benefit from having its own options/choices. For example, `.PHONY` is commonly placed above each 'abstract' rule definition. We use the single-list-style, but the additive-style is also common.
