# Shallow Analysis

## `drasil-build`

### `Build.Drasil`

Reexports smart constructors for building a Makefile from `Build.Drasil.Make.*`.

1. [ ] The API should be straightforward enough such that we can immediately reexport entire modules rather than each import individually.
2. [ ] Should be renamed to `Drasil.Build` (as per #1211).

### `Build.Drasil.Make`

Namespace carrying an AST of a `Makefile` as well as a `Doc`-renderer for the AST and a "rule transformer" (a typeclass that defines a single function, `makeRule`, that is intended to capture how a Makefile can be generated for [?] for a specific type).

1. [ ] Should be renamed to `Drasil.Build.Makefile`.

#### `Build.Drasil.Make.AST`

Declares types for building a `Makefile`, declaring it as a list of `Rule`s.

1. [ ] `type Annotation` is really a "comment" and is referred to as a comment everywhere else. This alias should just be renamed to `Comment`.
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
7. [ ] All usage of `Annotation` is a single-entry list or an empty list. We should consider changing this type alias to a `Maybe String`. Long `String`s can be hard-wrapped automatically (and realistically, this is a kind of "choice").
8. [ ] We should rename `.AST` to `.Core`.

Overall, the Makefile AST is incomplete. It works fine for our current purposes, but the variables issue is a leaky abstraction that should be fixed.

#### `Build.Drasil.Helpers`

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

We should consider creating another package (e.g., `drasil-doc` or `drasil-artifacts` that exports a bunch of features atop `pretty`), such as `tab` and `msIndent`. We can also take this as an opportunity to slowly move packages towards `prettyprinter` (the modern, `Text`-based document writer). Doing so would mean contributing to #4475. Two birds (rebuilding the Makefile AST/printer and beginning the move towards `prettyprinter`) with one stone.

#### `Build.Drasil.Import`

Only 2 things are necessary to export:

1. `RuleTransformer(..)`: A typeclass used to capture how arbitrary things can be translated into a list of `Rule`s.
2. `toMake`: A function that uses `RuleTransformer` to build a `Makefile`.

`RuleTransformer` is only instantiated for two things:

1. `DocSpec`: This is very bad because there is a lot of hard-coded discussion of specific target formats and document categories (the SRS!). This is deserving of being broken up further.
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
2. `CodeHarness`: Similarly, worth breaking up. It does not expose any configuration options. For Python-based projects for example, no `build` step is necessary, but this code will [always build it](https://github.com/JacquesCarette/Drasil/blob/main/code/stable/dblpend/src/python/Makefile).
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

1. [ ] `.Import` is not a name I quite like. Is it importing something else or is something importing it? It defines an interface for generating things into `Makefile`s. Either way, there's probably a better name: `Builder`. Alternatively, this code can be merged into `.AST`/`.Core`. It's unclear why this file separation is strictly necessary.
2. [ ] The `RuleTransformer` does not support any sort of configuration. Is this really necessary? This typeclass seems like an over-engineering attempt. It doesn't appear to add anything of value and it is currently the only way to form a `Makefile` (not just a list of `Rule`s) -- why? I think we can (should) remove it.

`Makefile`s should be specialized to more choices. I believe this code should be redesigned.

#### `Build.Drasil.MakeString`

Contains a language for building Makefile expressions and declaring variables.

This language should be audited properly. It looks like it would work for our very specific usecase of `Makefile`s in Drasil, but be very limiting for everything else.

#### `Build.Drasil.Print`

There isn't much to this file. It only contains a renderer for `Makefile`s to `Doc`s. Very standard. However, it would benefit from having its own options/choices. For example, `.PHONY` is commonly placed above each 'abstract' rule definition. We use the single-list-style, but the additive-style is also common.
