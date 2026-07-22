# `drasil-gen` Shallow File Analysis

## Summary

At the highest level, this package should be stripped down to only `WriteSystem.hs` and `ChunkDump.hs`. Everything else is more appropriately placed in other packages.

After breaking this package up, we have issues related to how this code works, but that will be explored _later_. In particular, capturing ICO problems meaningfully, hopefully avoiding the `drasil-srs` import in `drasil-code` (which would occur after moving the `drasil-code`-based code generator into `drasil-code`).

## Files

### `drasil-gen/lib/Drasil/Generator.hs`

Reexport file, exporting exclusively:

```haskell
Drasil.Generator.CaseStudyVariants
Drasil.Generator.Code
Drasil.Generator.CommonKnowledge
Drasil.Generator.Formats
Drasil.Generator.WriteSystem
```

### `drasil-gen/lib/Drasil/Generator/Formats.hs`

```haskell
-- | When choosing your document, you must specify the filename for
-- the generated output (specified /without/ a file extension).
type Filename = String

-- | Possible formats for printer output.
data Format = TeX | HTML | Jupyter | MDBook

instance Show Format where
  show TeX     = "PDF"
  show HTML    = "HTML"
  show Jupyter = "Jupyter"
  show MDBook  = "mdBook"
```

Contains the above data types.

* [ ] We have/had many `Filename` types. We currently only have another one in `drasil-printers`. This one is exclusively used in `drasil-gen/lib/Drasil/Generator/SRS.hs` for the type signature of `teXMakefile :: Filename -> FileLayout`. I think we should just remove it in favour of a `String` with good documentation.
* [ ] The `Format` type is abused in `drasil-website` to format things depending on how particular artifacts were generated. `Format` is otherwise used as an option in the generation options for a `SmithEtAlSRS` generator run. However, it completely lacks option-specific information. This is an active project in [#4989](https://github.com/JacquesCarette/Drasil/issues/4989) where we are actively trying to rewrite the entirety of `drasil-printers` to unlock more generation options.

### `drasil-gen/lib/Drasil/Generator/ChunkDump.hs`

Contains two functions:

```haskell
-- | Internal: For system debugging purposes, dump everything we can to a set of
-- files.
buildDebuggingFiles :: HasSystemMeta sys => sys -> [FileLayout]

-- | Internal: Build a JSON file from arbitrary data.
dumpTo :: ToJSON a => PathSegment -> a -> FileLayout
```

`buildDebuggingFiles` is the only one exposed by this module and it is used exclusively by `drasil-gen/lib/Drasil/Generator/WriteSystem.hs` when writing files generated for a 'system' to disk. [Very recently rewritten](https://github.com/JacquesCarette/Drasil/pull/5289).

Extension ideas:

1. [ ] Re.: generated files: a [`.tgf` file generator](https://en.wikipedia.org/wiki/Trivial_Graph_Format) for generating a chunk dependency graph.
2. [ ] Re.: generated files: a chunk type dependency graph.
3. [ ] Re.: individual systems: a new typeclass that lets them expose more kinds of debugging data, specialized to their usecase. This is not strictly a priority right now. The previous two would be usable _visualizations_.

### `drasil-gen/lib/Drasil/Generator/SRS/TypeCheck.hs`

Contains the code that _runs_ the type-checker:

```haskell
-- Note: this should be externally configurable wrt verbosity!
typeCheckSI :: SmithEtAlSRS -> IO ()
```

Issues:

1. It does not belong here.
2. It does not type-check everything it should.
3. It is operated too 'late', in the generation pipeline.

### `drasil-gen/lib/Drasil/Generator/SRS/TraceabilityGraphs.hs`

Contains a _renderer_ for the traceability graphs:

```haskell
-- | Creates a `FileLayout`s for the generated TraceyGraph directory.
outputDot :: GraphInfo -> FileLayout
```

* [ ] The encoding for the traceability graphs have moved to `drasil-metadata`, but they're not really _metadata_ (they're _document-level metadata_, not _drasil-concept-level metadata_). Therefore, along with the encoding that is currently in `drasil-metadata`, this code should be moved to `drasil-srs`. Realistically, it is only usable in the context of the SRS as well because of how specialized it is to the SRS format.

### `drasil-gen/lib/Drasil/Generator/WriteSystem.hs`

[Very recently _written_](https://github.com/JacquesCarette/Drasil/pull/5289).

Contains:

1. Code for running a Drasil generator (fully concretizing a 'system') and writing the produced software artifacts to disk.
2. A default options set that makes explicit how Drasil's `Makefile` operates and expects:
    1. Files will be arranged.
    2. How files will be placed.
    3. The system locale.
    4. When and where debugging files should be written.

The TODOs discussion in `drasil-gen/lib/Drasil/Generator/ChunkDump.hs` is relevant here. I don't see anything else immediately to suggest.

### `drasil-gen/lib/Drasil/Generator/SRS.hs`

Contains the "software-dossier"-like things related to generating generic document artifacts.

This code does not belong here. It's unclear _where_ it belongs, but for now, `drasil-srs` is a best guess. It is likely that we need another package that supports generating the auxiliary software artifacts to supplement general software projects.

### `drasil-gen/lib/Drasil/Generator/CommonKnowledge.hs`

```haskell
-- | Create a `ChunkDB` containing background knowledge common to all of
-- Drasil's existing case studies. This means knowledge related to the
-- SmithEtAl-esque SRS, mathematics, physics, general science, basic software,
-- and general documentation.
withCommonKnowledge :: [Reference] -> [DefinedQuantityDict] -> [IdeaDict] -> [CI] ->
    [ConceptChunk] -> [UnitDefn] -> [DataDefinition] -> [InstanceModel] ->
    [GenDefn] -> [TheoryModel] -> [ConceptInstance] -> [Citation] ->
    [LabelledContent] -> ChunkDB
withCommonKnowledge = insertAllOutOfOrder13 basisCDB

-- | The 'basis' chunk database to all of Drasil's case studies, containing
-- common background knowledge, including that related to the SRS, mathematics,
-- physics, general science, basic software, and general documentation.
basisCDB :: ChunkDB
basisCDB =
    insertAll basisReferences
  $ insertAll siUnits
  $ insertAll basisConceptChunks
  $ insertAll basisSymbols
  $ insertAll basisCIs
  $ insertAll basisIdeaDicts
  $ insertAll basisCitations
    empty
```

The above code is essentially what this file is. This file provides _each_ of our Drasil case studies (website, lesson plan, and the ICO software problems) with basic background information. This is extremely good. 

This code makes a few key assumptions:

1. That undergraduate-level mathematics and physics knowledge is all the background knowledge we are aware of.
2. That "code" is somehow relevant to each Drasil generator (see below code snippet).

```haskell
basisSymbols :: [DefinedQuantityDict]
basisSymbols =
  -- | DefinedQuantityDicts
  --  * codeDQDs - A list of DefinedQuantityDicts that are used for general
  --               code generation in all case studies
  codeDQDs
```

The above two assumptions are not explicitly mentioned nor captured anywhere. I'm not sure exactly how we capture this yet other than at the level of the Haskell code comments, but we should at least:

* [ ] Chop up the background knowledge gathered into 5 groups:
    * [ ] Knowledge necessary for the SRS.
    * [ ] Knowledge necessary for the code generator (standalone).
    * [ ] Knowledge necessary for the code generator (as part of the SRS generator).
    * [ ] Knowledge necessary for the lesson plan generator.
    * [ ] Knowledge necessary for the Drasil website generator.
* [ ] _Try_ to chop up each pool into domains and see how they domains are related.
* [ ] Restrict each case study to only its minimum necessary _domains_ (not _individual pieces of knowledge_!).
* [ ] _After chopping up the code into groups_: Moving the code elsewhere (`drasil-data`, `drasil-srs`, etc. as appropriate).

### `drasil-gen/lib/Drasil/Generator/Code.hs`

Contains the top-level code generator. Best discussed after the work mentioned at the bottom of this document.

### `drasil-gen/lib/Drasil/Generator/CaseStudyVariants.hs`

Contains 3 kinds of software generators:

```haskell
-- | A case study that only outputs an SRS in each of our supported variants.
caseStudyMainSRS :: SmithEtAlSRS -> SRSDecl -> String -> IO ()

-- | A case study that outputs both an SRS in each of our supported variants as
-- well as a single chosen software artifact in optionally many programming
-- languages.
caseStudyMainSRSWCode :: SmithEtAlSRS -> SRSDecl -> String -> Choices -> IO ()

-- | The same as 'caseStudyMainSRSWCode', except it also produces a
-- JupyterNotebook-based lesson plan.
caseStudyMainSRSWCodeZoo :: SmithEtAlSRS -> SRSDecl -> String -> [Choices] -> IO ()
```

And two "helper" functions:

```haskell
-- | Internal: The `build/` subfolder the Makefile expects each case study will
-- build in (other than the website).
caseStudyBuildFolder :: SmithEtAlSRS -> String

-- | Internal: Generate documents and construct the SRS directory layout
-- structure (and debug data) for an example.
writeSmithEtAlSrs :: SmithEtAlSRS -> SRSDecl -> String -> IO ([FileLayout], SmithEtAlSRS, String)
```

Beginning with the two "helper" functions:

* [ ] `caseStudyBuildFolder` contains important knowledge about the name of the folder to be generated for the software artifacts that the above 3 software generators produce. We reproduce this with `SystemMeta` recently in [#5201](https://github.com/JacquesCarette/Drasil/pull/5201).
* [ ] `writeSmithEtAlSrs` is _ripe_ for rewriting in a similar fashion to [#5201](https://github.com/JacquesCarette/Drasil/pull/5201). From a preliminary scan on what this would look like, a very important question reveals itself: Can we scan generated documents for the list of `Reference`s that _would_ be referrable through the generator? This would further clean up work in [#5302](https://github.com/JacquesCarette/Drasil/pull/5302) and would simplify [`fillReferences`](https://github.com/JacquesCarette/Drasil/blob/49ef10a79f9db8b7b8d84e72fbcb718d01f267c2/code/drasil-srs/lib/Drasil/SRS/DocumentLanguage.hs#L142-L163).
* [ ] On `Reference`s: We really need to spend some time to rearchitect this general area of code. We overload what "reference"s means too heavily.

The 3 software generators need to be rewritten using [`ToFiles`](https://github.com/JacquesCarette/Drasil/pull/5201) and moved to their respective packages. For example, `caseStudyMainSRS` should be moved to `drasil-srs`. For the other two, the answer is not as clear, but, a good "guest-guess" for now is `drasil-code`. However, this is almost definitely _not_ where the final resting place for this code should be. That is because `drasil-code` should involve exclusively things related to _code_. We should instead have a 3rd package that deals with ICO problems that: (1) `drasil-srs` imports and (2) `drasil-code` deals with. There's no point in speculating further than this about what we should be doing because this is _definitely_ wrong as it is, but it's a simple guess for now.

We should also recognize a few issues related to 3 generators:

* [ ] The SRS generator is also a kind of "zoo" generator; generating an SRS document in a host of formats.
* [ ] The Code generator (no -zoo) is also a kind of "zoo" generator; generating a codebase in a host of languages.
* [ ] The Code-zoo generator is closer to what the Code generator _should_ be doing. However, it follows a very different scheme than the rest (using a code system for generated folders rather than nesting them about chosen languages).
