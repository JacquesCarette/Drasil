# `drasil-system` Shallow File Analysis

5 files.

## `drasil-system/lib/Drasil/System.hs`

Reexport file, reexporting the latter 4 files.

## `drasil-system/lib/Drasil/System/Core.hs`

```haskell
-- | Project Example purpose.
type Purpose = [Sentence]
-- | Project Example background information, used in the 'What' section of
-- README.
type Background = [Sentence]
-- | Project Example scope.
type Scope = [Sentence]
-- | Project Example motivation.
type Motivation = [Sentence]

data SystemMeta = SystemMeta
  { _sysName    :: CI -- FIXME: This should not be a CI.
  , _authors    :: People
  , _purpose    :: Purpose
  , _background :: Background
  , _scope      :: Scope
  , _motivation :: Motivation
  , _systemdb   :: ChunkDB
  }

makeClassy ''SystemMeta

mkSystemMeta :: CI -> People -> Purpose -> Background -> Scope -> Motivation ->
  ChunkDB -> SystemMeta
mkSystemMeta = SystemMeta
```

`makeClassy ''SystemMeta` is good, but we make use of the metadata about what _type of system_ was encoded as well, for example in `drasil-lesson-plan` (where we use `notebook :: CI`) and in `drasil-srs` (where we use `srs :: CI`).

```haskell
class SystemKind sys where
  systemKind :: UIDRef ConceptChunk

type System sys = (SystemKind sys, HasSystemMeta sys)
```

This way we gain some more basic information about what 'kind' of system is being encoded as well. Really, `SystemKind` is a typeclass that could extend to anything that gives structured meaning to things (and would need to be appropriately renamed to something else). For example, another name might be `MetaConcept` or `HasMetaConcept`.

### TODOs

* [ ] Introduce `SystemKind` class to gain more basic information about what 'kind' of system is being encoded.
* [ ] Rename/generalize `SystemKind` to `MetaConcept` or `HasMetaConcept` and use for any/all chunk types.
* [ ] `_sysName` needs to be replaced with two things:
    * [ ] `_name :: ProgramName`, which contains the specific "program codenames" (e.g., `projectile`, `ssp`, etc.).
    * [ ] `_description :: Sentence`

## `drasil-system/lib/Drasil/System/DrasilWebsite.hs`

Contains:

```haskell
data DrasilWebsite = DW
  { _sm :: SystemMeta,
    _indexDoc :: Document,
    _webRefs :: M.Map UID Reference
  }

makeLenses ''DrasilWebsite
```

`DrasilWebsite` is a fairly 'low-level' encoding of the Drasil website, containing only a fully-written `Document` that will be rendered as the `index.html` page. For example, it can include:

1. The list of examples to be shown on the website.
2. A list of relevant papers to be shown on the website.

I'm sure there are other things that it can include as well.

`webRefs` is a (known) wart; a result of [#4862](https://github.com/JacquesCarette/Drasil/pull/4862) and [#4488](https://github.com/JacquesCarette/Drasil/pull/4488). It is to be dealt with partially in [#4688](https://github.com/JacquesCarette/Drasil/issues/4688), but we also need another ticket to investigate what's going on with `Reference`.

### TODOs

* [ ] Move to `drasil-website` -- [#5286](https://github.com/JacquesCarette/Drasil/pull/5286).
* [ ] Flesh out more. A 'higher-level' encoding of the Drasil website would be nice to see.
* [ ] Investigate how `Reference`s work.

## `drasil-system/lib/Drasil/System/SmithEtAlSRS.hs`

Contains the encoding of `SmithEtAlSRS`:

```haskell
-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data SmithEtAlSRS where
 ICO :: (Quantity h, MayHaveUnit h, Concept h,
  Quantity i, MayHaveUnit i, Concept i,
  HasUID j, Constrained j) =>
  { _meta         :: SystemMeta
  , _programName  :: String
  , _theoryModels :: [TheoryModel]
  , _genDefns     :: [GenDefn]
  , _dataDefns    :: [DataDefinition]
  , _instModels   :: [InstanceModel]
  , _inputs       :: NE.NonEmpty h
  , _outputs      :: NE.NonEmpty i
  , _constraints  :: [j]
  , _constants    :: [ConstQDef]
  -- FIXME: This is a list of all 'quantites' (variables) used/referenced in an
  -- SRS. Why is this here? For type-checking the SRS later. Should
  -- type-checking be done on the SRS level? No. This is a temporary hack.
  , _quantities   :: [DefinedQuantityDict]
  -- FIXME: This is a list of all labelled content required for the SRS to be
  -- generated. In particular, this is needed for the mdBook generator which
  -- _must_ export a CSV containing a list of all external resources that the
  -- mdBook compiler is allowed to access. This list should be re-written as
  -- part of a stateful renderer for the SRS instead.
  , _lbldCntnt    :: [LabelledContent]
  -- FIXME: Hacks to be removed once 'Reference's are rebuilt.
  , _refTable     :: M.Map UID Reference
  , _refbyTable   :: M.Map UID [UID]
  , _traceTable   :: M.Map UID [UID]
  } -> SmithEtAlSRS

makeClassy ''SmithEtAlSRS
```

### Parameters

Parameters can be grouped into 6 categories:

1. **System Metadata**:
   * `_meta :: SystemMeta`: Contains core metadata (name, authors, purpose, background, scope, motivation) and the central chunk database (`_systemdb`).
   * `_programName :: String`: The folder/abbreviation name of the system, derived from the `nm :: CI` name captured in `SystemMeta`; `_programName = filter (not . isSpace) $ abrv nm`
2. **Relevant Theories**:
   * `_theoryModels :: [TheoryModel]`
   * `_genDefns :: [GenDefn]`
   * `_dataDefns :: [DataDefinition]`
   * `_instModels :: [InstanceModel]`
3. **ICO Problem Capture**:
   * `_inputs :: NE.NonEmpty h`: Non-empty list of input variables (quantities with units and concepts).
   * `_outputs :: NE.NonEmpty i`: Non-empty list of output variables (quantities with units and concepts).
   * `_constraints :: [j]`: Constraints on variables.
   * `_constants :: [ConstQDef]`: Physical and mathematical constants.
4. **Display Entities**:
   * `_lbldCntnt :: [LabelledContent]`: Figures, tables, and generated traceability tables.
5. **Type-Checking Auxiliary**:
   * `_quantities :: [DefinedQuantityDict]`: A temporary list of all variables/quantities in the SRS. Used for type-checking mathematical expressions.
6. **Traceability Auxiliaries**:
   * `_refTable :: Map UID Reference`: A lookup map used to resolve hyperlinks and references.
   * `_refbyTable :: Map UID [UID]`: A reverse lookup table mapping a chunk UID to all chunks referencing it.
   * `_traceTable :: Map UID [UID]`: A traceability table mapping a chunk UID to all chunks it references.

#### System Metadata

The system metadata is used in two ways:

1. The `_programName` provides the _name_ for the generated project folders. This really ought to be a kind of option.
2. The `_meta`data provides information (name, authors, purpose, background, scope, motivation) that goes into the SRS, Makefiles, and READMEs.

#### Relevant Theories

The relevant theories are used for both SRS generation and code generation.

##### SRS Generation

* **Document _Expansion_**: The document layout declaration (`SRSDecl` / `DocDesc`) contains placeholder constructors like `TMs`, `GDs`, `DDs`, and `IMs`. During layout processing in `DocDecl.hs`, Drasil extracts the corresponding lists to populate the "Solution Characteristics Specification" and the "Specific System Description" sections.
* **Traceability Configuration**: The standard traceability configuration (specifically [`traceMatOtherReq` in `TraceabilityMandGs.hs`](https://github.com/JacquesCarette/Drasil/blob/b99972de32aa418d1eb2a47bb95f242f19804599/code/drasil-srs/lib/Drasil/SRS/Sections/TraceabilityMandGs.hs#L106-L119)) inspects the theory models list sizes to dynamically choose singular/plural headings (e.g., "Theory Model" vs "Theory Models").
* **Reference Map Gathering**: In `fillReferences` (in `DocumentLanguage.hs`), these theory lists are scanned for `Reference` targets so they are added to the system-wide `_refTable`.

##### Code Generation

* **Model Derivation**: In `CodeSpec.hs`, only `_dataDefns` and `_instModels` are currently used for code generation; `_theoryModels` and `_genDefns` are not used in code generation at all.
* **Calculation Path**: Formulas (in the form of `QDefinition`s) are extracted from `_dataDefns` and `_instModels`. These are used by `solveExecOrder` to determine a valid calculation path from inputs to outputs.
* **Derived Inputs**: `_dataDefns` are also checked to see if any inputs can be derived from other explicit inputs via `getDerivedInputs`.

#### ICO Problem Capture

##### SRS Generation

* **Not Directly Expanded in Document Generation (!)**: Unlike theories, the problem domain fields (`_inputs`, `_outputs`, `_constraints`, and `_constants`) are **not** queried from `SmithEtAlSRS` to build document content. Instead, the document layout declaration (`SRSDecl`) manually takes the constants list (passed to `AuxConsProg`) and constraints list (passed to the `Constraints` layout constructor) in the case study's body file.

##### Code Generation

* **Input/Output Spec**: `_inputs` and `_outputs` define the starting and target variables in `mkCodeSpec`.
* **Constraint Maps**: `_constraints` is mapped to a constraint map (`_cMap` using `constraintMap`) used to generate runtime checks on variables.
* **Constants Spec**: `_constants` is mapped to code definitions for constants (`_constDefns` and `_constMap`) that are used in calculation execution paths.

#### Display Entities

* **Reference Resolution**: `_lbldCntnt` stores the figures, tables, and dynamically-generated traceability tables. In `fillReferences` (in `DocumentLanguage.hs`), this list is scanned to populate the `_refTable` so we can build local hyperlinks in the output formats (HTML/TeX/mdBook) to resolve correctly to these entities.
* **Traceability Tables**: SVG-based traceability graphs generated by the compiler are appended to `_lbldCntnt` and inserted into the `ChunkDB` during document compilation. I'm not sure if they actually need to be added to this list. It might be completely unnecessary.

#### Type-Checking Auxiliaries

* **Typing Environment**: `_quantities` is a list of all variables/quantities used in the SRS. In `TypeCheck.hs` (`typeCheckSI`), it is used to build a local typing context and type-check expressions reachable from `_dataDefns` and `_instModels`.

#### Traceability Auxiliaries

* **References Map**: `_refTable` maps UIDs to their reference targets. Filled dynamically by `fillReferences` using references from sections, citations, and model chunks, allowing the final output renderers to resolve internal links.
* **Traceability Maps**: `_traceTable` (what a chunk references) and `_refbyTable` (what references a chunk) are computed from the document description. They are used to filter traceability matrices, and to build DOT graph node/edge configurations in `mkGraphInfo` for exporting SVG dependency graphs.
* **Debugging**: These tables are dumped to JSON files (`trace.json` and `reverse_trace.json`) via `buildDebugData` in `ChunkDump.hs` for compiler debugging.

#### Inter-Group Relationships

* **Traceability & Theories**: `_traceTable` and `_refbyTable` capture the cross-references and dependencies between the different theories/models/assumptions/etc. (`_theoryModels`, `_genDefns`, `_dataDefns`, `_instModels`).
* **References & Display**: `_refTable` aggregates reference locations from theories, sections, and display entities (`_lbldCntnt`), for building local document hyperlinks.
* **Problem Domain & Theories to Code Spec**: In code spec generation, `_inputs`, `_constants`, and `_outputs` define the starting state, boundary variables, and termination targets, while `_dataDefns` and `_instModels` specify the mathematical formulas used to find calculation paths between them.

### TODOs

* [ ] Needs to be moved to `drasil-srs` -- [#5287](https://github.com/JacquesCarette/Drasil/pull/5287).
* [ ] Add a link to the SmithEtAl paper.
* [ ] Create an option for the names of the folders we generate (the folders containing all artifact files).
* [ ] Deal with the "temporary" `_quantities` hack in `SmithEtAlSRS` (avoiding type-checking on the SRS level).
* [ ] Re-write `_lbldCntnt` as part of a stateful renderer for the SRS instead of holding it directly in `SmithEtAlSRS` (to properly handle mdBook CSV resource exports).
* [ ] Remove hacks (`_refTable`, `_refbyTable`, `_traceTable`) once `Reference`s are rebuilt.
* [ ] Check if traceability graphs actually need to be added to `_lbldCntnt` during compilation.

## `drasil-system/lib/Drasil/System/Transformations.hs`

Contains a typeclass (`ToFiles`) that effectively captures one software generator, referenced by its supported option set.

```haskell
-- | The goal of our systems is to be abstractions about human-readable software
-- artifacts. An instance of this typeclass ('ToFiles') defines a software
-- generator that explains how said abstractions can be made /fully concrete/
-- (i.e., made into concrete software artifacts).
class (HasSystemMeta sys) => ToFiles sys opts | opts -> sys where
  toFiles ::
    -- | The system.
    sys ->
    -- | The generation options.
    opts ->
    -- | The final, rendered software artifacts.
    [FileLayout]
```

`ToFiles` is currently only instantiated for `LessonPlan` and `DrasilWebsite`:

* https://github.com/JacquesCarette/Drasil/blob/b99972de32aa418d1eb2a47bb95f242f19804599/code/drasil-system/lib/Drasil/System/DrasilWebsite.hs#L55-L69
* https://github.com/JacquesCarette/Drasil/blob/b99972de32aa418d1eb2a47bb95f242f19804599/code/drasil-lesson-plan/lib/Drasil/LessonPlan/JupyterGenerator.hs#L40-L59

`ToFiles` is exclusively used in `WriteSystem.hs`:

https://github.com/JacquesCarette/Drasil/blob/b99972de32aa418d1eb2a47bb95f242f19804599/code/drasil-gen/lib/Drasil/Generator/WriteSystem.hs#L56-L72

`WriteSystem.hs` wraps together all assumptions the `code/Makefile` makes about the way artifacts are written to disk. This is useful because this code is reusable in each of our case studies.

One oddity about `WriteSystem` is that it uses the `CI` from the `SystemMeta`-information that each "system" is expected to instantiate. Each "system" is given a `CI` which contains its title and an abbreviation. This abbreviation is currently used as the name of the folder that Drasil generates. This is a known issue; [#4884](https://github.com/JacquesCarette/Drasil/issues/4884).

### TODOs

* [ ] Instantiate `ToFiles` for our remaining two case study variants:
    * [ ] `SmithEtAlSRS`
    * [ ] `CodeSpec`
* [ ] Deal with [#4884](https://github.com/JacquesCarette/Drasil/issues/4884).
