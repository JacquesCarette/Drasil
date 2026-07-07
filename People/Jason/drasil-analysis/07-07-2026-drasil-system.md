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

* [ ] `_sysName` needs to be replaced with two things:
    * [ ] `_name :: ProgramName`, which contains the specific "program codenames" (e.g., `projectile`, `ssp`, etc.).
    * [ ] `_description :: Sentence`

* [ ] `makeClassy ''SystemMeta` is good, but we make use of the metadata about what type of system is being encoded as well in `drasil-lesson-plan` (where we use `notebook :: CI`) and in `drasil-srs` (where we use `srs :: CI`).

```haskell
class SystemKind sys where
  systemKind :: UIDRef ConceptChunk

type System sys = (SystemKind sys, HasSystemMeta sys)
```

This way we gain some more basic information about what 'kind' of system is being encoded as well. Really, `SystemKind` is a typeclass that could extend to anything that gives structured meaning to things (and would need to be appropriately renamed to something else). For example, another name might be `MetaConcept` or `HasMetaConcept`.

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

* [ ] Needs to be moved to `drasil-website`.

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

* [ ] Needs to be moved to `drasil-srs`.
* [X] Needs to be rebuilt, but that's out of scope in this document.

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
