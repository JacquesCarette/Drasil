# Chunk Analysis

## Relevant Issues:
- https://github.com/JacquesCarette/Drasil/issues/2788
  - Started as a discussion of `mkQuantDef'`, but mostly goes on about UIDs/namespacing.
- https://github.com/JacquesCarette/Drasil/issues/2908
  - Underlying issue with making UIDs unique, there is code that assumes anything with a symbol is inserted as a DQD.
- https://github.com/JacquesCarette/Drasil/issues/4061
  - General discussion of `DefinedQuantityDict` and `QDefinition`
- https://github.com/JacquesCarette/Drasil/issues/4310
  - Raises the issue that "definition" is often not a definition.

## Quantity Analysis

### `IdeaDict` (`NamedIdea.hs`)
Haddock:
> 'IdeaDict' is the canonical dictionary associated to an 'Idea'.
  Contains a 'UID' and a term that could have an abbreviation ('Maybe' 'String').

Fields:
- `UID`
- Term: `NP`
- Abbreviation: `Maybe String`

Constructors have been 'upgraded' already:
- `idea :: UID -> NP -> String -> IdeaDict`
- `idea' :: UID -> NP -> IdeaDict`

### `ConceptChunk` (`Core.hs`)
Haddock (needs update):
> The ConceptChunk datatype records a concept that contains an idea ('IdeaDict'),
  a definition ('Sentence'), and an associated domain of knowledge (['UID']).

Fields:
- `UID`
- Term: `NP`
- Abbreviation: `Maybe String`
- Definition: `Sentence`
- Domains: `[UID]`

Constructors that have been 'upgraded' already:
- `cncpt :: Concept dom => UID -> NP -> Sentence -> String -> [dom] -> ConceptChunk`
- `cncpt' :: Concept dom => UID -> NP -> Sentence -> [dom] -> ConceptChunk`
- `cncpt'' :: UID -> NP -> Sentence -> String -> ConceptChunk`
- `cncpt''' :: UID -> NP -> Sentence -> ConceptChunk`

One 'bad' constructor remains:
- `cw :: Concept c => c -> ConceptChunk`
  - Used only to implement `dqdWr`

Commentary:
- `cncpt` and `cncpt'` are rarely used as the concept domain is rarely used. `cncpt''` and `cncpt'''` are far more common. Switch them around?

### `DefinedQuantityDict` (`DefinedQuantity.hs`)
Haddock:
>  DefinedQuantityDict is the combination of a 'Concept' and a 'Quantity'.
   Contains a 'ConceptChunk', a 'Symbol' dependent on 'Stage', a 'Space', and maybe a 'UnitDefn'.
   Used when we want to assign a quantity to a concept. Includes the space, symbol, and units for that quantity.

Fields:
- Concept: `ConceptChunk`
- Symbol: `Stage -> Symbol`
- Space: `Space`
- Unit: `Maybe UnitDefn`

Note: UID is inherited from `ConceptChunk`!

Constructors:
- `quant :: UID -> NP -> Sentence -> Symbol -> Space -> UnitDefn -> DefinedQuantityDict`
- `quant' :: UID -> NP -> Sentence -> (Stage -> Symbol) -> Space -> UnitDefn -> DefinedQuantityDict`
- `quantAU :: UID -> NP -> Sentence -> Maybe String -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> DefinedQuantityDict`
- `quantNoUnit :: UID -> NP -> Sentence -> Symbol -> Space -> DefinedQuantityDict`
- `quantNoUnit' :: UID -> NP -> Sentence -> (Stage -> Symbol) -> Space -> DefinedQuantityDict`
  - These 4 constructors are 'new', but ultimately went too far the wrong way.
- `dqd :: ConceptChunk -> Symbol -> Space -> UnitDefn -> DefinedQuantityDict`
- `dqdNoUnit :: ConceptChunk -> Symbol -> Space -> DefinedQuantityDict`
- `dqd' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> DefinedQuantityDict`
  - These 3 cause UID reuse
- `dqdWr :: (Quantity c, Concept c, MayHaveUnit c) => c -> DefinedQuantityDict`
  - Projects as `DefinedQuantityDict`.
- `implVar :: UID -> NP -> String -> Space -> Symbol -> DefinedQuantityDict`
- `implVar' :: UID -> NP -> Sentence -> Space -> Symbol -> DefinedQuantityDict`
- `implVarAU :: UID -> NP -> String -> Maybe String -> Space -> Symbol -> Maybe UnitDefn -> DefinedQuantityDict`
- `implVarAU' :: UID -> NP -> Sentence -> Maybe String -> Space -> Symbol -> Maybe UnitDefn -> DefinedQuantityDict`
  - Nested CC not inserted into CDB

Commentary:
- So many constructors.
  - Can we get rid or move `implVar`? It doesn't feel like it fits here
- Should the `UnitDefn` eventually be a `UIDRef`?

### `QDefinition e` (`Eq.hs`)
Note: no Haddock documentation

Fields:
- Quantity: `DefinedQuantityDict`
- Inputs: `[UID]`
- Expression: `e`

Note: UID is inherited from `DefinedQuantityDict`!

Constructors:
- `fromEqn :: String -> NP -> Sentence -> Symbol -> Space -> UnitDefn -> e -> QDefinition e`
- `fromEqn' :: String -> NP -> Sentence -> Symbol -> Space -> e -> QDefinition e`
- `fromEqnSt :: UID -> NP -> Sentence -> (Stage -> Symbol) -> Space -> UnitDefn -> e -> QDefinition e`
- `fromEqnSt' :: UID -> NP -> Sentence -> (Stage -> Symbol) -> Space -> e -> QDefinition e`
- `fromEqnSt'' :: String -> NP -> Sentence -> (Stage -> Symbol) -> Space -> e -> QDefinition e`
  - Internal DQD not inserted into CDB
- `mkQuantDef :: (Quantity c, MayHaveUnit c, Concept c) => c -> e -> QDefinition e`
  - Reuses UID of passed chunk, recreates the whole inner chunk
- `mkQuantDef' :: (Quantity c, MayHaveUnit c) => c -> NP -> e -> QDefinition e`
  - Overrides term and definition, reuses UID, see #2788
- `ec :: (Quantity c, MayHaveUnit c) => c -> e -> QDefinition e`
- `mkFuncDef0 :: (IsChunk f, HasSymbol f, HasSpace f, IsChunk i, HasSymbol i, HasSpace i) => f -> NP -> Sentence -> Maybe UnitDefn -> [i] -> e -> QDefinition e`
- `mkFuncDef :: forall f i e. (IsChunk f, HasSymbol f, HasSpace f, IsChunk i, HasSymbol i, HasSpace i) => f -> NP -> Sentence -> UnitDefn -> [i] -> e -> QDefinition e`
- `mkFuncDefByQ :: (Quantity c, MayHaveUnit c, HasSpace c, Quantity i, HasSpace i) => c -> [i] -> e -> QDefinition e`
  - UID reuse with overridden term and definition

Usages:
- `fromEqn`
  - Only in HGHC
- `fromEqn'`
  - Only in `drasil-data` for Newton's Second Law
- `fromEqnSt`
  - Unused
- `fromEqnSt'`
  - Only in SSP. Used to override term and definition
- `fromEqnSt''`
  - Only in SWHS.
- `mkQDefSt`
  - Only in MultiDefn `multiDefnGenQD`
- `mkQuantDef`
  - All over the place
- `mkQuantDef'`
  - All over the place (but slightly less)
- `ec`
  - Just SSP
- `mkFuncDef`
  - Unused
- `mkFuncDef'`
  - Unused
- `mkFuncDefByQ`
  - Multiple examples

Commentary:
- Do we need the type parameter `e`? The type parameter makes inserting these into the database tricky.
- Once again, too many constructors
- Relationship between `QDefintion`, `MultiDefn` and `DataDefinition`
  - Do all three need to exist?

### `ConstrConcept` (`Constrained.hs`)
Haddock:
> ConstrConcepts are conceptual symbolic quantities ('DefinedQuantityDict')
  with 'Constraint's and maybe a reasonable value (no units!).
  Similar to 'ConstrainedChunk' but includes a definition and domain.

Fields:
- Quantity: `DefinedQuantityDict`
- Constraints: `[ConstraintE]`
- Reasonable Value: `Maybe Expr`
- Rationale: `Maybe Sentence`

Note: UID is inherited from `DefinedQuantityDict`!

Constructors:
- `constrained' :: (Concept c, MayHaveUnit c, Quantity c) => c -> [ConstraintE] -> Expr -> ConstrConcept`
- `constrainedNRV' :: (Concept c, MayHaveUnit c, Quantity c) => c -> [ConstraintE] -> ConstrConcept`
- `constrainedWithRationale :: (Concept c, MayHaveUnit c, Quantity c) => c -> [ConstraintE] -> Expr -> Sentence -> ConstrConcept`
  - All use `dqdWr` 
- `cuc' :: String -> NP -> String -> Symbol -> UnitDefn -> Space -> [ConstraintE] -> Expr -> ConstrConcept`
- `cucNoUnit' :: String -> NP -> String -> Symbol -> Space -> [ConstraintE] -> Expr -> ConstrConcept`
- `cuc'' :: String -> NP -> String -> (Stage -> Symbol) -> UnitDefn -> Space -> [ConstraintE] -> Expr -> ConstrConcept`
  - Internal DQD not in CDB
- `cnstrw' :: (Quantity c, Concept c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> ConstrConcept`
  - Uses `dqdWr`, used for projecting `UncertQ` as `ConstrainedChunk`

Field Usages:
- Constraints:
  - SRS Generator (`SpecificSystemDescription` `fmtPhys` and `fmtSfwr`)
  - `drasil-code` `ConstraintMap`, ultimately used in `genInputConstraints` and `genInputConstraintsProc`
- Reasonable Value/Rationale: Only used in SRS generator (`SpecificSystemDescription` `inDataConstTbl`)

Constructor Usages:
- `constrained'`
  - Multiple examples
- `constrainedNRV'`
  - Projectile, glassbr, SSP
- `constrainedWithRationale`
  - BSS
- `cuc'`
  - Multiple examples
- `cucNoUnit'`
  - GlassBR
- `cuc''`
  - SWHS
- `cnstrw'`
  - GlassBR, SSP, SWHS, SWHSNoPCM

Commentary:
- What if we want a reasonable value without constraints? Having a `ConstrConcept` with no constraints is a bit odd.
Related: https://github.com/JacquesCarette/Drasil/issues/1524

### `UncertQ` (`UncertainQuantity.hs`)
Haddock:
> UncertQs are conceptual symbolic quantities with constraints and an 'Uncertainty'.
  Contains a 'ConstrConcept' and an 'Uncertainty'.

Fields:
- Quantity: `ConstrConcept`
- Uncertainty: `Uncertainty`

Note: UID is inherited from `ConstrConcept`!

Constructors:
- `uq :: (Quantity c, Constrained c, Concept c, HasReasVal c, MayHaveUnit c) => c -> Uncertainty -> UncertQ`
  - Uses `dqdWr`
- `uqc :: String -> NP -> String -> Symbol -> UnitDefn -> Space -> [ConstraintE] -> Expr -> Uncertainty -> UncertQ`
- `uqcND :: String -> NP -> Symbol -> UnitDefn -> Space -> [ConstraintE] -> Expr -> Uncertainty -> UncertQ`
  - Inner `ConstrConcept` not inserted into CDB
- `uqDirect :: ConstrConcept -> Uncertainty -> UncertQ`
  - Reuses UID

Field Usages:
- Uncertainty: Only in SRS (`SpecificSystemDescription` `inDataConstTbl`)

Constructor Usages:
- `uq`
  - Multiple examples
- `uqc`
  - Multiple examples
- `uqcND`
  - GlassBR
- `uqDirect`
  - BSS

Commentary:
- Nested under `ConstrConcept`, but a `ConstrConcept` without any constraints could be considered 'nonsense'.
What if we want uncertainty without constraints?

### `DataDefinition` (`DataDefinition.hs`)
Haddock:
> A data definition is a 'QDefinition' that may have additional notes:
  the scope, any references (as 'DecRef's), maybe a derivation, a label ('ShortName'), a reference address, and other notes ('Sentence's).

Fields:
- Definition: Either a `SimpleQDef` or a `ModelQDef`
- UID
- References: `[DecRef]`
- Derivation: `Maybe Derivation`
- Short name: `ShortName`
- Reference Address: `String`
- Notes: `[Sentence]`

### `MultiDefn e` (`MultiDefn.hs`)
Haddock:
> 'MultiDefn's are QDefinition factories, used for showing one or more ways
  we can define a QDefinition.

Fields:
- UID
- Quantity: `DefinedQuantityDict`
- Description: `Sentence`
- Definitions: `[DefiningExpr e]`

### `CodeChunk` (`CodeVar.hs`)
Haddock:
> Basic chunk representation in the code generation context.
  Contains a DefinedQuantityDict and the kind of code (variable or function).

Fields:
- Quantity: `DefinedQuantityDict`
- Kind: `VarOrFunc`

Note: UID is inherited from `DefinedQuantityDict`!

### `CodeVarChunk` (`CodeVar.hs`)
Haddock:
> Chunk representing a variable.

Fields:
- Code chunk: `CodeChunk`
- Owning object: `Maybe CodeChunk` 

Note: UID is inherited from `CodeChunk`!

### `CodeFuncChunk` (`CodeVar.hs`)
Haddock:
> Chunk representing a function.

Fields:
- Code chunk: `CodeChunk`

Note: UID is inherited from `CodeChunk`!

### `ConceptInstance` (`Core.hs`)
Haddock:
> Contains a 'ConceptChunk', reference address, and a 'ShortName'.
  It is a concept that can be referred to, or rather, a instance of where a concept is applied.
  Often used in Goal Statements, Assumptions, Requirements, etc.

Fields:
- UID
- Concept: `ConceptChunk`
- Reference address: `String`
- Short name: `ShortName`

Constructors:
- `cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance`
  - Takes a single `Concept` to use in concept domain list, definitely a hack

Related methods:
- `sDom` collapses domain list down to a single UID, errors if multiple

Field usages:

Constructor usages:
- cic
  - used across examples to construct assumptions/FRs/NFS/etc

Commentary:
- Should each 'domain' be split to a separate chunk type?
