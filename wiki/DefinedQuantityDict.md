I decided to tackle `DefinedQuantityDict` first, since that's where this issue started, and I noticed the following.

# Summary of `DefinedQuantityDict`
A `DefinedQuantityDict` is created by taking a `ConceptChunk` and adding the parts necessary to also make it a `Quantity`, which includes a symbol (that might depend on the stage), a space, and maybe a unit.

# "Issues" with DQD currently:
## `ConceptChunk`s made only to be used in a DQD
Sometimes, as in Projectile (see below), we create a `ConceptChunk` that is only used in a DQD. What is the rationale for this? Couldn't we just define the `ConceptChunk` locally in the constructor for the DQD, or is there some reason (e.g., to support future changes that might need just the `ConceptChunk` for something else) that we define the `ConceptChunk` separately? I was able to migrate this specific example to just using a local `ConceptChunk` and it worked without a problem, although the code got a bit harder to read (which might be a valid enough reason for this method of building DQDs). (The concept is defined [here](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/projectile/lib/Drasil/Projectile/Concepts.hs) and the `DefinedQuantityDict` is defined [here](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/projectile/lib/Drasil/Projectile/Unitals.hs)).

```haskell
-- Concepts.hs
launAngle :: ConceptChunk
launAngle = cc' launchAngleNC
  (foldlSent_ [phraseNP (the angle), S "between the", phrase launcher `S.and_` S "a straight line"
             `S.fromThe` phraseNP (launcher `toThe` target)])

-- Unitals.hs
import qualified Drasil.Projectile.Concepts as C (launAngle)

launAngle :: ConstrConcept
launAngle = constrained' (dqd' C.launAngle (autoStage lTheta) Real (Just radian)) [physc $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)] (sy pi_ $/ exactDbl 4)

```

## "Projections" to DQDs
There is a constructor `dqdWr`, that takes a chunk with all the required information for a DQD and creates a corresponding DQD. 
### Examples
It is called on the following chunks in the examples:
- In GamePhysics: `unitSymbs`, `inputConstraints`, and `outputConstraints`
- In NoPCM: `concepts`, `constrained`, `tempW`, `watE`, and `surface`
- In SSP: `coords`, `inputs`, `xMaxExtSlip`, `xMaxEtrSlip`, `xMinExtSlip`, `xMinEtrSlip`, `yMaxSlip`, `yMinSlip`, `outputs`, `units`, `unitless`, `inputsWUncrtn`, `inputsNoUncrtn`, `constrained`, and `surface`

Occurences from [`SSP.Unitals`](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/ssp/lib/Drasil/SSP/Unitals.hs) (with only relevant code included):
```haskell
symbols :: [DefinedQuantityDict]
symbols = map dqdWr inputs -- :: [DefinedQuantityDict]
  ++ map dqdWr outputs -- :: [ConstrConcept]
  ++ map dqdWr units -- :: [UnitalChunk]
  ++ map dqdWr unitless -- :: [DefinedQuantityDict]

inputs :: [DefinedQuantityDict]
inputs = map dqdWr inputsWUncrtn -- :: [UncertQ]
  ++ map dqdWr inputsNoUncrtn -- :: [DefinedQuantityDict]
```

**Issue:** Some of these values are already `DefinedQuantityDict`s, so calling `dqdWr` again is redundant.

**Issue:** The `dqdWr` function (and likely other related functions) creates a new `DefinedQuantityDict` instead of simply accessing the existing one within the passed object, if applicable (for example, `ConstrConcept`, `UncertQ`, and `UnitalChunk` all contain a DQD within them at some level). Is this undesired behaviour, or a more efficient way of doing this?

**Issue:** The quantities shown here should be built automatically, instead of having to be built manually here. Some investigation would need to be conducted to find out where this should be done (in `SystemInformation` maybe?).

In every one of these cases, its use is to combine quantities of different types into one list of `DefinedQuantityDict`s.

### `drasil-lang`
It is also used in `drasil-lang` in the following functions:
- In `Chunk.Constrained`: `constrained'`, `constrainedNRV'`, and `cnstrw'`
- In `Chunk.UncertainQuantity`: `uq`

Occurences in [`Chunk.Constrained`](https://github.com/JacquesCarette/Drasil/blob/f7eaea75e0bcf270040c9fa4af214cd467410ddd/code/drasil-lang/lib/Language/Drasil/Chunk/Constrained.hs#L115-L123):
```haskell
-- | Creates a 'ConstrConcept' with a quantitative concept, a list of 'Constraint's and an 'Expr'.
constrained' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [ConstraintE] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept (dqdWr q) cs (Just rv)

-- | Similar to 'constrained'', but defaults 'Maybe' 'Expr' to 'Nothing'.
constrainedNRV' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [ConstraintE] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept (dqdWr q) cs Nothing
```

All related functions involve creating a `DefinedQuantityDict` behind the scenes at some point, so this approach seems reasonable to me.

## Merging a `Quantity` that `MayHaveUnit` and a `ConceptChunk`

There is a constructor [`dqdQd`](https://github.com/JacquesCarette/Drasil/blob/09365d7d1f5dc9b54e84a8d64cb601184456ea0c/code/drasil-lang/lib/Language/Drasil/Chunk/DefinedQuantity.hs#L79-L81) that merges a quantity and a concept.
```haskell
-- | When we want to merge a quantity and a concept. This is suspicious.
dqdQd :: (Quantity c, MayHaveUnit c) => c -> ConceptChunk -> DefinedQuantityDict
dqdQd c cc = DQD cc (symbol c) (c ^. typ) (getUnit c)
```

This function is used in the following chain:

|This function... | ...is used by this one...| ...in this location.|
|---|---|---|
|`dqdQd`|`combine` and `combine'`| `SysInfo.GetChunk`|
|`combine` and `combine'`|`ccss`| `SysInfo.GetChunk`|
|`ccss`| `extractUnits`| `Drasil.DocumentLanguage`|
|`ccss` and `extractUnits`| `mkRefSec`| `Drasil.DocumentLanguage`|

Then, [`mkRefSec`](https://github.com/JacquesCarette/Drasil/blob/1ab7a8c2b875e8a7e6ff6bcc45e74c4e4bc38b32/code/drasil-docLang/lib/Drasil/DocumentLanguage.hs#L258-L284) generates the reference section from within `mkSections` which is used by `mkDoc`. I'm not sure what the better implementation would look like, but since the Table of Symbols needs a Symbol, a Description, and maybe a Unit, `DefinedQuantityDict` seems like the best chunk type to use; it's just "suspicious" that we build it by combining two entities.