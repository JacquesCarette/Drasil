This wiki page is for keeping track of progress on the Projectile example, as well as documenting the process of generating the artifacts.

# SRS

_Sam:_ I managed to create a [blank SRS template](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/Drasil/Template/Body.hs) that will be useful in the future for generating new documents/examples. I started a rough copy of the SSD in [this PR](https://github.com/JacquesCarette/Drasil/pull/1405).

The following four "chunks of information" can reference each other according to this graph:

![chunkRefs](https://user-images.githubusercontent.com/35857611/58351831-e3b37200-7e37-11e9-88d2-64003f50f7c6.png)

## Goal Statements
The following had to be added to the second argument of `symbMap` to generate an empty goal statements section (including what's from A and TM sections):

```haskell
nw program : nw goalStmt : map nw [input_, output_, system] 
```

## Assumptions
The following had to be added to the second argument of `symbMap` to generate an empty assumptions section:
```haskell
(nw projectile : map nw [information, physicalSystem, problemDescription,
    problem, section_, solutionCharacteristic, specification] ++
  map nw [assumption, dataDefn, genDefn, inModel, thModel])
```

## Theoretical Models
The following had to be added to the second argument of `symbMap` to generate an empty theoretical model section (including what was added from the assumption section):
```haskell
(nw equation : nw general)
```
When adding the TMs, the list of concepts (ie. acceleration) for them had to be added to the first argument (with `qw`) and the second (with `nw`).

"[Theoretical models] are equations that are just true. They aren't derived from anything else, but are usually the starting points for derivations of other equations." (Brooks MacLachlan) Based on this, the definitions of velocity and acceleration are the TMs.

## Instance Models
"[Instance models] are the final equations that give you your outputs." (MacLachlan) The outputs of Projectile are a `boolean` for if the target was hit, a `boolean` for if the target was short, and the offset distance from the target.

The following had to be added to the second argument of `symbMap` to generate an empty instance models section (including what's from A, DD, GD, GS, and TM sections):

```haskell
map nw [model, symbol_]
```

## General Definitions
"[General definitions] are intermediate definitions between the TMs and IMs." (MacLachlan) With guidance from Brooks, the 
equations for calculating displacement and the time are GDs. 

Nothing had to be added to the second argument of `symbMap` to generate an empty general definitions section from what was already included for the A, DD, GS, and TM sections.

## Data Definitions
"[Data definitions] are definitions for things that can be immediately calculated from the inputs, but aren't outputs themselves. We typically say they "support" the TMs, GDs, and IMs." (MacLachlan) The DDs are the equations for the x and y components of velocity, since they are trivial to calculate, but important for later in the process.

The following had to be added to the second argument of `symbMap` to generate an empty data definitions section (including what's from A, GS, and TM sections):

```haskell
nw datum
```

When adding the DDs, the list of concepts (ie. angle of projectile) for them had to be added to the first argument (with `qw`) and the second (with `nw`).

# Code

_Sam:_ I used the generated source code for GlassBR as a guide and made (working?) code for Projectile (found in the [src/](https://github.com/JacquesCarette/Drasil/tree/master/code/drasil-example/Drasil/Projectile/src) folder in Projectile) with (manual) test cases.