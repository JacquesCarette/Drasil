## Introduction

This investigation was started to analyze the uses of the abbreviation functions within Drasil. The functions that were analyzed as a part of this investigation and their brief summaries are as follows:
* [`getAcc`](https://github.com/JacquesCarette/Drasil/wiki/Investigation-of-Abbreviation-Functions#getacc)
  * It was used in 52 different places and can easily be replaced with `S . abrv`. A discussion as to whether or not this action should be taken is ongoing and the latest findings can be viewed through [this comment](https://github.com/JacquesCarette/Drasil/issues/3472#issuecomment-1675229741).

* [`getAccStr`](https://github.com/JacquesCarette/Drasil/wiki/Investigation-of-Abbreviation-Functions#getaccstr)
  * It was defined to be equal to `abrv` and hence performed the same functionality. Therefore, it was removed. The issue pertaining to this is [#3623](https://github.com/JacquesCarette/Drasil/issues/3623).
* `getA` -> No Change suggested.
* [`abrv`](https://github.com/JacquesCarette/Drasil/wiki/Investigation-of-Abbreviation-Functions#abrv)
  * The `abrv` function was used in 40 different places. It was misused in 19 of them and was replaced by `programName`. For the latest discussion on the remaining usages of the `abrv` function, visit [this comment](https://github.com/JacquesCarette/Drasil/issues/3472#issuecomment-1677597843).
* [`prependAbrv`](https://github.com/JacquesCarette/Drasil/wiki/Investigation-of-Abbreviation-Functions#prependabrv)
  * `prependAbrv` was used 12 different times. It was mostly used for the creation of the various `DataDefinition`, `GenDefn`, `InstanceModel`, and `TheoryModel`. The issue relating to the design modifications of `prependAbrv` is [#3626](https://github.com/JacquesCarette/Drasil/issues/3626).

NOTE: A much more detailed discussion is present at [#3472](https://github.com/JacquesCarette/Drasil/issues/3472).

## Methods used:

![Screenshot 2023-08-04 150740](https://github.com/JacquesCarette/Drasil/assets/110783820/3dea7860-baee-4730-bb00-2a1bdd112690)

## Findings:

### `getAcc`

`getAcc` was used in 52 different places.

It is defined as the following:

```Haskell
-- | Get abbreviation in 'Sentence' form from a 'CI'.
getAcc :: CI -> Sentence
getAcc = S . abrv
```

This can easily be removed and can be replaced with `S . abrv` in each of its occurrences, however, since it is used in 52 different instances, the suggestion to keep it was made.

***

### `getAccStr`

`getAccStr` is only used once and is defined as the following:

```Haskell
-- | Get abbreviation in 'String' form from a 'CI'.
getAccStr :: CI -> String
getAccStr = abrv
```

This function should be removed and its use must be replaced with `abrv` as it performs the same functionality as `abrv`.

***

### `abrv`

The `abrv` function was used in 40 different places. 

It is defined as the following:

```Haskell
-- | CommonIdea is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: c -> String
```

In 19 different instances, the functionality of `abrv` was misused and the function was replaced by `programName` to improve traceability and readability.

A lot of the usage of `abrv` is to obtain the abbreviation of `sys`. `sys` is just referring to the name of the examples. An alternate function that we could use in many of these cases is `programName` which would convert the code from `abrv` sys to `programName`  sys. This would allow us to obtain the original program name of these examples. However, the reason why `abrv` is used as opposed to `programName` is because of the examples of Game Physics and PD Controller. If we use `programName` instead of `abrv`, the names of these examples get converted from `pdcontroller` to `pd_controller` and `gamephysics` to `game_physics`, or `PD Controller` to `PD_Controller` and `Game Physics` to `Game_Physics` depending on the circumstance.

For some of the examples, we may prefer to keep `abrv` because it is used on the main website of Drasil and it may look better to avoid the underscore rather than keeping it. However, for other instances, we should consider moving towards using `programName` instead of `abrv` as I believe that it would be more representative of the function that we are trying to carry out. Alternatively, we could also look into modifying the way the project names are defined and modify those so that for the program name, we do not have any underscores and for the abbreviation, we truly do have an abbreviation. The reason I make this comment is because for all examples other than PD Controller and Game Physics, the `programName` is the exact same as the abbreviation for these examples, which in my opinion defeats the purpose of `programName` and `abrv` for being two separate functions.

For a much more detailed discussion, visit [here](https://github.com/JacquesCarette/Drasil/issues/3472#issuecomment-1638883175)

***

### `prependAbrv`

`prependAbrv` is used in 12 different instances. It is defined as the following:

```Haskell
-- | Prepends the abbreviation from a 'CommonIdea' to a 'String'.
prependAbrv :: CommonIdea c => c -> String -> String
prependAbrv c s = abrv c ++ (':' : repUnd s)
```

All of the cases of `prependAbrv` occur in the following four files: `DataDefintion.hs`, `GenDefn.hs`, `InstanceModel.hs`, and `Theory.hs`.

In all cases, `prependAbrv` is used to create labels for the Data Definitions, General Definitions, Instance Models, and Theory Models elements. The functions in which they are used to create the said Data Definitions, General Definitions, Instance and Theory Models in the examples. 

An example of a place where `prependAbrv` is used is the following: 

```Haskell
-- | Smart constructor for general definitions with no references.
gdNoRefs :: IsUnit u => ModelKind ModelExpr -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs mkind u derivs sn_ = 
  GD mkind (fmap unitWrapper u) derivs [] (shortname' $ S sn_) (prependAbrv genDefn sn_)
```

Usage of the function in which `prependAbrv` is used in an example is the following:

```Haskell
velXGD_1 :: GenDefn
velXGD_1 = gdNoRefs (equationalModel' velXQD_1) (getUnit velocity) (Just velXDeriv_1) "velocityX1" [{-Notes-}]
```

`sn_` is just a `String` that takes in the name of the General Definition element. If `sn_` is replaced with any other string, then the output on the SRS documents will reflect that. However, that would only impact `RefName`. It would not impact the `RefBy` sections. This is because we obtain the `RefBy` by obtaining the `UID` from within the `RefBy` table and then obtain the reference from there.

The following image showcases the effect of when I change `sn_` to `"Hello"`. 

![image](https://github.com/JacquesCarette/Drasil/assets/110783820/5a4b0850-769d-4b87-a1f6-097d30d175c6)

The above refers to `GD:velocityX1`.

However, when this appears in `RefBy`, it appears as the following: 

![image](https://github.com/JacquesCarette/Drasil/assets/110783820/c4ef47ca-557c-4b79-a1d0-487732bddfbc)

The following code block showcases how we obtain the references from the `RefBy`:

```Haskell
-- | Translates a traceability map into a reference map.
generateRefbyMap :: TraceMap -> RefbyMap
generateRefbyMap = invert

-- | Trace a 'UID' to referenced 'UID's.
refbyLookup :: UID -> RefbyMap -> [UID]
refbyLookup c = fromMaybe [] . Map.lookup c
```

The way references are formed is as follows:

```Haskell
-- | Finds the reference address of the 'GenDefn'.
instance HasRefAddress      GenDefn where getRefAdd l = RP (prepend $ abrv l) (view ra l)
-- | Finds the units of the 'GenDefn'.
instance HasAdditionalNotes GenDefn where getNotes    = notes
-- | Finds the units of the 'GenDefn'.
instance MayHaveUnit        GenDefn where getUnit     = gdUnit
-- | Finds the idea of a 'GenDefn' (abbreviation).
instance CommonIdea         GenDefn where abrv _      = abrv genDefn
-- | Finds the reference address of a 'GenDefn'.
instance Referable          GenDefn where
  refAdd      = view ra
  renderRef l = RP (prepend $ abrv l) (refAdd l)
```

For forming references, it uses the `getRefAdd` function which first obtains the abbreviation and then uses the `prepend` function to prepend that abbreviation to its name which is obtained by `view ra l`. 

The `prepend` is defined as the following:

```Haskell
-- | Prepends a 'String' to an 'IRefProg'.
prepend :: String -> IRefProg
prepend s = RS s +::+ RS ":" +::+ Name
```

Since the abbreviation of any General Definition element is `GD`, `GD` is prepended to the name. Thus, `RefBy` is not affected by this. `Refname` is affected because it is just a title and not a referable link. 

Similar instances have been defined for Data Definitions, Instance Models and Theory Models as well. Similar to General Definitions, all elements of Data Definitions, Instance Models, and Theory Models have the abbreviation of DD, IM, and TM respectively. 

For further information, visit [this comment](https://github.com/JacquesCarette/Drasil/issues/3472#issuecomment-1659138312).

 For @JacquesCarette 's analysis, visit [this comment](https://github.com/JacquesCarette/Drasil/issues/3472#issuecomment-1664255121)

