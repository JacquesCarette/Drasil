## Current Capture (As of 69b96d)

In this section, we will dissect all chunk-like types associated with the following code:

```haskell
-- * Newton's Second Law of Rotational Motion

newtonSLR :: TheoryModel
newtonSLR = tm (equationalModelU "newtonSLR" newtonSLRQD) [dRef hibbeler2004]
  "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRQD :: ModelQDef
newtonSLRQD = mkQuantDef' QP.torque (nounPhraseSP "Newton's second law for rotational motion") newtonSLRExpr

newtonSLRExpr :: ExprC r => r
newtonSLRExpr = sy QP.momentOfInertia $* sy QP.angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = [foldlSent
  [S "The net", getTandS QP.torque, S "on a", phrase rigidBody `S.is`
   S "proportional to its", getTandS QP.angularAccel `sC` S "where",
   ch QP.momentOfInertia, S "denotes", phrase QP.momentOfInertia `S.the_ofThe`
   phrase rigidBody, S "as the", phrase constant `S.of_` S "proportionality"]]
```

More precisely, we will focus on discussing things exposed through classy lenses.

### Visualized

**Legend**:

1. Subgraphs (boxes) indicate that all nodes contained within share the same `UID`.
2. Edge labels of dashed arrows describe their meaning. Meaningless without.
3. Plain arrows mean "contains."

```mermaid
flowchart TD
    subgraph UID_SLR ["UID: 'newtonSLR'"]
        TM["TheoryModel: newtonSLR"]
        MK["ModelKind"]
        TM --> MK
    end

    subgraph UID_Torque ["UID: 'torque'"]
        subgraph In_Theory ["Theory (Unregistered)"]
            MKs["ModelKinds: EquationalModel"]
            QD["QDefinition: newtonSLRQD"]
            DQD1["DQD 1 (Internal)<br/>term: 'Newton's second law...'"]
            MKs --> QD --> DQD1
        end

        subgraph In_Data ["drasil-data (Registered)"]
            CP["ConceptChunk: CP.torque"]
            DQD2["DQD 2: QP.torque<br/>term: 'torque'"]
            CP --> DQD2
        end
    end

    MK --> MKs
    QD -.->|inherits UID| DQD2

    CDB[("ChunkDB")]
    TM -.->|registered| CDB
    DQD2 -.->|registered| CDB
    
    Call["phrase newtonSLRQD"] -.->|Carries ref. to 'torque' UID| QD
    Call -.->|refers to|QD
    CDB -.->|resolves 'torque' to|DQD2
```

### 1. A `TheoryModel`

```haskell
newtonSLR :: TheoryModel
newtonSLR = tm (equationalModelU "newtonSLR" newtonSLRQD) [dRef hibbeler2004]
  "NewtonSecLawRotMot" newtonSLRNotes
```

The `TheoryModel` exposes the following:

* A `UID` (from inner `ModelKind`): "newtonSLR"
* A `term` (from inner `ModelKind`): Newton's second law for rotational motion
* A `Maybe` abbreviation (from inner `ModelKind`) via `Idea`: `Nothing`
* A human-readable definition (from inner `ModelKind`): `EmptyS`
* A list of decorated references: `hibbeler2004`
* A list of 'notes' (see [Notes](#notes-sentence))
* A "shortname": "NewtonSecLawRotMot"
* A reference address: "TM:NewtonSecLawRotMot"
* Another abbreviation (different from the earlier one!) via `CommonIdea`: "TM"
* An axiomatization of its body (from inner `ModelKind`): $\symbf{τ}=\symbf{I}α$
* Duplicate reference address exposure: both `HasRefAddress` (`getRefAdd`) and `Referable` (`renderRef`) yield `"TM:NewtonSecLawRotMot"`

### 2. A `ModelKind`

Note: `ModelKind` is a wrapper around `ModelKinds` that inherits everything, but permits overriding the `UID` and the `term`. The reason why we have _both_ `ModelKind` and `ModelKinds` was purely for operational reasons: to get Drasil working, without affecting `stable/`. Now that we want do things 'correctly,' this hack is at the front of the line. 

```haskell
equationalModelU "newtonSLR" newtonSLRQD
```

The above code is nested in `newtonSLR`'s definition.

The `ModelKind ModelExpr` exposes the following:

* A `UID` (it owns!): "newtonSLR"
* A `term` (it owns, but cloned from the internal `QDefinition`): Newton's second law for rotational motion
* A `Maybe` abbreviation (from inner `M-K-s`) via `Idea`: `Nothing`
* A human-readable definition (from inner `M-K-s`): `EmptyS`
* An axiomatization of its body (from inner `M-K-s`): $\symbf{τ}=\symbf{I}α$
* A list of type-checkable expressions (from inner `M-K-s`): $\symbf{τ}=\symbf{I}α$

### 3. A `ModelKinds ModelExpr`

This is constructed by automatically through the `equationalModelU` constructor:

```haskell
-- | Smart constructor for 'EquationalModel's, deriving Term from the 'QDefinition'
equationalModelU :: String -> QDefinition e -> ModelKind e
equationalModelU u qd = MK (EquationalModel qd) (mkUid' u) (qd ^. term)
```

Called with `equationalModelU "newtonSLR" newtonSLRQD` and we get a `EquationalModel newtonSLRQD :: ModelKinds ModelExpr` exposing the following through lenses, **all inherited from the internal `QDefinition`**:

* A `UID`: "torque"
* A `term`: "Newton's second law for rotational motion"
* A `Maybe` abbreviation: `Nothing`
* A human-readable definition: `EmptyS`
* An axiomatization of its body: $\symbf{τ}=\symbf{I}α$
* A list of type-checkable expressions: $\symbf{τ}=\symbf{I}α$

### 4. A `QDefinition`

```haskell
newtonSLRQD :: ModelQDef
newtonSLRQD = mkQuantDef' QP.torque (nounPhraseSP "Newton's second law for rotational motion") newtonSLRExpr
```

The `ModelQDef` (`QDefinition ModelExpr`) exposes the following through lenses:

* A `UID` (cloned from internal `DefinedQuantityDict`, derived from `QP.torque`): "torque"
* A `term` (inherited from internal `DefinedQuantityDict`): "Newton's second law for rotational motion"
* A `Maybe` abbreviation (inherited from internal `DefinedQuantityDict`): `Nothing`
* A `DefinedQuantityDict` it "defines": (its internal `DefinedQuantityDict`!)
* A `Space` (inherited from inner DQD): `Real`
* A `Symbol` (inherited from inner DQD): $\symbf{τ}$
* A human-readable definition (inherited from inner DQD): `EmptyS`
* A `Maybe UnitDefn` (inherited from inner DQD): `Just` (called "torque", derived from $Nm$)
* A "defining expression" (exposing the right-hand-side information; a `ModelExpr` here): $\symbf{I}α$
* An axiomatization: $\symbf{τ}=\symbf{I}α$
  The axiomatization is based on a list of input variables and the defining expression. If there were input symbols, the final expression would have been of the form: $\symbf{τ}(x,y,z,...)=\symbf{I}α$
* A list of type-checkable expressions: $\symbf{τ}=\symbf{I}α$

### 5. `DefinedQuantityDict`s

#### 5.1. One internally held in the `QDefinition` from (4)

Constructed inside `mkQuantDef'` via `quant' (c ^. uid) t EmptyS (symbol c) (c ^. typ) (getUnit c)`:

* A `UID` (inherited from `QP.torque`): "torque"
* A `term` (overridden with `t` above): "Newton's second law for rotational motion"
* A `Maybe` abbreviation via `Idea`: `Nothing`
* A human-readable definition: `EmptyS`
* A `Space`: `Real`
* A `Symbol`: $\symbf{τ}$
* A `Maybe UnitDefn`: `Just` (called "torque", derived from $\text{N}\cdot\text{m}$)

This one is **not inserted** into the `ChunkDB`.

#### 5.2. `drasil-data`'s `torque`

Defined in `Data.Drasil.Quantities.Physics` via `dqd CP.torque (vec lTau) Real torqueU`:

* A `UID` (from `CP.torque`): "torque"
* A `term` (from `CP.torque`): "torque"
* A `Maybe` abbreviation via `Idea`: `Nothing`
* A human-readable definition (from `CP.torque`): "a twisting force that tends to cause rotation"
* A `Space`: `Real`
* A `Symbol`: $\symbf{τ}$
* A `Maybe UnitDefn`: `Just` (called "torque", derived from $\text{N}\cdot\text{m}$)

This one **is inserted into the `ChunkDB`**.

#### 5.3. The Clash

5.1 and 5.2 are distinct `DefinedQuantityDict` sharing a `UID` (`"torque"`), symbol ($\symbf{τ}$), space (`Real`), and unit (`torqueU`). However, their **terms** and **definitions** are in conflict:

* 5.1 has term *"Newton's second law for rotational motion"* and definition `EmptyS`.
* 5.2 has term *"torque"* and definition *"a twisting force that tends to cause rotation"*.

Since only `QP.torque` (5.2) is registered in the `ChunkDB`, any sentence referencing `newtonSLRQD` (which emits `Ch TermStyle NoCap "torque"` as a `Sentence` fragment) resolves to 5.2, completely discarding the overridden term in 5.1.

Of course, there is a background issue: `newtonSLRQD` should not have created its own duplicate DQD.

### Miscellaneous

#### `ExprC r => r`

```haskell
newtonSLRExpr :: ExprC r => r
newtonSLRExpr = sy QP.momentOfInertia $* sy QP.angularAccel
```

#### Notes (`[Sentence]`)

```haskell
newtonSLRNotes :: [Sentence]
newtonSLRNotes = [foldlSent
  [S "The net", getTandS QP.torque, S "on a", phrase rigidBody `S.is`
   S "proportional to its", getTandS QP.angularAccel `sC` S "where",
   ch QP.momentOfInertia, S "denotes", phrase QP.momentOfInertia `S.the_ofThe`
   phrase rigidBody, S "as the", phrase constant `S.of_` S "proportionality"]]
```

## Interpreting Current Capture

Let's try to interpret the above chunks as theory definition modules and extensions.

For starters, let us recall what they are as per Dr. Farmer's Simple Type Theory book.

### Recall: Theories and Theory Extensions

* A language, $L$, is a tuple $(\set{a_1, \dots, a_m}, \set{c_{1,\alpha_1}, \dots, c_{n,\alpha_n}})$ where $a_i$ are base types and $c_{i,\alpha_i}$ are constant symbols (the $i^\text{th}$ symbol is of type $\alpha_i$).
* An axiomatic theory is a tuple, $T=(L,\Gamma{})$, where $L$ is a language and $\Gamma{}=\set{A_{1,\omicron},\dots,A_{n,\omicron}}$ is a set of formulas written in Alonzo.
* A "theory definition module" presents a theory in a human-readable/digestible manner:
```
  Theory Definition X.Y.Z
	  Name: NP
	  Base Types: a_1, ..., a_m
	  Constant Symbols: c_{1,_1}, ..., c_{n,b_n}
	  Axioms:
	    1. A_{1,B}  (NAME_1/DESCRIPTION_p)
	    2. ...
	    p. A_{p,B}  (NAME_p/DESCRIPTION_p)
```
* A theory ($T_2$) extends another ($T_1$) if it contains all the same base types, constant symbols, and formulas as the other ($T_1$), written $T_2 \ge T_1$.
* Viewing a theory extension ($T_2 \ge T_1$) as an additive operation (i.e., adding components to $T_1$ to find $T_2$), a "theory extension module" presents the new base types, constant symbols, and formulas only appearing in $T_2$:
```
  Theory Extension X.Y.Z
	  Name: NP_2
	  Extends: NP_1
	  New Base Types: a_1, ..., a_m
	  New Constant Symbols: c_{1,_1}, ..., c_{n,b_n}
	  New Axioms:
	    1. A_{1,B}  (NAME_1/DESCRIPTION_1)
	    2. ...
	    p. A_{p,B}  (NAME_p/DESCRIPTION_p)
```

In Dr. Farmer's book, Alonzo was the language of the axioms that relates the constant symbols and base types.
