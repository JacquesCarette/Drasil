
# Typed Expressions in Drasil

## Basic Background Information

* Drasil is a system of knowledge-oriented programming languages, and a compiler for said languages.
* Drasil is bootstrapped in Haskell.
  * Eventually, Drasil will be built in Drasil.

## Objective

Expressions should be well-typed, and expose information about their resultant types.

Specifically, we would like the terms of the Expression language to be "safe" in usage, so that we can reliably generate well-formed and well-behaved programs.

## Solution: Forming a system of constraints for expression formations

In order to ensure that all terms of the expression language are "valid"/well-formed, we must define what it means for any term to be "valid"/well-formed. We will define "validity"/well-formedness here through defining a system of inference rules that dictate when we're able to form terms of the expressions abstract syntax tree.

### The Inference Rules

TODO: inference rules & syntax chart as done in "Practical foundations for Programming Languages" (e.g., Harper, pg. 34). Export to SVG(?) and link here.

<small>Omitted for now, many are "obvious", but I will write them out properly soon™.</small>

## Major Design Choice: Phase distinction for type checking

An important thing to remember is that, ultimately, Drasil is a _compiler_ on its own. Presently, Drasil and the ASTs of Drasil programs are bootstrapped in Haskell as a host language. In the future, Drasil might become a self-hosted compiler (and likely would need to be for _Drasil in Drasil_).

Since Drasil is bootstrapped in Haskell, we might find ourselves (as I have) looking to advanced Haskell tools to build Drasil. This might be great, and work great, but it would pose a problem later when trying to build Drasil in Drasil. Additionally, it would mean that we are imposing implicit restrictions on Drasil from those advanced tools.

Here, we define the expression languages using an Algebraic Data Type in Haskell. In order for us to impose typing restrictions in Haskell, we may lean on Haskell's type system (e.g., leveraging GADTs), or we can roll our own type system for the expressions by enforcing type restrictions on expression formations at time of construction (e.g., runtime, which is the "Drasil compile-time").

### Sol. 1: Leaning on Haskell's type system

[Prototype](https://github.com/balacij/NewExprTests/tree/main)

[Relevant Snippet:](https://github.com/balacij/NewExprTests/blob/202360805bf6305d717811e5d2cdb9f258141c4f/src/Lib.hs#L21-L48)

```haskell
var1 :: QuantityDict Int
var1 = mkQuantityDict (Proxy @Int) (mkUid "var1") "a1" "b1"

var2 :: QuantityDict Bool
var2 = mkQuantityDict (Proxy @Bool) (mkUid "var2") "a2" "b2"

func1 :: QuantityDict (Int -> Int)
func1 = mkQuantityDict (Proxy @(Int -> Int)) (mkUid "func1") "c1" "d1"

func2 :: QuantityDict (Bool -> Int -> Int -> Int)
func2 = mkQuantityDict (Proxy @(Bool -> Int -> Int -> Int)) (mkUid "func2") "c2" "d2"

varDef1 :: QDefinition Expr Int
varDef1 = mkQDefinition (mkUid "varDef1") var1 (int 1) "e1" "f1"

varDef2 :: QDefinition Expr Bool
varDef2 = mkQDefinition (mkUid "varDef2") var2 (not_ $ bool False) "e2" "f2"

funcDef1 :: QDefinition Expr (Int -> Int)
funcDef1 = mkQDefinition (mkUid "funcDef1") func1 (lam $ \x -> add x (int 1)) "g1" "h1"

funcDef2 :: QDefinition Expr (Bool -> Int -> Int -> Int)
funcDef2 = mkQDefinition (mkUid "funcDef2") func2 (lam $ \b -> lam $ \x -> lam $ \y -> ifTE b x y) "g2" "h2"

funcDef3 :: QDefinition Expr (Int -> Int)
funcDef3 = mkQDefinition (mkUid "funcDef3") func1 (lam (\x -> add x (int 1))) "g3" "h3"
```

[Prototype of notable problem: unable to automatically collect and apply a function to all things of a specific type constructor, ignoring its type arguments, due to being unable to disambiguate the type arguments](https://github.com/balacij/NewExprTests/tree/failedAttemptAtConsumingAllWithTypeConstructor)

#### Pros

* Traceability of errors: Haskell reported errors are very descriptive and easy to read, showing the specific line numbers and files that a problem occurs.
* Readability: Types of Quantities, Definitions, etc. are displayed elegantly through the type arguments of the Haskell variables.
* Leaning on Haskell's rich types, we can defer writing many of the type restrictions to implicit ones imposed through Haskell's type system.

#### Cons

* Named arguments in function calls do not play well statically.
* Relies heavily on "upgraded" ChunkDBs.
* Types of the literals show up with Haskell names, instead of the preferred names (the ones used in the `Space` ADT).
* Dictating the space/type of a quantity relies on either `Type Applications` or on `Proxy`. Either way, it relies on something that is very deep in Haskell.
* Even with the upgraded ChunkDBs, picking all chunks of a particular type constructor is difficult. It requires us to pick each individually by a monomorphic type. For example, if we have a `Thing a b c`, and we wanted to find all `Thing`s and apply some function `f :: Thing a b c -> d` to them all, we would need to be able to resolve a monomorphic type signature of each individual `Thing`, or else Haskell won't compile. To my understanding, this error is, in part, because we don't have enough information for Haskell to properly resolve the typeclass instances of each `Thing`.

### Sol. 2: Rolling our own type system

[Prototype](https://github.com/balacij/NewExprTests/tree/typeCheckedInDrasil)

[Relevant Snippet:](https://github.com/balacij/NewExprTests/blob/b92ad99091a266b3d5c577d69b0b3cbcbe752537/src/Lib.hs#L23-L39)

```haskell
var1 :: QuantityDict
var1 = mkQuantityDict S.Integer (mkUid "var1") "a1" "b1"

var2 :: QuantityDict
var2 = mkQuantityDict S.Boolean (mkUid "var2") "a2" "b2"

func1 :: QuantityDict
func1 = mkQuantityDict (S.Function (S.Integer NE.:| []) S.Integer) (mkUid "func1") "c1" "d1"

func1_dummy_var :: QuantityDict
func1_dummy_var = mkQuantityDict S.Integer (mkUid "func1_dummy_var") "input parameter" "input parameter"

func2 :: QuantityDict
func2 = mkQuantityDict (S.Function (NE.fromList [S.Boolean, S.Integer, S.Integer]) S.Integer) (mkUid "func2") "c2" "d2"

varDef1 :: QDefinition Expr
varDef1 = mkQDefinition (mkUid "varDef1") var1 (int 1) "e1"

varDef2 :: QDefinition Expr
varDef2 = mkQDefinition (mkUid "varDef2") var2 (not_ $ bool False) "e2"

funcDef1 :: QDefinition Expr
funcDef1 = mkFuncDefinition (mkUid "funcDef1") func1 [func1_dummy_var] (add [int 1, sy func1]) "explanation"

-- | If this chunk below is evaluated anywhere, it will cause an error.
funcDef1_BAD :: QDefinition Expr
funcDef1_BAD = mkFuncDefinition (mkUid "funcDef1") func1 [func1_dummy_var] (bool True) "explanation"
```

#### Pros

* As rules are explicitly written, we have generally more flexibility in our type system. In particular, we are not constrained by the statics of Haskell's type system.
  * Allows for named arguments in function calls.
  * Low complexity in implementation.
* Where Haskell's type system (based on simple type theory) fails to sufficiently express what we need (e.g., where we might want dependent types), this approach will allow us to naturally express and type check said situations.
* This prototype can likely be implemented directly in existing Drasil infrastructure.
  * Additionally, at a glance, looks like it would work without the "upgraded" ChunkDBs. However, having Typed UID references is a large bonus, we can likely make this an aside issue bringing discussion of chunk formation into question as well.
* Giving quantities a type/space is done normally through `QuantityDict`s.
* _Arguably_ does not exhibit the "notable problem" described above in the "Leaning on Haskell's type system" section because each chunk type is expected to have no type parameters.

#### Cons

* If an expression is ill-formed, and since Haskell is lazy-evaluated, we will likely only find out at some point during the runtime (Drasil's compile-time). In other words, we would not be able to leverage Haskell Language Server to statically find errors as we write expressions (though, we could write our own LSP if we really wanted it later [/largely down the road]).
* Haskell's type system is very rich and works well for _most_ things.
* Readability of types is a bit more difficult to read than the other method because the other method exposes them through Haskell type signatures.
* Traceability of errors (due to errors only being thrown at runtime, we would need to display UIDs to be able to locate problematic areas).
* A bit more tedious to write out explicit type restrictions on formations; we will need to do all of the heavy lifting that the other approach does for us automatically.
* Since Drasil does not yet support dummy variables for function arguments, we currently rely on having global QuantityDicts for each function's arguments (but I think we can resolve/change this later).

#### Potential Changes

* "error" usage might make things a bit difficult, perhaps we can switch to using more `Either Expr Error` (with `type Error = String/Text`)?
* Add support for higher order expressions functions (doesn't look too difficult with this style!)
* Dummy variables instead of using registered QuantityDicts for parameters

#### Future

* Types and type constraints might be better to be written as ideas in Drasil as well, using a Haskell stub as the runtime.

### Sol. Follow-up

From the meeting on Feb 18th, 2022, we've decided to give Sol. 2 a go.

## Other Decisions

1. Should symbols refer to specific implementations of a symbol, or to _just_ the symbol itself? As of right now, we are going for the latter for flexibility.
2. The expression language has been split off into 3 different languages, each with their own purpose.
    <small>This is already documented elsewhere, I will omit its discussion here.</small>
3. In light of the discussion of type parameters above, we might also want to consider reverting my past change of adding "Expr"-like things as a type argument for QDefinitions, potentially replacing them with a GADT for Simple (Expr), Model (ModelExpr), and Constant (Literal). Similarly, with ModelKinds.
