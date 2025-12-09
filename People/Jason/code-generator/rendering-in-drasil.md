# Rendering chunks in Drasil and the role of Haskell

This will be somewhat of a braindump because there are several threads that I
will try to tie together here:

1. Encoding `sin`: ADT, tagless, or chunk? Why?
2. `drasil-database`: Gathering chunk dependencies.
3. `drasil-lang`: The `drasil-code`-related code that does not belong.
4. `drasil-printers`: Duplication in printers and high coupling.
5. `drasil-artifacts`: A prospective library to support chunk _rendering_.

The objective of this is to argue for `drasil-artifacts` amongst other work,
including capturing `sin` as a chunk (!).

## Background

In this section, we will discuss completely separate issues.

### 1. Encoding `sin`: deep, tagless, or _chunk_? Why?

In an in-person meeting with @smiths, @JacquesCarette, and I, we discussed how
`sin` is captured in Drasil. Currently, we [capture
it](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-lang/lib/Language/Drasil/Expr/Lang.hs)
as a unary function in two ways:

```haskell
-- | Unary functions (abs, log, ln, sin, etc.).
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg
  deriving (Eq, Show)
...

-- | Expression language where all terms are supposed to be 'well understood'
--   (i.e., have a definite meaning). Right now, this coincides with
--   "having a definite value", but should not be restricted to that.
data Expr where
  -- | Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> Expr -> Expr
  ...

class ExprC r where
  -- | Smart constructor to apply sin to an expression.
  sin :: r -> r
  ...
```

The first is through our deep embedding of our mathematical expression language
(`Expr`) and the second is through the tagless embedding of the same language.

#### Deep embedding

We use the deep embedding to create a hard boundary of terms permitted in
specific locations, for example, `Expr` requires all its terms to have a
definite meaning. However, because we have multiple kinds of boundaries we want
to impose on our mathematical expression language, we have 3 languages: `Expr`,
`ModelExpr`, and `CodeExpr`. The latter two are for general-purpose display-only
mathematics and admitting OO-related terminology, respectively. There is
significant duplication between these three languages: `Expr` is at the heart of
each.

#### Tagless embedding

The tagless embedding allows us to be polymorphic about the representation type,
avoid writing duplicate expressions by allowing us to write expressions without
forcing a decision about which langauge the expression belongs to. For example,
$\sin(e)$ can belong to any of them. With the tagless embedding, this expression
has the type `ExprC r => r`. With the deep embedding, we would write $\sin(e)$
as `UnaryOp Sin e`, but `UnaryOp` would be a constructor of one of the 3
languages that would indicate which expression language this expression strictly
belongs to.

#### Further Duplication

Constructor duplication is not the only kind of duplication we have in our
expression languages, however. For example, a common operation we also perform
is "searching for referenced chunks:"

1. [Expr](https://github.com/JacquesCarette/Drasil/blob/main/code/drasil-lang/lib/Language/Drasil/Expr/Extract.hs)
2. [ModelExpr](https://github.com/JacquesCarette/Drasil/blob/main/code/drasil-lang/lib/Language/Drasil/ModelExpr/Extract.hs)
3. [CodeExpr](https://github.com/JacquesCarette/Drasil/blob/main/code/drasil-lang/lib/Drasil/Code/CodeExpr/Extract.hs)

All three files are more or less the same.

Another kind of duplication we have amongst them is simplification (while we
only have a bit, it is duplication):

1. [Expr](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-lang/lib/Language/Drasil/Expr/Class.hs#L245-L255)
2. [ModelExpr](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-lang/lib/Language/Drasil/Expr/Class.hs#L411-L421)
3. [CodeExpr](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-lang/lib/Drasil/Code/CodeExpr/Lang.hs#L197-L207)

And for a final example of duplication, we have rendering. Between `Expr` and
`ModelExpr`, the rendering of `ModelExpr` to a human-readable document for
mathematicians and the likes should merely be an extension of `Expr`. However,
it currently relies on duplicate code, much like that previous.

It goes without saying in our circle, but with duplication comes fragile
maintenance and development, and increased maintenance costs (time and money).
We try to avoid it as much as we can.

#### Commentary on Duplication

We want to avoid duplication in Drasil. Hence, this is an issue. We don't quite
have a solution to yet. We don't have a crystal clear understanding of the role
that Haskell plays in Drasil yet. Using complex Haskell features might not be
the right thing to use to avoid this duplication because we haven't really
captured anything about "sin," we've only gained the ability to _use it_.

#### What are we missing about `sin`?

First, an example: to capture kinematics equations, we use a mixture of
`TheoryModel`s, `GenDefn`s and `DataDefinition`s which contain instances of
`Expr` and `ModelExpr`. We capture quantities like velocity with
`DefinedQuantityDict`s and we create a "velocity" word chunk (an `IdeaDict`)
that lets us reference the word "velocity" in the context of writing an SRS
document. We also capture a structured mathematical meaning of "velocity" that
lets us assign it any number of formulas we choose and for a variety of
contexts. For example, in different Drasil projects, we might need individual
instances of velocity for each entity in our problem model, or we might use
different coordinate systems. Regardless, all carry different definitions of
velocity. For `sin`, when we want to reference the word, we resort to hacks,
such as in `SglPend/GenDefs.hs` (see below snippet) or relying on injecting
expression terms into `Sentence`s (which will *always* appear in math-mode --
what if we wanted code-mode, say, in the design document?) and them being
renderable to human language as well (again, a kind of duplication in our
renderers).

```text
drasil-example/sglpend/lib/Drasil/SglPend/GenDefs.hs:
217:... = ... S "we approximate" +:+ S "sin" +:+ ch pendDisplacementAngle ...
```

In different applications, we might want a different definition of `sin`. For
example, in the context of some arbitrary Java application I'm looking to
generate, I might want it to generate code corresponding to `StrictMath.sin`
rather than `Math.sin` (our default implementation). If we had more
sophisticated software design in `Choices`, we might be able to pick
`StrictMath` instead of `Math`, but what if I wanted my own custom definition
that I know is faster than both `StrictMath` and `Math` for my needs of `sin`? I
would probably need to argue (derive) for a custom definition of `sin` in the
SRS (or maybe this belongs in the prospective SDS phase... but location does not
matter much). We cannot do this in Drasil.

At this point, you might recall $\pi$. From the math-side, we have a definition
of $3.14159...$ but we generate $Math.pi$ (or whatever else language-native
concept). How do we do that? We do this through
[`ConceptMatch`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-code/lib/Language/Drasil/Choices.hs#L133-L136):

```haskell
  -- | Map of 'UID's for Drasil concepts to code concepts.
  -- Matching a 'UID' to a code concept means the code concept should be used
  -- instead of the chunk associated with the 'UID'.
  conceptMatch :: ConceptMatchMap,
```

Which relies on a very well-placed hack in
[`CodeSpec`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-code/lib/Language/Drasil/CodeSpec.hs#L188-L189):

```haskell
const' = map qtov (filter ((`Map.notMember` conceptMatch (maps chs)) . (^. uid))
  cnsts)
````

And an [enumerated data
type](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-code/lib/Language/Drasil/Choices.hs#L155-L157)
of all the things we can possibly substitute the GOOL definitions of:

```haskell
-- Currently we only support one code concept, more will be added later
-- | Code concepts. For now, just pi.
data CodeConcept = Pi deriving Eq
```

All of this code strictly works for $\pi$! However, as it is, it has an
extensibility issue: it only supports the language-native concept of $\pi$ that
was defined in `drasil-gool`. We wouldn't be able to, for example, use `numpy`'s
definition of $\pi$
([`numpy.pi`](https://numpy.org/devdocs/reference/constants.html#numpy.pi)). The
code related to making this work is fragile, and any maintenance work for
extending this is equally fragile. Furthermore, it would mean that if a
prospective physicist were to use Drasil for one of our off-the-shelf
generators, they would need to edit Haskell code (which is not something I would
want them to do, though we might have differing opinions).

To sum up:

1. The syntax of `sin` is not usable usable in the same way as our user-defined
   concepts.
2. The semantics of `sin` is not extensible in the ways we would like.
3. The code related to our encoding of `sin` contains significant duplication.

Naturally, the question arises: is our design enough?

#### Deep and Tagless embeddings: Are they really enough?

I believe what all of this is telling us is that we want to move more concepts
inwards into Drasil. This all makes me believe that we want the ability to
create "tagless" embeddings of languages to be a key feature of Drasil, not
something we ask of Haskell. In some vague sense, with `IdeaDict`s, we already
do this a bit.

**Speculation:**

1. If we had the ability to capture "Drasil in Drasil," then we might view the
   exiting copy of Drasil we have today as the "Drasil we generated with some
   specific configuration options that uses Haskell-native concepts to perform
   Drasil-like operations for a specific subset of Drasil." You might need to
   squint your eyes really hard to understand what I mean (and this is probably
   best to continue the discussion of in person or when I feel ready to break
   this down to a degree I would feel comfortable writing about without wasting
   our time).
2. Continuing along this line of thinking, I continue to have a nagging feeling
   that the mythical "Drasil in Drasil," functionally, would have much more
   reliance on `drasil-database`, an internalized language of transformations,
   and some very specific parts of `drasil-lang` merged into `drasil-database`
   (e.g., `IdeaDict`, `Concept`, `Sentence`, etc.). And I think this should be
   no surprise considering our discussions of triform theories, domain-specific
   languages, syntax and semantics, and domains.

### 2. `drasil-database`: Gathering chunk dependencies

Consider the following two tables in
[`System`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-system/lib/Drasil/System.hs#L89-L90):

```haskell
  , _refbyTable   :: M.Map UID [UID]
  , _traceTable   :: M.Map UID [UID]
```

These two tables are for generating our chunk "trace" tables, for example,
Projectile's:

* [Assumptions vs Assumptions Table](https://jacquescarette.github.io/Drasil/examples/projectile/SRS/HTML/Projectile_SRS.html#Table:TraceMatAvsA)
* [Assumptions vs All Table](https://jacquescarette.github.io/Drasil/examples/projectile/SRS/HTML/Projectile_SRS.html#Table:TraceMatAvsAll)
* ['Items and Other Sections' Table](https://jacquescarette.github.io/Drasil/examples/projectile/SRS/HTML/Projectile_SRS.html#Table:TraceMatRefvsRef)

What these tables capture is what chunks _directly depend on_. The relationship
is two-way: for a chunk A, what chunks A depends on and what chunks depend on A.

#### How we build trace tables for the SRS

Creating these tables begs scanning `Sentence`s from a _pre-generated SRS
document_.

* [`mkDoc`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-docLang/lib/Drasil/DocumentLanguage.hs#L77-L97)

```haskell
-- | Creates a document from a 'System', a document description ('SRSDecl'), and
-- a title combinator.
mkDoc :: System -> SRSDecl -> (IdeaDict -> IdeaDict -> Sentence) -> (Document, System)
mkDoc si srsDecl headingComb =
  let dd = mkDocDesc si srsDecl
      sections = mkSections si dd
      -- Above this line, the content to be generated in the SRS artifact is
      -- pre-generated (missing content involving 'Reference's and
      -- 'LabelledContent'). The below line injects "traceability" maps into the
      -- 'ChunkDB' and adds missing 'LabelledContent' (the generated
      -- traceability-related tables).
      si'@SI{ _authors = docAuthors } = fillLC dd $ fillReferences sections $ fillTraceMaps dd si
      -- Now, the 'real generation' of the SRS artifact can begin, with the
      -- 'Reference' map now full (so 'Reference' references can resolve to
      -- 'Reference's).
      heading = whatsTheBigIdea si `headingComb` sysName si'
      authorsList = foldlList Comma List $ map (S . name) docAuthors
      toc = findToC srsDecl
      dd' = mkDocDesc si' srsDecl
      sections' = mkSections si' dd'
  in (Document heading authorsList toc sections', si')
```

* [Filling in a `System`'s trace tables](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-docLang/lib/Drasil/DocumentLanguage.hs#L162-L168):

```haskell
-- | Fills in the traceabiliy matrix and graphs section of the system
-- information using the document description.
fillTraceMaps :: DocDesc -> System -> System
fillTraceMaps dd si = si''
  where
    tdb = generateTraceMap dd
    si' = set traceTable tdb si
    si'' = set refbyTable (invert tdb) si'
```

* [Scanning pre-generated document sections](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-docLang/lib/Drasil/TraceTable.hs#L17-L48)

```haskell
-- | Creates a dependency plate for 'UID's.
dependencyPlate :: DLPlate (Constant [(UID, [UID])])
dependencyPlate = preorderFold $ purePlate {
  pdSub = Constant <$> \case
    (Goals _ c) -> getDependenciesOf [defs] c
    _ -> [],
  scsSub = Constant <$> \case
    (Assumptions a) -> getDependenciesOf [defs] a
    (TMs _ _ t)     -> getDependenciesOf [\x -> map (^. defn) (x ^. defined_quant) ++
      map (^. defn) (x ^. operations), notes] t
    (DDs _ _ d _) -> getDependenciesOf [derivs, notes] d
    (GDs _ _ g _) -> getDependenciesOf [defs, derivs, notes] g
    (IMs _ _ i _) -> getDependenciesOf [derivs, notes] i
    _ -> [],
  reqSub = Constant . getDependenciesOf [defs] <$> \case
    (FReqsSub c _) -> c
    (NonFReqsSub c) -> c,
  lcsSec = Constant . getDependenciesOf [defs] <$> \(LCsProg c) -> c,
  ucsSec = Constant . getDependenciesOf [defs] <$> \(UCsProg c) -> c
} where
  getDependenciesOf :: HasUID a => [a -> [Sentence]] -> [a] -> [(UID, [UID])]
  getDependenciesOf fs = map (\x -> (x ^. uid, concatMap (lnames' . ($ x)) fs))
  defs :: Definition a => a -> [Sentence]
  defs x = [x ^. defn]
  derivs :: MayHaveDerivation a => a -> [Sentence]
  derivs x = maybe [] (\(Derivation h d) -> h : d) $ x ^. derivations
  notes :: HasAdditionalNotes a => a -> [Sentence]
  notes = (^. getNotes)

-- | Creates a traceability map from document sections.
generateTraceMap :: [DocSection] -> M.Map UID [UID]
generateTraceMap = M.fromList . concatMap (foldFor docSec dependencyPlate)
```

#### Issues with how we currently capture them

There are at least 3 problems with this code:

1. It only considers things directly mentioned through `Sentence`s. In other
   words, there is a chance it misses relationships.
2. It is specialized to the SRS template (see
   [`DLPlate`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-docLang/lib/Drasil/DocumentLanguage/Core.hs#L247-L331)).
   This complicates extension (for the template or to new templates) and
   maintenance.
3. The code is repetitive (i.e., duplication).

#### Generalizing chunk dependency capture

As part of my work on the `ChunkDB`, a feature (requirement) of all chunks
registered in the `ChunkDB` is that they know what chunks they _directly_ depend
on (one-level dependency relationsips). This has been an work-in-progress
project for a long time and currently, we default all of our chunk depenency
lists to `[]`. More recently, Xinlu started [filling in these
lists](https://github.com/JacquesCarette/Drasil/pull/4434) and highlighted to us
that producing these lists could perhaps be
[automated](https://github.com/JacquesCarette/Drasil/pull/4476) (incomplete).

The nice thing about this is that when we generate an SRS, we generate chunks
(!), and so, if/when above PRs are merged, we should be able to replace the code
related to manually scanning `Sentence`s with checking chunk dependencies and
dependants.

### 3. `drasil-lang`: The `drasil-code`-related code that does not belong

**Note**: I've discussed this issue in
[#4509](https://github.com/JacquesCarette/Drasil/pull/4509) as well using PR
annotations.

Currently,
[`drasil-lang`](https://github.com/JacquesCarette/Drasil/tree/79a4a1bd9b1de3bef1f062ddac039dcc8f28bc76/code/drasil-lang/lib)
contains at least two major package namespaces:

1. `Drasil.Code`
2. `Language.Drasil`

Originally/previously, all code in Drasil lived in `Language.Drasil`, and then
new packages and namespaces were gradually pulled out of `Language.Drasil`
(fixme: needs fact checking from @smiths/@JacquesCarette). `Drasil.Code` in
particular is a package of modules sorted from `Language.Drasil` that are
strictly relevant to `drasil-code` (the package primarily responsible for
converting an SRS ICO problem into executable software artifacts).

Now, with the following PRs, nothing in `Language.Drasil` depends on
`Drasil.Code`:

1. [drasil-lang: Switch RawContent's CodeBlock constructor from using CodeExpr to Expr. #4497](https://github.com/JacquesCarette/Drasil/pull/4497)
2. [Remove drasil-code-related re-exports from drasil-lang. #4503](https://github.com/JacquesCarette/Drasil/pull/4503)

However, despite the two namespaces now being completely independent within
`drasil-lang`, `Drasil.Code` cannot be moved to `drasil-code` (yet). Why?
Because `drasil-code` depends on `drasil-printers` and `drasil-printers` depends
on the few files leftover in `Drasil.Code` from `drasil-lang`. So, if we were to
move these modules to `drasil-code`, we would be forming a cyclic dependency!

#### Cyclic dependency between `drasil-code` and `drasil-printers`

##### Why does `drasil-printers` rely on `Drasil.Code`?

`drasil-printers` relies on `Drasil.Code` because `Drasil.Code` contains the
definition of `CodeExpr`, which relies on the rest of the modules in
`Drasil.Code`. `drasil-printers` imports `CodeExpr` because it exports a
[renderer](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-printers/lib/Language/Drasil/Printing/Import/CodeExpr.hs#L103-L113)
for them (to the printing-focused expression language).

##### Why does `drasil-code` rely on `drasil-printers`?

`drasil-code` relies on `drasil-printers` for 5 reasons primarily:

* Rendering `Symbol`s.
* Rendering `Sentence`s.
* Rendering `CodeExpr`s.
* Rendering `Expr`s.
* Creating Markdown files.

#### How should we resolve the cycle?

I believe the simplest way to avoid the cycle is to move the textual renderers
closer to the definitions of the languages. This also has the benefit of
simplifying searching for what code renders what. However, I intentionally
explored a different in
[#4509](https://github.com/JacquesCarette/Drasil/pull/4509) (which I will omit
from this document).

Should we choose to move the individual renderers closer to the definitions of
our languages, we should:

1. Create a new package, e.g., `drasil-printlang`.
2. Move the printing expression language and other generic ASTs we build in
   `drasil-printers` towards `drasil-printlang`.
3. Move what's leftover in `drasil-printers` elsewhere.
4. Delete `drasil-printers`.

### 4. `drasil-printers`: Duplication in printers and high coupling

**Duplication**: In the first section, we discussed duplication across our
mathematical expression languages. This includes in `drasil-printers` where we
have renders for each of our 3 expression languages into the printing-focused
expression language (again, this code has significant duplication). However, if
you look at any of the output formats between the renderers, there is also
duplication that begs someone to mine through it and de-duplicate it. For
example, Markdown rendering could be better shared between Jupyter and the
Markdown renderer.

**High coupling**: As a package, `drasil-printers` seems to be highly coupled
with the needs of our other packages, but not quite in the right way, as a
library of tools at their disposal. Rather, excluding the generic printing
languages, `drasil-printers` is more of a series of modules that are better
placed within the coupled libraries.

The names of modules would make one think they are made for generic use.
However, looking at them in vacuum (including dependencies), it contains
features that `drasil-docLang` and `drasil-code` specifically depend on that
they should be doing instead. That is, for Drasil, as an SCS generator, all of
these files exist for good reason, but the library is highly coupled with all
other packages. For example:

1. The [`.dot` file
   renderer](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-printers/lib/Language/Drasil/DOT/Print.hs#L31-L65)
   is highly specialized to our SRS format. It would most likely be best to move
   this code to `drasil-docLang` and gradually move the most generic parts back
   into `drasil-printers`.
2. It contains a
   [`CSS`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-printers/lib/Language/Drasil/HTML/CSS.hs#L8-L138)
   generator that contains code specialized to our usecase in our other
   packages. The 'generator' is also not quite a 'generator', but a 'copy/paste
   CSS to disk' tool. It should likely be moved elsewhere.
3. The
   [`JSON`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-printers/lib/Language/Drasil/JSON/Print.hs#L1-L2)
   renderer is really a Jupyter Notebook generator. It doesn't use a JSON
   renderer in the backend [to create new
   cells](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-printers/lib/Language/Drasil/JSON/Helpers.hs#L128-L142).
   We should rename it, at least.
4. The
   [`Markdown`](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-printers/lib/Language/Drasil/Markdown/Print.hs#L36-L42)
   renderer is also really an `mdBook` generator.
5. It contains knowledge that we might use the package in [Scientific and
   Engineering
   contexts](https://github.com/JacquesCarette/Drasil/blob/5ab30a2ba1915b17d9102ff6c86d28eb212512e2/code/drasil-printers/lib/Language/Drasil/Printing/PrintingInformation.hs#L20-L22).
   Shouldn't this information be erased by the time we render into the various
   generic rendering languages we expose in `drasil-printers`?

A complete audit of the code will show more discussion topics and issues (for
example, "Motivation" and "Purpose" are very unlikely keywords to appear in a
generic printing library).

### 5. `drasil-artifacts`: A prospective library to support chunk _rendering_

[#3843](https://github.com/JacquesCarette/Drasil/issues/3864) and its related
issues are the background context of this. To make a long story short, a library
sitting atop `Doc`s that helps us record more information about `Chunk -> Doc`
rendering operations would be helpful for several reasons:

1. We would know which chunks are rendered in which `Doc`s (and in which
   _files_, too).
2. For the chunks "rendered," we would be able to create reference handles
   (e.g., link anchors in HTML/Markdown and absolute variable names in code)
   without assuming which ones will be rendered or won't be.
3. More generally, we can add any kind of logging and information gathering to
   our chunk rendering code. With this, for example, we can:
   * Use policies to automate writing of abbreviation introductions depending
     on whether they have been introduced or not. For example, rather than our
     current `Sentence`-based injection of "long-form (short-form)" abbreviation
     introductions, if a short-form term was already introduced, then we can
     write the short-form, else we should record usage of and write the
     introduction form.
   * The work done in
      [#4363](https://github.com/JacquesCarette/Drasil/pull/4363) could be done
      again without relying on scanning `Sentence`s but through dumping a list
      of short-forms introduced in the file.
   * We could introduce statistics about how often concepts are referenced in
     documents and potentially suggest when multiple compound noun-phrase-named
     concepts should have an abbreviation assigned (e.g., if we used National
     Aeronautics and Space Administration multiple times in a document without
     having captured the acronym [NASA], we could automatically suggest that the
     user should consider creating one).
4. We gain concrete traceable links between various software artifacts. For
   example, we should be able to say "this concept was rendered into x, y, and z
   files in the following forms... ."

## Tying Them All Together

At this point, you might be thinking: Jason, aren't you supposed to be working
on adding more features to `drasil-code`? Yes, that is what I am doing, and I'm
avoiding over-engineering (at least until ideas are validated) any solutions.
All of this discussion spawns from wanting to add features to `drasil-code` but
running into systemic issues that are holding us back.

### How this Started

All of this started because I wanted to clean up the code in `drasil-code`
before I start adding features. The last change I was working on before writing
this was about moving `CodeExpr` from `drasil-lang` to `drasil-code`. This did
not end up being easy, and it is forcing us to deal with our ongoing issues with
"printing"/"rendering" in Drasil.

### Action Plan

1. **`drasil-printers`**: The steps in [How should we resolve the
   cycle?](#how-should-we-resolve-the-cycle) are perhaps optimistic. What could
   be moved into `drasil-printlang`, for example? `PrintingInformation` (or some
   printing configuration type variable) would need to be moved there, as well
   as the various 'printing ASTs'. Afterwards, we would move the individual
   renderers closer to the definitions of the data types they render. This will
   involve moving code to `drasil-lang`, `drasil-code`, and `drasil-docLang`.
2. (tangential to the core issues with `drasil-code`; not strictly necessary)
   **`drasil-artifacts`** is really just a renamed copy of `drasil-printers`. At
   this point, so we would look towards 'upgrading' it with stateful rendering
   and, while we're at it, switching the backend from `pretty` `Doc`s
   (`String`-based) to `Prettyprint` `Doc`s (`Text`-based). So, we can drop
   `drasil-artifacts` in favour of keeping our existing package name;
   `drasil-printers` (which now becomes a 'base' package in all other packages,
   except for `drasil-database`).
3. **`drasil-code`**: De-coupling from `drasil-lang` and continuously cleaning
   up the code. This will involve cutting as many dependencies on `drasil-lang`
   as possible. In particular, replacing reliance on `DefinedQuantityDict`s with
   some other (internally held) data type. Moving 'translation' logic of IMs,
   DDs, GDs, and TMs from `drasil-code` to `drasil-gen`.
4. **`drasil-lang`**: Continuing to move code out from `drasil-lang` and into
   other packages.
