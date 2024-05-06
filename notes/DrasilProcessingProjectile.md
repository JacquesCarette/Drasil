# How Drasil's Implementation Works: Examining Projectile

## DOCUMENT META-INFORMATION

**Target Audience**: Newcomers to Drasil

Key assumptions and notes:
* Readers understand the differences between compilers, generators, translators,
  transformers, printers, mappings, and refinement. All are really different
  forms of a unary IO function with different properties and connotations.
* Readers understand the niche that Drasil serves (scientific computing
  software) and pros/cons of Drasil compared to traditional software development
  means.
* Drasil-specific terminology and acronyms will either be common (and assumed)
  or created (and defined) for the purposes of this document.

However, we will not assume that:
* Readers understand the background theory of Drasil (unimportant to this
  document).
* Readers understand how Drasil's implementation works (the objective of this
  document).

## Objective

To describe how Drasil's implementation works through examining what enables
Drasil to generate various software artifacts for the Projectile case study.

## Projectile

The Projectile case study examines the motion of a projectile launched from rest
towards an object, describing calculations required to determine if a “hit”
occurs and a derivation of said calculations from physics theory.

### Mathematics

Given a target position distance ($p_{target}$, in $m$ from the launch
position), launch speed ($v_i$) and angle ($\theta$), we can calculate the
landing position of the projectile ($p_{land}$), determine how far the
projectile landed from the target ($d$), and describe the situation ($s$) within
a reasonable tolerance ($\epsilon$) using the following system of equations:
1. $p_{land} = \frac{2 v_i \sin{}(\theta{})\cos{}(\theta{})}{g}$
2. $d = p_{land} - p_{target}$
3. $s =
  \begin{cases}
    \text{``Hit!''}, & |\frac{d}{{p_{target}}}|\lt{} \epsilon{} \\
    \text{``Short!''}, & d \lt{} 0\\
    \text{``Long!''}, & d \gt{} 0
  \end{cases}$

### Drasil

Modelled after an
[original](https://github.com/smiths/caseStudies/tree/master/CaseStudies/projectile)
project containing manually created Software Requirements Specifications (SRS)
and software artifacts, the Projectile case study was reconstructed in Drasil.

#### Outputs

Through Drasil, the Projectile case study [produces the following
artifacts](../code/stable/projectile/):

* An SRS (describing the problem) in...
  * LaTeX,
  * HTML, and
  * (work-in-progress) Jupyter Notebooks (JN).
* A corresponding software (the solution) in various languages (Java, C++,
  Python, Swift, and C#) and with various options for each language...
  * program modularity (splitting up the code into several files for
    reusability and readability, or not),
  * logging (should there be logging?),
  * data type representation (are real numbers represented as floats? doubles?),
  * logic-error handling (should business logic errors be soft warnings or hard
    errors?),
  * etc.

#### Inputs

The [Projectile](../code/drasil-example/projectile/) project houses the code
that:
1. [orchestrates the generation](../code/drasil-example/projectile/app/Main.hs)
   of the [outputs](#outputs) using
2. encodings of all related [“knowledge
   fragments”](../code/drasil-example/projectile/lib/Drasil/Projectile/) needed
   that go into description of the SRS.

The “knowledge fragments” (also called “chunks”) capture the structure of
various pieces of information using Embedded Domain-Specific Languages (eDSLs)
hosted in Haskell. For example, a “quantity” is captured using
[`QuantityDict`s](../code/drasil-lang/lib/Language/Drasil/Chunk/Quantity.hs):
```haskell
data QuantityDict = QD { _id' :: IdeaDict
                       , _typ' :: Space
                       , _symb' :: Stage -> Symbol
                       , _unit' :: Maybe UnitDefn
                       }
```

`QuantityDict`s capture a few ideas related to “quantities:”
1. `_id'`: What does this quantity represent (mostly textual content)?
2. `_typ'`: What is the _type_ of this quantity (e.g.,
   real/rational/integer/string/etc.)?
3. `_symb'`: How is this quantity displayed when referenced in mathematical
   equations and in code (e.g., $g$, $\pi$, $\tau$, etc.)?
4. `_unit'`: Does this quantity have a unit (e.g., $m$, $s$, $m/s$, etc.)?

For example, the [output message
variable](../code/drasil-example/projectile/lib/Drasil/Projectile/Unitals.hs):
```haskell
message :: QuantityDict
message = vc "message" (nounPhraseSent (S "output message as a string")) lS String
```

The [defining
formula](../code/drasil-example/projectile/lib/Drasil/Projectile/Expressions.hs)
is then constructed:
```haskell
message :: PExpr
message = completeCase [case1, case2, case3]
  where case1 = (str "The target was hit.",        abs_ (sy offset $/ sy targPos) $< sy tol)
        case2 = (str "The projectile fell short.", sy offset $< exactDbl 0)
        case3 = (str "The projectile went long.",  sy offset $> exactDbl 0)
```

And, finally, an "instance model"/"theory" captures the relationship of the
defining formula and the variable with information important to the context of
the definition (i.e., the entire project):
```haskell
messageIM :: InstanceModel
messageIM = imNoDerivNoRefs (equationalModelN (nounPhraseSP "output message") messageQD)
  [qwC offset $ UpFrom (Exc, neg (sy targPos))
  ,qwC targPos $ UpFrom (Exc, exactDbl 0)]
  (qw message)
  [] "messageIM" [offsetNote, targPosConsNote, offsetConsNote, tolNote]

messageQD :: SimpleQDef
messageQD = mkQuantDef message E.message
```

Each variable in the mathematical system is defined for Drasil. However, for
some more complicated, involving say constraints or declarations of their
uncertainty, supplementing chunks may build on their relevant `QuantityDict`s.
For example, for $p_{land}$, we have some amount of uncertainty ($+/- 10%$) and
know that it should be non-negative:

```haskell
landPosUnc, ... :: UncertQ
landPosUnc = uq landPos defaultUncrt
...

..., landPos, ... :: ConstrConcept
...
landPos = constrainedNRV' (uc C.landPos (subStr lP "land") Real metre) [gtZeroConstr]
```

Note: In this above code snippet, there appears to be code duplication in the
definition of `landPos`, but the `ConstrConcept` also defines a `QuantityDict`
at the same time (i.e., there is no duplication in this case).

Once all the variables, theories, and all other pieces (of knowledge) involved
with the problem description are defined, they can be defined as part of a
single cohesive unit (called
[`SystemInformation`](../code/drasil-sysinfo/lib/SysInfo/Drasil/SystemInformation.hs)),
a [system](../code/drasil-example/projectile/lib/Drasil/Projectile/Body.hs),
with meta-information (authors, title, purpose statement, etc.):

```haskell
si :: SystemInformation
si = SI {
  _sys         = projectileTitle,
  _kind        = Doc.srs,
  _authors     = [samCrawford, brooks, spencerSmith],
  _purpose     = [purp],
  _background  = [],
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block SimpleQDef],
  _constraints = map cnstrw constrained,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}
```

Note: `_sysinfodb` and `_usedinfodb` are instances of `ChunkDB`s, which are just
intended to collect all relevant pieces of knowledge to the system but not
necessarily in any structured way, just purely in a way to retrieve individual
pieces.

<details>

<summary>Finally, once the “system” is constructed, the SRS document can be 
generated by first declaring the desired structure of the document. [Code is
hidden here for the sake of space.]
</summary>

```haskell
mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IPurpose $ purpDoc projectileTitle Verbose
      , IScope scope
      , IChar [] charsOfReader []
      , IOrgSec inModel (SRS.inModel [] []) EmptyS],
  GSDSec $ 
      GSDProg 
        [ SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList]
        , UsrChars [userCharacteristicsIntro]
        , SystCons [] []],  
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg purp []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `S.the_ofThe` phrase projectile, 
                  S "geometric layout" `S.the_ofThe` phrase launcher `S.and_` phrase target]]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub EmptyS []
      , NonFReqsSub
      ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
    AuxConsProg projectileTitle constants,
  Bibliography
  ]
```

</details>

#### Mapping

Two major sub-generators enable the production of the outputs:
1. a rendering of the Scientific Computing Software (SCS) SRS-related knowledge
   into HTML/LaTeX/JN (_DocRender_), and
2. a knowledge-extraction transformation alongside user-defined polyfills that
   transform the SCS-SRS-related knowledge into the various programming
   languages [earlier mentioned](#outputs) (_CodeGen_).

Together, these two generators form the major SCS product family compiler
(henceforth called the _SmithEtAl SCS generator_ (SmithSciSoft)). Similar to
SmithSciSoft, the two generators are defined using finer components:

1. DocRender is enabled by first building the layout using the SRS document
   description language, transforming it into the generic document language, and
   then finally rendering that artifact into the document flavours
   (HTML/LaTeX/JN).
2. CodeGen is enabled by first extracting the system of equations derived in an
   SCS SRS (SCSRS?) and, where needed, applying user-defined
   [polyfills](https://en.wikipedia.org/wiki/Polyfill_(programming)), such that
   they are “solvable” and a “code-generation” transformer may be applied that
   transforms them into a Generic Object-Oriented Language (GOOL) program, which
   can finally be rendered into various software projects with differing
   programming language options (amongst other generic software project options,
   e.g., modularity, logging, etc.).

Note that these 3 terms do _not_ appear in the code. They are only discussed for
the purposes of this document for the sake of explaining the real “story” the
code is telling.

#### Limitations

The most important limitation to note is that Drasil is currently limited to
generating code for Input-Calculate-Output (ICO) style scientific problems, with
further restrictions on what calculations and expressions it supports (e.g.,
vectors and matrices are insufficiently supported).

## Taking a Closer Look at Drasil & its Packages

Assuming that Drasil is largely focused on building SmithSciSoft (i.e., ignoring
the greater scope), we can take a look at the aforementioned 3 key points of
interest in Drasil and where they're found in the codebase.

### Inputs

The true “inputs” (i.e., in the sense that this is the _data_ that Drasil
manipulates at runtime) to Drasil are:
* `drasil-example`
* `drasil-data`
* `drasil-metadata`

The packages most relevant to _defining_ the structure of the SRS inputs and
scientific knowledge are:
* `drasil-theory`
* `drasil-lang`
* `drasil-docLang`

The packages most relevant to gathering everything nicely to tell a final
“large” story are:
* `drasil-sysinfo`
* `drasil-database`

Drasil takes inspiration from documentation-driven development. As such,
development of scientific software revolves around developing a *document*. The
document in Drasil is not visual quite like LaTeX, Word, Markdown, etc., but is
*structurally* similar to the [Smith et al. SCS SRS
template](https://github.com/smiths/capTemplate/blob/main/docs/SRS/SRS.pdf),
except that it largely focuses on the “fill-in-the-blanks” sections of the SRS
template (an abstracted SRS template of sorts). We can take any one of the
examples in [`drasil-example`](../code/drasil-example/), but we will continue
using Projectile.

#### `drasil-example`

The projects roughly take the same file structure, but that is not strictly
enforced, only the data fed into Drasil's generators (that it defines) is. The
`app/` folder contains the software generation code that “runs” Drasil against
the abstract knowledge developed in the `lib/` folder:

* `Body.hs`: Imports *all* knowledge developed by the Projectile project and
  wraps it up nicely in a `SystemInformation` and `ChunkDB` as necessary.
* `Choices.hs`: Declares how the configuration that the code generator uses
  against the project. At a meta-level, it also contains implicit knowledge
  about whether the project is capable of generating code or not. We do not try
  to generate code if the project is unfit for it.

* `Figures.hs`: Declares *external* assets (here, relative links to figure
  images) for display-use-related reasons in the generated SRSs.
* `References.hs`: Declares references to works *external* to Drasil. The bib
  style is useful for this and used.

* `Concepts.hs`: Defines arbitrary *terms* (concepts) that are used in the
  project, including quantity concepts, names, etc.
* `Assumptions.hs`, `Goals.hs`, & `Requirements.hs`: Defines *concepts* related to
  assumptions, goals, or requirements-related “fill-in-the-blanks” from the SRS
  and logical formulation of the project.

* `Unitals.hs`: Defines variables used in the project as well as their involved
  constraints, uncertainty percentages, etc.

* `DataDefs.hs`, `GenDefs.hs`, & `IMods.hs`: Defines the various “theories”
  needed in the SRS. Normally there is also a `TMods.hs` as well.
* `Derivations.hs` & `Expressions.hs`: Defines derivations and expressions used
  in the theories. Kept only to keep the earlier listed files “small.”

* `Lesson/`: Contains the Projectile Lesson Plan project. Ignored for the
  purposes of this document.

Together, these files contain the key knowledge fragments recognized and used in
the Smith et al. SRS template to decompose scientific software problems
logically.

#### `drasil-data`

`drasil-data` supplements each of Drasil's case studies (or, “examples”) with a
repository of knowledge fragments that can be imported and used. For example, it
contains theories and constants related to physics, and a myriad of concepts
related to the SRS and common concepts in science.

#### `drasil-metadata`

Similar to `drasil-data`, `drasil-metadata` contains “data,” however the nature
of the data in `drasil-metadata` is intended to be “meta” to the other packages.
Currently, that largely coincides with concepts/terminology used _in_ the
Haskell code and Drasil's version information (for use in Drasil's artifacts).

By and large, `drasil-example`, `drasil-data`, `drasil-metadata` contain
*instances* of knowledge that are used *by* Drasil to do whatever we ask of
Drasil to do (i.e., generate software artifacts). However, in order for Drasil
to understand how to work with this “external” data, we need to define their
structure (syntax). Other than these two projects, all other `drasil-*` projects
relate to inner workings and capabilities of Drasil. As related to the “inputs”
to Drasil, is largely done through the chunks defined in `drasil-theory`,
`drasil-lang`, and `drasil-docLang`.

#### `drasil-theory`

`drasil-theory` defines the concepts related to the “theory” *types* found in
the SRS:
* `DataDefinition`: Definitions that we just assume of mathematical variables in
  the project.
* `TheoryModel`: Theories from textbooks that we use to logically develop
  mathematical models of our scientific problems.
* `GenDefn`: Theories that are either refined from `TheoryModel`s but are not
  quite relevant to our final mathematical model.
* `InstanceModel`: Theories that go into the final mathematical model that,
  together, defines the mathematical problem declared in the SRS.

Note that all of these types are currently under development by Dr Smith at the
moment. Additionally, `drasil-theory` contains concepts related to each of these
theories, for use in inter-document references and discussion in the SRS.

#### `drasil-lang`

Most of the other packages were extracted from `drasil-lang`, but the action of
moving away all relevant chunks and files out of `drasil-lang` has not quite
been completed. Currently, `drasil-lang` contains a variety of chunks related
to:
* declaring concepts (`ConceptInstance`, `ConceptChunk`, `IdeaDict`),
* building natural language expressions (`Sentence`, `NounPhrase`,
  `Capitalization`, `NP`, etc.),
* declaring references to people (`People`),
* building URIs (`URI`),
* math:
  * declaring pretty mathematical symbols (`Symbol`),
  * declaring quantities (`QuantityDict`, `UnitaryChunk` [enforces units]),
  * defining quantities (`QDefinition`, `DefinedQuantityDict`, `UnitalChunk`
    [defined quantity with a unit]),
  * building mathematical expressions (`Expr`, `ModelExpr`) and literals
    (`Literal`),
  * mathematical types (`Space`),
  * building mathematical derivations (`Derivation`),
  * declaring units (`UDefn`, `UnitDefn`, `UnitEquation`),
  * constraints and uncertainties around quantities (`Uncertainty`,
    `ConstrainedChunk`, `ConstrConcept`),
  * mathematical relations (`RelationConcept`),
  * ODEs (`DifferentialModel`),
* (SRS-biased) documents: content, references, citations, etc.,
* the code generator: `CodeVarChunk`, `CodeChunk`, and
* Unique (chunk) Identifiers (`UID`).

`drasil-lang` is not used for any kind of rendering, only declaring chunks and
languages.

#### `drasil-docLang`

`drasil-docLang` declares the structure of an SRS document (declaring the
sections of the SRS) and includes a renderer for said SRS into `Doc`s and tools
to populate the SRS with information from a `SystemInformation`. This structure
is used to configure how we want to present an abstract SCS system as an SRS.

```haskell
-- | A Software Requirements Specification Declaration is made up of all necessary sections ('DocSection's).
type SRSDecl = [DocSection]

-- | Contains all the different sections needed for a full SRS ('SRSDecl').
data DocSection = TableOfContents                       -- ^ Table of Contents
                | RefSec DL.RefSec                      -- ^ Reference.
                | IntroSec DL.IntroSec                  -- ^ Introduction.
                | StkhldrSec DL.StkhldrSec              -- ^ Stakeholders.
                | GSDSec DL.GSDSec                      -- ^ General System Description.
                | SSDSec SSDSec                         -- ^ Specific System Description.
                | ReqrmntSec ReqrmntSec                 -- ^ Requirements.
                | LCsSec                                -- ^ Likely Changes.
                | UCsSec                                -- ^ Unlikely Changes.
                | TraceabilitySec DL.TraceabilitySec    -- ^ Traceability.
                | AuxConstntSec DL.AuxConstntSec        -- ^ Auxiliary Constants.
                | Bibliography                          -- ^ Bibliography.
                | AppndxSec DL.AppndxSec                -- ^ Appendix.
                | OffShelfSolnsSec DL.OffShelfSolnsSec  -- ^ Off the Shelf Solutions.
```

#### `drasil-sysinfo`

<details>

<summary>

`drasil-sysinfo` contains the definition of the `SystemInformation` chunk type,
declaring a single input-calculate-output “system” (a specific category of SCS),
which is the only kind of system that Drasil currently generates (executable)
code for.

</summary>

```haskell
-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data SystemInformation where
 SI :: (CommonIdea a, Idea a, Idea b, HasName c,
  Quantity e, Eq e, MayHaveUnit e, Quantity f, MayHaveUnit f, Concept f, Eq f,
  Quantity h, MayHaveUnit h, Quantity i, MayHaveUnit i,
  HasUID j, Constrained j) => 
  { _sys         :: a
  , _kind        :: b
  , _authors     :: [c]
  , _purpose     :: Purpose
  , _background  :: Background
  , _quants      :: [e]
  , _concepts    :: [f]
  , _instModels  :: [InstanceModel]
  , _datadefs    :: [DataDefinition]
  , _configFiles :: [String]
  , _inputs      :: [h]
  , _outputs     :: [i]
  , _defSequence :: [Block SimpleQDef]
  , _constraints :: [j] --TODO: Add SymbolMap OR enough info to gen SymbolMap
  , _constants   :: [ConstQDef]
  , _sysinfodb   :: ChunkDB
  , _usedinfodb  :: ChunkDB
  , refdb        :: ReferenceDB
  } -> SystemInformation
```

</details>

#### `drasil-database`

`drasil-database` contains the `ChunkDB` type definition, the in-memory chunk
database we use to record all chunks we use in a project. Currently, that is the
extent to which `ChunkDB` is used. However, `ChunkDB` [will
eventually](https://github.com/JacquesCarette/Drasil/issues/2873) support some
desired features:
* gradual `ChunkDB` growth,
* unrestricted types permitted (right now it has a limited number of chunk types
  that can be appended to it),
* ensuring added chunks all have their chunk dependencies also contained in the
  `ChunkDB` (i.e., recorded), and
* running “checks” not expressible with Haskell's type system against each added
  chunk (i.e., complex constraints, such as multiplicity of attributes,
  type-checking, etc.).

### Outputs

All “outputs” (software artifacts) of Drasil are rendered into
[`Doc`s](https://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint.html)
before being written to the host machine. Only a handful of Drasil's libraries
deal with external software artifacts:
* `drasil-build`
* `drasil-code`
* `drasil-gool`
* `drasil-printers`

#### `drasil-build`

`drasil-build` contains a partial (non-spec.) encoding of
[Makefiles](https://www.gnu.org/software/make/manual/make.html). Other packages
can use `drasil-build` to construct abstract Makefiles and then also use
`drasil-build` to render the Makefile ASTs into `Doc`s.

#### `drasil-code`

For the purposes of the *outputs*, `drasil-code` contains a
[copy](../code/drasil-code/lib/Language/Drasil/Code/Imperative/Doxygen/Import.hs)
of the default [Doxygen](https://www.doxygen.nl/) configuration file.

#### `drasil-gool`

`drasil-gool` contains the AST of GOOL in addition to several GOOL renderers (to
`Doc`s, not other ASTs):
* Python
* Java
* C++
* C#
* Swift

To recall, [GOOL](https://arxiv.org/abs/1911.11824) is the Generic
Object-Oriented Language that Drasil uses to compile abstract OO programs into
various OO programming language flavours.

#### `drasil-printers`

`drasil-printers` contains:
* a renderer that dumps chunk information from a `ChunkDB`,
* a [GraphViz DOT](https://graphviz.org/) language AST and `Doc` renderer,
* shallow, (math-related software)-biased encodings of HTML, Markdown, and TeX
  that produces `Doc`s, and
* an abstract (math-related software)-biased document AST and `Doc` renderer, as
  well as renderers to TeX and HTML, and from various chunk types from
  `drasil-lang` (e.g., `Expr`, `Symbol`) and other packages.

### Mapping

The following code is from Projectile's [Main generation orchestration
code](../code/drasil-example/projectile/app/Main.hs) (the important bits,
rather):
```haskell
main :: IO ()
main = do
  ...
  gen (DocSpec (docChoices SRS [HTML, TeX, JSON]) "Projectile_SRS") srs printSetting
  gen (DocSpec (docChoices Jupyter [])      "Projectile Lesson") PL.nb PL.printSetting
  genCodeWithChoices choiceCombos
  ...
```

The two `gen` IO operations correspond to DocRender, while `genCodeWithChoices`
corresponds to CodeGen.

#### `drasil-code`

`drasil-code` corresponds with CodeGen, consisting of a few important
components:
1. an encoding of ICO-style problems (and their solution schematics) called
   [`CodeSpec`](../code/drasil-code/lib/Language/Drasil/CodeSpec.hs), focused on
   their representation in “code,”
2. a [renderer of said
   encodings](../code/drasil-code/lib/Language/Drasil/CodeSpec.hs) (1) into GOOL
   programs using an off-the-shelf design and including user-provided code
   polyfills, and
3. an extraction tool that attempts to create (1) using the knowledge gathered
   in the abstract SCS system.

#### `drasil-docLang`

`drasil-docLang` corresponds to DocRender, containing a language for building
SRS documents (`SRSDecl`), tools to help the construction of them (smart
constructors), and a renderer (`mkDoc`) for the SRS documents to the generic
printing language of `drasil-printers`.

#### `drasil-gen`

`drasil-gen` wraps all of our transformers and renderers into a handful of tools
that, together, make up SmithSciSoft.

#### `drasil-printers`

As mentioned earlier, `drasil-printers` contains renderers that transform
various mathematical constructions, symbols, natural language, and other
SRS-related content into its generic document language for further printing into
one of the supported output document flavours:
* LaTeX,
* HTML,
* Jupyter Notebooks, and
* a plaintext variant for debugging purposes.

### Extras

#### `drasil-utils`

As with most programming projects, a “utilities” folder is helpful. For us, it
is important to heed two notes, however:
1. none of the utilities can involve chunks at all,
2. the utilities are restricted in scope to supplementing the standard `base`
   library for our common needs.

#### `drasil-website`

`drasil-website` is the Drasil-based project we use to generate the website. It
is a work-in-progress, pending analysis to better understand its needs and make
sure we're building the project “the Drasil way.”

#### `drasil-codeLang`

This package has not quite found its meaning yet, it currently houses only the
following type alias:

```haskell
type Comment = String
```
