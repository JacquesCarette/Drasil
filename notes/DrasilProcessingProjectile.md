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
towards an object, describing calculations required to determine if a "hit"
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
  * the work-in-progress Jupyter Notebooks (JN).
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
2. encodings of all related ["knowledge
   fragments"](../code/drasil-example/projectile/lib/Drasil/Projectile/) needed
   that go into description of the SRS.

The "knowledge fragments" (also called "chunks") capture the structure of
various pieces of information using Embedded Domain-Specific Languages (eDSLs)
hosted in Haskell. For example, a "quantity" is captured using
[`QuantityDict`s](../code/drasil-lang/lib/Language/Drasil/Chunk/Quantity.hs):
```haskell
data QuantityDict = QD { _id' :: IdeaDict
                       , _typ' :: Space
                       , _symb' :: Stage -> Symbol
                       , _unit' :: Maybe UnitDefn
                       }
```

`QuantityDict`s capture a few ideas related to "quantities:"
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

<summary>Finally, once the "system" is constructed, the SRS document can be 
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
   they are "solvable" and a "code-generation" transformer may be applied that
   transforms them into a Generic Object-Oriented Language (GOOL) program, which
   can finally be rendered into various software projects with differing
   programming language options (amongst other generic software project options,
   e.g., modularity, logging, etc.).

Note that these 3 terms do _not_ appear in the code. They are only discussed for
the purposes of this document for the sake of explaining the real "story" the
code is telling.

## Taking a Closer Look at Drasil & its Packages

### Inputs

The true "inputs" to Drasil are:
* `drasil-example`
* `drasil-data`

The packages most relevant to gathering everything nicely to tell a final
"large" story are:
* `drasil-sysinfo`
* `drasil-database`

The packages most relevant to _defining_ the structure of the SRS inputs and
scientific knowledge are:
* `drasil-theory`
* `drasil-lang`
* `drasil-docLang`

### Outputs

<hr>

TODO: Discuss:
* `drasil-build`
* `drasil-gool`
* `drasil-printers`

<hr>

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
here more so correspond to

<hr>

TODO:
* Discuss the flow of each of the "big" generators at a finer grain, discussing
  the packages related to them.
* `drasil-code`
* `drasil-codeLang`
* `drasil-gen`
* `drasil-metadata`
* `drasil-printers`

<hr>

### Extras

<hr>

TODO: Discuss:
* `drasil-utils`
* `drasil-website`

<hr>
