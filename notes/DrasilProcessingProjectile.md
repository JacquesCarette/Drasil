# Understanding Drasil's implementation through examining how Drasil processes case studies

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

However, it is not assumed that:
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

Given a target position distance ($p_{target}$, in $m$ from the launcher), and
launch speed ($v_i$) and angle ($\theta$), we can calculate the landing position
of the projectile ($p_{land}$) and determine if the projectile successfully hit
the target ($h$) within a reasonable tolerance ($\epsilon$) using the following
system of equations:
1. $p_{land} = \frac{2 v_i \sin{}(\theta{})\cos{}(\theta{})}{g}$
2. $h = |p_{land} - p_{target}| < \epsilon$

### Drasil

Modelled after an original, manually created Software Requirements
Specifications (SRS) and corresponding software artifacts, the Projectile case
study was reconstructed in Drasil.

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

<hr>

TODO:
* Discuss theories
* Discuss the SRS document layout description
* Discuss `ChunkDB`
* Discuss `SystemInformation`
* Discuss code polyfills (can't do this with Projectile, GlassBR is better, but
  DblPend or SglPend is probably best)

<hr>

#### Mapping

The generation of the outputs is enabled through two major sub-generators:
1. a rendering of the Scientific Computing Software (SCS) SRS-related knowledge
   into HTML/LaTeX/JN (_DocRender_), and
2. a knowledge-extraction transformation alongside user-defined polyfills that
   transform the SCS-SRS-related knowledge into the various programming
   languages [earlier mentioned](#outputs) (_CodeGen_).

Together, these two generators form the major SCS product family compiler
(henceforth called the _SmithEtAl SCS generator_ (SmithSciSoft)). Similar to
SmithSciSoft, the two generators are defined using finer components:

1. DocRender is enabled by first building the layout using the SRS document
   language description language, transforming it into the generic document
   language, and then finally rendering that artifact into the document flavours
   (HTML/LaTeX/JN).
2. CodeGen is enabled by first extracting the system of equations derived in an
   SCS SRS (SCSRS?) and, where needed, applying user-defined polyfills, such
   that they are "solvable" and a "code-generation" transformer may be applied
   that transforms them into a Generic Object-Oriented Language (GOOL) program,
   which can finally be rendered into various software projects with differing
   programming language options (amongst other generic software project options,
   e.g., modularity, logging, etc.).

Note that these 3 terms do _not_ appear in the code. They are only discussed for
the purposes of this document for the sake of explaining the real "story" the
code is telling.

## Taking a Closer Look at Drasil

### Inputs

<hr>

TODO: Discuss:
* `drasil-example`
* `drasil-sysinfo`
* `drasil-database`
* `drasil-theory`
* `drasil-lang`
* `drasil-data`
* `drasil-docLang`

<hr>

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
