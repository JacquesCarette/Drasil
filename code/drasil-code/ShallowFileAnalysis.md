# Shallow File Analysis

## `drasil-code`

Overall, I believe that before we make major architectural changes, we should:

* [ ] Reorganize `-code`.
* [ ] Break up `-code`.
* [ ] Simplify `-code`.
* [ ] Rename things in `-code`.

In particular after breaking up `-code`, we should take many smaller refactoring passes at the new packages.

Something that was very nice is how validating this analysis was to me in my previous analysis about the projectile variations and where choices are made. The problem is now how we pivot to that structure, which we will definitely need to make in many small steps.

### `./lib/Language/Drasil/GOOL.hs`

Re-export file.

### `./lib/Language/Drasil/Choices.hs`

A giant list of generation options (types, constructors, and the mega `Choices` type).

One particularly dubious thing is that it contains a list of output source code formats (`[Lang]`). If we have a single software dossier, we normally only generate _one_ programming language (in common scenarios), which suggests that this is somewhat used as a "Drasil showcase" generator more than a practical software generator.

One major issue with `Choices` is that the `Lang` type does not include any language-specific options.

### `./lib/Language/Drasil/Chunk/Code.hs`

Contains some very suspicious code:

```haskell
-- | Finds the code name of a 'CodeChunk'.
instance CodeIdea    CodeChunk where
  codeName = render . symbolDoc . codeSymb . view qc
  codeChunk = id
```

It contains other instances of `CodeIdea` for `CodeVarChunk` and `CodeFuncChunk`.

Regarding the instances, I wrote (in [#5217](https://github.com/JacquesCarette/Drasil/pull/5217)):

```haskell
-- FIXME: This file really wants to move closer to the definitions of these
-- chunks. But, we can't do that because those chunks currently reside in
-- `drasil-lang`, and `drasil-printers` (which this file relies on) depends on
-- `drasil-lang`. i.e., a cycle!
```

The dependency issue chain between `drasil-printers`, `drasil-code`, and `drasil-lang` occurs again.

It also contains `ccObjVar`, a constructor for `CodeVarChunk`, that relies on the orphaned instances mentioned earlier. This code belongs next to the definition of `CodeVarChunk`.

### `./lib/Language/Drasil/Chunk/CodeBase.hs`

As of [#5217](https://github.com/JacquesCarette/Drasil/pull/5217), this file only contains:

```haskell
-- | Get a list of 'CodeChunk's from an equation.
codevars :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars e m = map (varResolve m) $ eDep e

-- | Get a list of 'CodeChunk's from an equation (no functions).
codevars' :: CodeExpr -> ChunkDB -> [CodeVarChunk]
codevars' e m = map (varResolve m) $ eDep' e

-- | Make a 'CodeVarChunk' from a 'UID' in the 'ChunkDB'.
varResolve :: ChunkDB -> UID -> CodeVarChunk
varResolve  m x = quantvar (findOrErr x m :: DefinedQuantityDict)
```

1. [ ] Nothing about this file is about `CodeBase`. It should be renamed to something else, e.g., `Extractors`.
2. [ ] What's particularly odd about `varResolve` as well is that it `DefinedQuantityDict` into a `CodeVarChunk`. How does it gain any type information? What happened to the Double/Float representation knowledge? That doesn't appear anywhere here.
3. [ ] `varResolve` makes me question both `quantvar` and `CodeVarChunk`. It suggests we can delete `CodeVarChunk` without any real loss of information. Or perhaps that we're not using `CodeVarChunk` in a meaningful way.
4. [ ] `varResovle` also indicates that we are not inserting actual `CodeVarChunk`s into the `ChunkDB` (bad, because we try to retrieve it later).

### `./lib/Language/Drasil/Chunk/CodeDefinition.hs`

Contains two interesting data types:

```haskell
-- | The definition may be specialized to use ODEs.
data DefinitionType = Definition | ODE

-- | A chunk for pairing a mathematical definition with a 'CodeChunk'.
data CodeDefinition = CD { _cchunk   :: CodeChunk
                         , _def      :: CodeExpr
                         , _auxExprs :: [CodeExpr] -- FIXME: This is not what it sounds like. This is a list of input symbols.
                         , _defType  :: DefinitionType
                         }
makeLenses ''CodeDefinition
```

1. [ ] Its `HasChunkRefs` instance is incorrect!!!
    ```haskell
    instance HasChunkRefs CodeDefinition where
    chunkRefs cd = chunkRefs (cd ^. cchunk)
    {-# INLINABLE chunkRefs #-}
    ```
2. [ ] The name means very little.

This type is really used for declaring which quantity will be defined by which formula or that it should be solved by an ODE solver. For ODEs, it does something bizarre; it is used to gather a list of input variables to be used in the ODE solver. This is very hacky. Said list should come from elsewhere (why do code-level variables leak into this when we don't have a concrete solver in scope yet?).

This type is used in `ICOSolutionSearch.hs`, which indicates that we are abusing it. We need another type to represent steps in a calculation algorithm for an ICO problem.

### `./lib/Language/Drasil/Chunk/ConstraintMap.hs`

Contains a map of `UID`s to constraints. Instead of attaching them here, it would be good for them to be attached to the variable definitions. One thing that's dubious is that it knows about physical- and software-imposed constraints. That information looks like it should be erased by now, or perhaps be renamed to "semantic" and "solution."

### `./lib/Language/Drasil/Chunk/NamedArgument.hs`

Contains a single data definition:

```haskell
-- | Any quantity can be a named argument (wrapper for 'DefinedQuantityDict'),
-- but with more of a focus on generating code arguments.
newtype NamedArgument = NA {_qtd :: DefinedQuantityDict}
makeLenses ''NamedArgument
```

`NamedArgument` inherits _everything_ from `DefinedQuantityDict`.

1. [ ] Why would it not inherit from a `CodeVarChunk` instead? Or be a variant of one? Is it really necessary?
2. [ ] It looks very sparingly used:
    ```console
    rg "narg\b" -ths
    
    drasil-code/lib/Data/Drasil/ExternalLibraries/ODELibraries.hs
    27:import Language.Drasil.Chunk.NamedArgument (NamedArgument, narg)
    102:mthdArg = narg $ implVar (mkUid "method_scipy") (nounPhrase
    106:atolArg = narg $ implVar (mkUid "atol_scipy") (nounPhrase
    110:rtolArg = narg $ implVar (mkUid "rtol_scipy") (nounPhrase
    
    drasil-code/lib/Language/Drasil/Code.hs
    74:import Language.Drasil.Chunk.NamedArgument (NamedArgument, narg)
    
    drasil-code/lib/Language/Drasil/Chunk/NamedArgument.hs
    7:  narg) where
    51:narg :: (Quantity q, MayHaveUnit q, Concept q) => q -> NamedArgument
    52:narg = NA . dqdWr
    ```

`NamedArgument` looks like an attempt to capture something meaningful, but I'm not sure if a `newtype` wrapper around a `DefinedQuantityDict` is enough.

### `./lib/Language/Drasil/Chunk/Parameter.hs`

Similar to `NamedArgument.hs`, contains data types:

```haskell
-- | Determines whether a parameter is passed by value or by reference.
data PassBy = Val | Ref

-- | Chunk representing a parameter.
data ParameterChunk = PC {_pcc :: CodeChunk
                         , passBy :: PassBy}
makeLenses ''ParameterChunk
```

These inherit everything `CodeChunk`. Alternative to my earlier suggestion regarding `NamedArgument`, we should check to see if we can replace `NamedArgument` with a variant of `ParameterChunk`.

### `./lib/Language/Drasil/Code.hs`

Reexport file. Contains far too many things. It is very unclear _why_ it exports everything in that single file, and it almost definitely exports things that should be split off into other re-export modules. For example, manual module-building is uncommon work (appearing only in `glassbr`) and involves very delicate work. It should likely be split off into a separate module that re-exports everything related to manual module-building.

### `./lib/Language/Drasil/Code/CodeQuantityDicts.hs`

Contains the basic `DefinedQuantityDict`s needed to generate the examples and a list of them (`codeDQDs`) for use in `drasil-gen/../CommonKnowledge.hs`. 

1. [ ] These `codeDQDs` should probably only be inserted during _code-generation-time_. i.e., it should be removed from `drasil-gen` and moved to the code generation function (wherever that may be).

### `./lib/Language/Drasil/Code/Lang.hs`

Contains a single type: `Lang`, indicating the supported generation options that `drasil-code`'s renderer offers. Note that this is related to an _option_. What we really need from `drasil-code` is a namespace called `Renderer`. `Renderer` should contain all the options and doodads necessary to configure it. Then we need a core AST/IR for `drasil-code` separated into another namespace. This way, we can concretely say that this is or is not a part of the AST/IR.

### Input Data Description Language

3.5 files that are heavily related.

#### `./lib/Language/Drasil/Code/DataDesc.hs`

`DataDesc.hs` contains a language (`DataDesc`) for describing the shape of program input files. We use this in two ways:

1. For reading in our own "sample input" files and writing them to disk later.
2. For generating input readers in our generated programs that can read our designated input files.

We also have a new version of `DataDesc` (`DataDesc'`) that is currently unused. I'm not sure what the rationale for it is yet, but we can probably find something in a commit message or an issue.

Now, `DataDesc` is sort of odd because it only contains layout information. We can think of it like a flexible specific of a TSV-like file.

It looks to me like `DataDesc` has a coherent design somewhere, but I don't immediately understand it. The code surrounding it is also very difficult to read, so this will need to be done. At face value, `DataDesc` looks like an over-engineered to a relatively straightforward problem?

Also, our current usage of `DataDesc` locks us into strictly using it. Programs often read CSVs, TSVs, pandas/pickle, YAML, and JSON.

#### `./lib/Language/Drasil/Code/Imperative/WriteInput.hs`

Contains code related to _writing_ a sample input file (knowledge held within Drasil).

#### `./lib/Language/Drasil/Code/Imperative/ReadInput.hs`

Contains code related to _reading_ a sample input file into Drasil.

Together with `WriteInput.hs`, we appear to parse a sample input file with Drasil using a `DataDesc` and then write the same data out using the same `DataDesc` to our generated folders.

#### `./lib/Language/Drasil/Code/Imperative/Import.hs` (The $.5$)

This is the $.5$ because it is a very large file and only contains:

```haskell
-- | Generates a function that reads a file whose format is based on the passed
-- 'DataDesc'.
genDataFunc :: (OOProg r) => Name -> Description -> DataDesc -> GenState (SMethod r)
genDataFunc nameTitle desc ddef = do
  let parms = getInputs ddef
  bod <- readData ddef
  publicFunc nameTitle void desc (map pcAuto $ quantvar inFileName : parms)
    Nothing bod
```

This code is a bit confusing. It's not quite clear how the generated code fits into the bigger picture of the program.

### `./lib/Language/Drasil/Code/Imperative/ConceptMatch.hs`

I already wrote about this in [#1862](https://github.com/JacquesCarette/Drasil/issues/1862#issuecomment-3009017292). The idea is really good. The idea is that we can substitute our concepts with other ones, such as language-native concepts.

For example, we can have our own implementation of `sin` or we can use the one from `java.lang.Math`. Currently we lock ourselves into a single `sin` implementation with GOOL, but we should be able to modify this.

In some sense, this has the potential to be one of the most important files in `drasil-code` (if not the most).

### `./lib/Language/Drasil/Code/Imperative/Doxygen/Import.hs`

This file is massive, containing a Doxygen config with only a few options filled in. We should look into trimming it down a bit, at least deleting the Doxygen config comments. This file appears to be directly extraced from a copy-pasted Doxygen config. What we really should have is an encoding (a record or a map) of a Doxygen config, with a bunch of switches for each option. 

### Two Most Important Files in `drasil-code`

In these two files, the "invisible programmer," the one that knows how to convert an ICO problem flexibly encoded in the SRS, is captured. Their actions are sound for our problems, but it's not clear enough why they do what they do nor how their work functions. I say "invisible programmer" because it looks like a lot of tacit knowledge is being captured in these two files but we're only seeing an operationally functional axis of that knowledge. We need to decompose it a bit more to reveal where more choices can be exposed.

#### `./lib/Language/Drasil/CodeSpec.hs`

Contains a "Code Specification" (`CodeSpec`) that contains all the information necessary for the generator to function. The smart constructor for `CodeSpec` has type: `codeSpec :: S.SmithEtAlSRS -> Choices -> CodeSpec`.

The problem with `CodeSpec` is that it captures too many things at once and is too aware of the ICO nature without that being explicitly written anywhere. We need to make this more explicit.

* [ ] Contains an old and a new version of `CodeSpec`. This was part of an unfinished refactor that we should just revert.
* [ ] The type signature is peculiar. Why should the code generator know about the SRS? We should move the "invisible programmer" elsewhere (e.g., `drasil-gen`). The invisible programmer is the one that knows that the SRS will always contain an ICO problem and that `CodeSpec` is well-suited to solving ICO problems.
* [ ] Mixes 3 levels that should likely be split:
    1. A pure mathematical problem.
    2. A concrete software-based calculation problem reframing the pure mathematical problem in ... simplified, calculable terms.
    3. A specific high-level software solution to the concrete calculation problem.

(I'm going to need to refine my terminology used for the 3rd point here. The best way is likely to take one of the examples and run it through this model/problem framing to see how it works.)

#### `./lib/Language/Drasil/ICOSolutionSearch.hs`

About solving the execution order for an ICO problem.

Note:

1. Does not work with ODEs. Need to investigate how this is really working.
2. Needs to drop unused variables. Some calculations are unnecessary (!).

* [ ] `ICOSolutionSearch` is good but should be opt-in. A user should be able to override this or to create a "concrete software-based calculation problem" without encoding SRS-related information.
* [ ] It should be moved to `drasil-gen`, where I hope the "invisible programmer" will live.

### Manually Constructed Code

#### `./lib/Language/Drasil/Mod.hs`

This file contains an AST for building modules (heavily OO-inspired class files), objects, and function bodies.

* [ ] This file should not be re-exported through `Language.Drasil.Code` on the basis that it circumvents the normal activities of `drasil-gen`.
* [ ] The file is very OO-oriented without explicitly noting that. We should make that more clear.
* [ ] We should investigate if this file is strictly necessary _after_ dealing with the first issue above (this way we can see where it is imported more clearly).

### External Libraries

These 3 files deserve more than a shallow file analysis. They are outside the scope of this work for now.

#### `./lib/Language/Drasil/Code/ExternalLibrary.hs`

This code contains sensible documentation. I'm not going to think about this file too much right now but it looks not too bad. Low priority.

#### `./lib/Language/Drasil/Code/ExternalLibraryCall.hs`

There are a myriad of smart constructors that do very little. We should look more carefully into this file.

#### `./lib/Language/Drasil/Code/ExtLibImport.hs`

Supports the previous two files.

### ODEs

I'm going to ask a very silly question: why are ODEs ever related to code generation?!

From the point of view of generating code, we should only see steps as more computations. Differentiating between ODEs and plain formulas means we haven't erased enough/rendered enough information yet.

Currently, to generate code, we make it _work_, but the way we make it work is questionable. This again goes back to the invisible programmer mentioned earlier.

This deserves more than a shallow file analysis.

### `./lib/Language/Drasil/Data/ODEInfo.hs`

Contains basic information for how an ODE should be solved. This files deserve more than a shallow file analysis. At a surface level, it contains a record, a smart constructor, and a function:

```haskell
-- | Create well-formatted ODE equations which the ODE solvers can solve.
createFinalExpr :: DifferentialModel -> [CodeExpr]
```

### `./lib/Language/Drasil/Data/ODELibPckg.hs`

Contains an encoding of an "ODE library package" that is usable for solving some ODE problems. There's one assumption about these instances that is undocumented: that the ODE packages each have good support for each kind of ODE problem we support. Each one has different capabilities. We can enforce these capability restrictions with runtime `error`s.

### `./lib/Language/Drasil/Code/Imperative/GenODE.hs`

Surprisingly carries very little about generating ODE-related code. Rather, it contains 1 function that is used to validate) that a language is supported by a specific ODE library and contains 2 functions that create messages (`Choice`s-related errors or design-logging, for `designLog.txt` files).

### Building and Rendering Comments

We should consider creating a namespace for comments (which contain human-readable descriptions).

#### `./lib/Language/Drasil/Code/Imperative/Comments.hs`

Contains 5 functions related to _rendering_ descriptions, units, comments, etc. into text. This file has more to do with rendering comments than building or encoding them.

#### `./lib/Language/Drasil/Code/Imperative/Descriptions.hs`

Contains renderers for generating descriptive comments about various areas of the generated code. There is a lot of tacit information about the implementation of the code generator that lives here.

* [ ] We should try using the `LayoutObj` language instead of raw `String`s or `Sentence`s to write comments. This will give us more rendering options (hopefully).
* [ ] This file is rather complete, but I'm not quite sure why these descriptions are placed here instead of next to the actual code related to the descriptions.
* [ ] Should the information captured in this file be captured in chunks? I.e., the generated descriptions?

### Code "Renderer"s (Software Dossier "helpers")

The same analysis applies to the following files:

* `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/CppRenderer.hs`
* `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/CSharpRenderer.hs`
* `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/JavaRenderer.hs`
* `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/JuliaRenderer.hs`
* `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/MatlabRenderer.hs`
* `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/PythonRenderer.hs`
* `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/SwiftRenderer.hs`

An implementation of a "software dossier" custom to that specific language.

* [ ] Are the monads used anywhere?
* [ ] The typeclass usage for `SoftwareDossierSym` looks like over-engineering. We should reevaluate the design.

#### `./lib/Language/Drasil/Code/Imperative/GOOL/LanguageRenderer/LanguagePolymorphic.hs`

Contains:

1. 3 renderers for software-dossier-related things (Doxygen, README, Makefile) with options.
2. A default doxygen config.
3. Two helper functions:
    * [ ] `noRunIfLib :: ImplementationType -> Maybe Runnable -> Maybe Runnable`. Used in the definition of `makefile` in each of the aforementioned `XRenderer.hs` files. Hints at a design issue: we don't need to copy-paste this code in each `XRenderer.hs` file, but we should have some single piece of code that goes to those languages and asks for one compiler/renderer function.
    * [ ] `docIfEnabled :: [Comments] -> DocConfig -> Maybe DocConfig`. I won't comment on this one. It just needs some more analysis, but it looks like we should try to delete this function.

### Software Dossier

We have [README](#readme-generation) and [Makefile](#makefile-generation) ASTs and generators not directly placed in the `SoftwareDossier` namespace that should be moved there.

#### `./lib/Language/Drasil/SoftwareDossier/SoftwareDossierSym.hs`

```haskell
-- | Members of this class must have a doxygen configuration, ReadMe file,
-- omptimize doxygen document, information necessary for a makefile, and
-- auxiliary helper documents
class SoftwareDossierSym r where
  doxConfig :: String -> SoftwareDossierState -> Verbosity -> Maybe (r FileLayout)
  readMe ::  ReadMeInfo -> r FileLayout

  optimizeDox :: r Doc

  makefile :: [FilePath] -> ImplementationType -> [Comments] -> SoftwareDossierState ->
    ProgData -> r FileLayout

  unReprDoc :: r Doc -> Doc
```

#### `./lib/Language/Drasil/SoftwareDossier/FileNames.hs`

Contains the following:

```haskell
-- | Common syntax for several renderers.
doxConfigName, makefileName, sampleInputName, readMeName :: String
-- | "doxConfig".
doxConfigName = "doxConfig"
-- | \"Makefile\".
makefileName = "Makefile"
-- | "input.txt".
sampleInputName = "input.txt"
-- | "README.md".
readMeName = "README.md"
```

There's a few things we can do here:

1. [ ] Move their content to chunks.
2. [ ] Move their content to generation options of the software dossier (excluding `sampleInputName`).
3. [ ] Make them configurable.

What I really see with this file is spread our code around about unconventional boundaries. I don't immediately understand why these files were broken up the way they were. We have tacit design decisions here that we never made explicit, and we aren't really consistent enough to resurrect the design.

#### README Generation

##### `./lib/Language/Drasil/Code/Imperative/README/Core.hs`

Contains an encoding of an idiomatic README file, containing things we would want to see in a "good" README. "Good" here meaning "containing the things we would normally expect or like to see in a README file."

* [ ] Rename `ReadMeInfo` to `README` or `IdiomaticREADME`.
* [ ] I know this data type is at least partially influenced by a few papers. We should note that here.

##### `./lib/Language/Drasil/Code/Imperative/README/Render.hs`

Contains the Markdown renderer for the README file. It doesn't actually use a proper Markdown IR, it just immediately jumps to rendering Markdown code. This is a known issue, a part of [#4989](https://github.com/JacquesCarette/Drasil/issues/4989).

#### Makefile Generation

Both of these files need to be moved to a `Makefile` namespace, preferably within `SoftwareDossier` for now (even though I don't think that will be its final home).

##### `./lib/Language/Drasil/Code/Imperative/Build/AST.hs`

This file contains a mix of data types that together resemble the common components of a `Makefile` (arguably something more generic than a `Makefile`, but the data types are specialized to `Makefile`s). However, the overall structure is difficult to understand. This code can be cleaned up (re-organized and comments updated) before we decide on how to "fix" it.

##### `./lib/Language/Drasil/Code/Imperative/Build/Import.hs`

* [ ] The filename is confusing. It does not import something and convert it into a `Makefile`, it generates a `Makefile` `Doc` using `drasil-makefile`. We should rename it to `Render.hs` or `Generate.hs`.

### The Code Generator

<!-- TODO: The below is incomplete. -->

#### `./lib/Language/Drasil/Code/Imperative/DrasilState.hs`

One piece of code that jumped out at me:

```haskell
-- | Gets exported inputs for InputParameters module.
-- If there are no inputs, no input variables are exported.
-- If 'Unbundled', no input variables are exported.
-- If 'Unmodular' and 'Bundled', module is named after program.
-- If 'Modular' and 'Bundled', inputs are exported by InputParameters module.
-- In 'Unmodular' 'Bundled' and 'Modular' 'Bundled' cases, an InputParameters
-- constructor is generated, thus "InputParameters" is added to map.
getExpInput :: Name -> Choices -> [Input] -> [ModExp]
getExpInput _ _ [] = []
getExpInput prn chs ins = inExp (modularity $ architecture chs) (inputStructure $ dataInfo chs)
  where inExp _ Unbundled = []
        inExp Unmodular Bundled = (ipName, prn) : inVarDefs prn
        inExp Modular Bundled = (ipName , ipName) : inVarDefs ipName
        inVarDefs n = map ((, n) . codeName) ins
        ipName = icNames chs InputParameters
```

There's so many things that are implicitly assumed here!

1. Why are we assuming that we're going to generate an "input parameters" module? How is that at all relevant to "code"? Reading in values from various sources is sensible, but that's on the level of code blocks. This architectural choice of wrapping all input values (i.e., bundling them) is a choice that should have already been processed. In other words, `InputParameters` should be a module that carries a single function `read_inputs` that does the reading and bundling into a `struct`, but (!), this is a design decision that should have been made earlier.
2. What makes this generator so special that it needs special treatment? The function body is very similar to other local functions.

Aside: The bundling/unbundling feature is good and bad. One on hand, it is sensible for simple programs, but in larger projects, we often read from multiple input files, as is the case in `GlassBR`. We should handle this better, and generally refine our notion of an "input." For example, an input can be from stdin, a file, program arguments, a network connection, or just be called an input with a comment such as in the context of interactive notebooks.

The rest of this file contains things related to `SoftwareDossier`-related 'choices'. This file deserves more than a shallow file analysis because it contains a mix of things that look over-engineered and under-engineered. We will need time to delicately disentangle what's going on here.

#### `./lib/Language/Drasil/Code/Imperative/FunctionCalls.hs`


#### `./lib/Language/Drasil/Code/Imperative/GenerateGOOL.hs`

This file needs to be broken up and deleted. It aggregates some of the renderers related to the software-dossier-related things (i.e., `Makefile`s, `Doxygen`, `README`) and then smart constructors that sit atop GOOL. I wonder if some of these things are better off being in `drasil-gool`. 

```haskell
data ClassType = Primary | Auxiliary
```

This file also defines the above datatype for differentiating between a "primary module" from "auxiliary modules." I don't understand the difference between a primary and auxiliary module. From a very quick scan, it looks like what it really wants to establish is the difference between a file containing a program entry point or not.

#### `./lib/Language/Drasil/Code/Imperative/Generator.hs`



#### `./lib/Language/Drasil/Code/Imperative/Helpers.hs`

Contains two functions:

```haskell
-- | Puts a state-dependent value into a singleton list.
liftS :: State a b -> State a [b]
liftS = fmap (: [])

-- | Converts a 'ScopeType' to a 'Scope'
convScope :: (SharedProg r) => ScopeType -> r ScopeData
convScope Local  = local
convScope Global = global
convScope MainFn = mainFn
```

* [ ] `convScope` should be moved closer to the definition of `ScopeType`.
* [ ] `liftS` looks ok, but it's only used in two modules. We should see if we can move it to one of them.

#### `./lib/Language/Drasil/Code/Imperative/Import.hs`

#### `./lib/Language/Drasil/Code/Imperative/Logging.hs`

#### `./lib/Language/Drasil/Code/Imperative/Modules.hs`

#### `./lib/Language/Drasil/Code/Imperative/Parameters.hs`

#### `./lib/Language/Drasil/Code/Imperative/SpaceMatch.hs`

#### `./lib/Language/Drasil/Code/PackageData.hs`
