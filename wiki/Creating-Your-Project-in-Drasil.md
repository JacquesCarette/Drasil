# Contents

1. [Tips to Get Started](#tips-to-get-started)
    - [Getting Started](#getting-started)
    - [Sub-packages in Drasil](#sub-packages-in-drasil)
2. [Creating Your Project in Drasil](#creating-your-project-in-drasil)
    - [Adding a New User and Project Sub-Folder](#adding-a-new-user-and-project-sub-folder)
    - [Create a Blank Software Requirements Specification (SRS) Template](#create-a-blank-srs-template-for-your-project)
    - [Modifying `Main.hs`](#modifying-mainhs-for-your-software)
    - [Create Sections in `Body.hs`](#creating-sections-in-your-srs-template-within-bodyhs)
    - [Update `yaml` and `cabal` Files](#update-yaml-and-cabal-files)
    - [Build Your Project - Run `make`](#build-your-project--run-make)
    - [Updating Stable](#updating-stable-folder-in-codestable)
    - [GitHub Actions](#github-actions--builds--tests)
    - [Logging](#logging)
3. [Adding Sections to SRS Body](#adding-sections-to-srs-body)
    - [Adding Introduction Section](#adding-introduction-section)
    - [Adding Specific System Description (SSD) Section](#adding-specific-system-description-ssd-section)
    - [Adding Goal Statements Section](#adding-goal-statements-section)
    - [Adding Assumptions Section](#adding-assumptions-section)
    - [Adding Theoretical Models Section](#adding-theoretical-models-section)
    - [Adding General Definitions Section](#adding-general-definitions-section)
    - [Adding Data Definitions and Acronym Section](#adding-data-definitions-and-acronym-section)
    - [Adding Instance Models Section](#adding-instance-models-section)
    - [Adding Constraints and Properties of a Correct Solution Section](#adding-constraints-and-properties-of-a-correct-solution-section)
    - [Adding Requirements Section](#adding-requirements-section)
    - [Adding Non-Functional Requirements Section](#adding-non-functional-requirements-section)
    - [Adding Likely and Unlikely Changes](#adding-likely-and-unlikely-changes)
    - [Adding Other Sections](#adding-other-sections)
    - [Adding Traceability Section](#adding-traceability-section)
    - [Notes](#notes)
4. [Code Generation](#code-generation)
    - [Modify `Main.hs`](#modify-mainhs)



# Tips to Get Started

1. You will be working mainly in the code folder.
2. There are several sub-packages that you will require to create your software in Drasil. 
3. Many scientific theories, concepts, quantities and units commonly used in scientific problems are already coded in Drasil, you can reuse them as required. E.g Newton’s law of motion theory, units like meters, seconds, and so on. Drasil also contains concepts like gravitational acceleration, kinetic energy, velocity, and so on.
4. You will be using branches and pull requests for your project. See the [Contributor's Guide](Contributor's-Guide) for more details
5. In addition to this manual, you can use example projects as your cheat sheet to build your own project. 
6. Refer to the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/index.html) for Drasil. A variant with [fully exposed modules](https://jacquescarette.github.io/Drasil/docs/full/index.html) is also available.
7. We would like to keep improving this user guide based on your feedback. Please create issue(s) on GitHub with what you think is missing, unclear, or should be included as you follow this guide. You can assign issues to @JacquesCarette or @smiths.


## Getting Started: 

- Instructions on how to build a virtual machine for [Linux on Windows](https://docs.microsoft.com/en-us/windows/wsl/install-win10) (optional)
- Instructions on how to fork the Drasil repo and enable [continuous integration](https://gitlab.cas.mcmaster.ca/smiths/cas741/-/tree/master/Tools/VM_Instructions). This is required to create your project in Drasil. Also check out this [pull request](https://github.com/JacquesCarette/Drasil/pull/2461) for more information on setting up your own bot for GitHub Actions.
- [Setting up your workspace](https://github.com/JacquesCarette/Drasil/wiki/New-Workspace-Setup)

## Sub-packages in Drasil:
- `drasil-lang` contains language primitives focused on mathematical
knowledge capture. This package also includes a language for symbols (`Symbol`) and a language for mathematical expressions embedding symbols (`Expr`). It describes the layout of a document and much more.

- `drasil-data` contains common (reusable) knowledge such as mathematical constants and documentation concepts. You can add new concepts here.

- `drasil-utils` contains common utilities used by non-`drasil-lang` sub-packages.

- `drasil-database` contains SystemInformation and a chunk database
structure (`ChunkDB`).

- `drasil-docLang` contains a DSL describing the Software Requirement Specification (SRS) template. It includes a translation routine for converting to a Document DSL from `drasil-lang`.

- `drasil-theory` contains chunk types for describing refinements of mathematical theories and instantiation of definitions.

- `drasil-code` contains a DSL for describing (and generating) object-oriented code. 

- `drasil-printers` contains set of routines to translate `drasil-lang` Documents to a common markup language such as HTML or LATEX.

- `drasil-gen` defines the entry point for artefact generation.

- `drasil-example` contains a set of examples maintained in Drasil to demonstrate the capabilities of the language. For consistency, you can create a sub-folder for your project here. You will be working mostly in your new subfolder.

- `drasil-website` generates the homepage for the Drasil website.

# Creating Your Project in Drasil

**Note: The instructions of this tutorial were last updated for a [commit on 29 Oct 2022](https://github.com/JacquesCarette/Drasil/commit/b555167ce688040b766742b03442fbf7af750087).**

After all installation tasks and forking is completed and you are able to run existing example projects in Drasil, you are ready to create your own. You can use [Visual Studio Code](https://code.visualstudio.com/), [Sublime](https://www.sublimetext.com/) or any other editor that you are familiar with.
- VS Code makes it easier to search folders and sub-folders.
- Sublime creates links to occurrences of parameters or functions used within your package and other example packages (it only create links to files open files on your machine when you point with your cursor).


## Adding a New User and Project Sub-Folder

1. If you are a new user, add your information `Peoples.hs` in `code/drasil-data/lib/Data/Drasil/People.hs`. Create a new row and follow the same format to add your name, using the different [`person`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:person) functions as needed.

2. Now, you will need to create a new sub-folder for your project. Navigate to `/code/drasil-example/` and create your new folder.

3. Make a copy of template folder into the new folder and rename the new folder to your project name. Make sure you give your project an intuitive name. For example, Double Pendulum can be represented as DblPendulum, Projectile is Projectile, Solar Water Heating System is SWHS, etc. (note – project name should be one word, no spaces).

4. Import module `Data.Drasil.People` into `Body.hs`. This is the file where you added your name. This will be used to include your name as an author on your generated SRS document. E.g. `import Data.Drasil.People (brenda)`

5. Decide a name for your software system and define it as a Common Idea ([`CI`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:CI)) in a new file named `Concepts.hs`. For example, gamePhysics would be represented with:
```Haskell
gamePhysics :: CI
gamePhysics = commonIdeaWithDict "gamePhysics" (pn "game physics library") "Game Physics" [physics]
```
For more information, please see the [documentation](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:commonIdeaWithDict) for `commonIdeaWithDict`.

6. In `Body.hs`, start constructing your chunk database (`ChunkDB`) in the `symbMap` function by adding your program name (e.g `nw  “your software name”`). Make sure the parameter you are adding here tallies with the value of the `_sys` record in defined in `si` in `Body.hs`. `si` holds all of your system's information as a `SystemInformation` record type.

7. Now you are ready to start constructing your SRS.

## Create a Blank SRS Template for Your Project:

1. In your newly created project folder, open `Body.hs`. Modify the file to reflect your project name by replacing all instances of the word 'Template' with your project name. You can perform a find and replace operation to complete this.

2. Go to the `si` (`SystemInformation`) section of the code in `Body.hs` and populate the following fields with your own functions: 
```Haskell
_sys         = example --(will become the name of your software system e.g. dblpendulum)
_authors     = [authorName] --(will become the same name you added to People.hs)
```
For reference, the `si` data is used to hold global information about a system.
```Haskell
si :: SystemInformation
si = SI {
  _sys         = example,
  _kind        = Doc.srs,
  _authors     = [authorName],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = [] :: [InstanceModel],
  _datadefs    = [] :: [DataDefinition],
  _configFiles = [],
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _defSequence = [] :: [Block QDefinition],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [QDefinition],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}
```

3. Remember to import your software system name from `Concepts.hs` into `Body.hs` as well as all other relevant modules. Once you have done this, you may totally remove the `example` and `authorName` functions from `Body.hs`. There should be no need for these functions anymore as the second step renders them unneeded. If there are, just replace them with the ones you created.

## Modifying `Main.hs` for Your Software
		       
1. Update your `Main.hs` file: Your main module will load all the other modules in your project to build and generate your SRS.
2. Open the `Main.hs` file in the current folder and replace all instances of `template` with your project name, save and close.


## Creating Sections in Your SRS Template Within `Body.hs`

1. Start constructing your chunk database i.e `symbMap` function – As you build sections of your SRS, you will be required to fill in this database function. 
2. Add `nw program` in the second argument of `symbMap`, and import the module where the variable `program` is defined i.e `import Data.Drasil.Concepts.Software (program)`.

   
## Update `yaml` and `cabal` Files
The `package.yaml` file contains some information about your project, including all the modules used in your project. From there, the compiler will generate a `.cabal` file for you. The information in the `.cabal` is then needed to actually build your project and connect all the packages included. As you add more modules to you project folder, you will need to update `package.yaml` (which will automatically update the `.cabal` file). You should almost never have to manually update the `.cabal` file.

1. Navigate to `../code/drasil-example/**{your-new-example-folder}**/package.yaml`
2. At the bottom, update the file using the other examples as a guide. It should look something like this:
```yaml
...
library:
  source-dirs: lib
  when:
  - condition: false
    other-modules: Paths_{your-example-name}

executables:
  yourExampleName:
      main: Main
      source-dirs: app
      ...
      dependencies: 
      - {your-example-name}
      when:
      - condition: false
        other-modules: Paths_{your-example-name}
```


## Build Your Project – Run `make`
1. Navigate to `code/MakeFile`, add your project to the list of other example projects and modify as applicable.
2. Navigate to the `code` folder from git bash or your command prompt to build your project.
3. Run `make` to build your project.
4. Navigate to your project folder in `build` to view your output files.
5. When you build your project, both a `.tex` and `.html` version of your SRS document will be auto-generated in your SRS folder. They will be separated into PDF and HTML subfolders. By now, your `.html` SRS file should show a blank SRS document.
6. After the build operation is completed, check the output of your SRS in the `.html` file. A cascading style sheet is also autogenerated in the same folder. 

## Updating Stable folder in `code/stable/`
You might have to update stable for all the other examples if you get the following error for any of the other examples. The one shown below is from the Template project: 
```
-------------------------------------------
- logs/Template_log.log IS NOT EMPTY -- DIFFERENCE
- BETWEEN GENERATED AND STABLE OUTPUT FOUND
-------------------------------------------
-------------------------------------------
- ERROR IN GENERATED OUTPUT SEE ABOVE FOR -
-             MORE DETAILS                -
-------------------------------------------
```
Subsequently, you need to update stable for your new project whenever you make any changes.
1. Make a copy of your project folder from `/Drasil/code/build/yourProjectName`.
2. Paste in `Drasil/code/stable` to create a folder for your project. Ensure that the version in `stable` matches the version in `build` when pushing your project to your remote repository.

3. Rename your folder with the name you used for `_sys` from the `si` variable in `Body.hs`. This should be the same as what appeared in the `build` folder.
4. Update `.html` and `.tex` files (copy and replace) only.
5. When you build your project, and it says `GENERATED OUTPUT MATCHES STABLE VERSION`, then stable is up to date, no need to update. Here is what a stabilized output will look like:
```
----------------------------
Make complete, checking logs
----------------------------
-------------------------------------------
- GENERATED OUTPUT MATCHES STABLE VERSION -
-------------------------------------------
```

### GitHub Actions / Builds & Tests
If you'd like to test your new project against the existing build scripts, please include `[workflow-trigger]` in a commit message when pushing your work. This will force our Drasil build workflow to run on your branch, and you will be able to view logs of the run under the "Actions" tab. Alternatively, if you are working in a branch that already has a PR attached to it, it will automatically build and test without the `[workflow-trigger]` in your commit message. You may then view the logs related to your test run by finding the Action run linked to your commit/PR, or by visiting the "Actions" page and looking for your triggered test run.

For more information about our automated tests, please see our [Workflow documentation](Workflow#continuous-integration-ci---github-actions--builds--tests).

## Logging

A log file will be auto-generated in a `logs` folder for your project after every build: An example file path for this is: `/code/logs/yourProjectName_log.log`. The logs are generated by using `diff` between the `build` and `stable` folders to find any differences between the two folders.

# Adding Sections to SRS Body

1. After creating a blank SRS, you will be able to add sections to it and populate with information.
2. The `Body.hs` module of your project will be used to gather the sections of your SRS.
3. We will now begin to fill in the function `mkSRS`. The value of `mkSRS` will include all the sections required in your SRS document. Visit the different examples for more information on this. A list of potential sections and their constructors can be found in the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#g:3).
4. You will find all the sections that can be created in  `../code/drasil-docLang/lib/Drasil/DocLang/SRS.hs`
5. Begin adding sections using the excerpt below:
```Haskell 
mkSRS = [ TableOfContents -- Adds a Table of Contents section
    RefSec $       -- Adds the reference material section
      RefProg intro  -- Add the introduction blob
        [TUnits         -- Adds table of units subsection
        , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits] 
       -- The above adds table of symbols with contents and specified rules e.g ordering content of table, bold vector symbols, adds purpose of document, etc.
        , TAandA -- Adds table of abbreviation and acronym subsection
        ],
```

6. Ensure that your import list contains DocLang's SRS:
```Haskell
import qualified Drasil.DocLang.SRS as SRS
```

7. At this point build your code (by running `make`) and see what you generate. You should see the sections in step 5 displayed in your SRS. Depending on what you have added to mkSRS you may need to update the ChunkDB first, which will be explained below.

8. The remainder of the tutorial will go over adding a subset of potential sections to your project. A complete list of potential sections can be found in the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#g:3). Not all sections need to be included in your project.


## Adding Introduction Section
You can add the ‘Introduction’ section and related sub-sections into `Body.hs`. The SRS section builder is defined in this module: 

`/code/drasil-docLang/lib/Drasil/DocLang/SRS.hs`

Introduction constructors can be found in the [IntroSec Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#t:IntroSec). 

1. Start a new section in `mkSRS` after the block you created in step 5 above. 
2. Add the introduction section information to your project. An example Introduction will look something like:
```Haskell
IntroSec $
    IntroProg justification (phrase dblpendulum) -- This line introduces the system you want to build. 'justification' is a function that needs to be defined in `Body.hs` to hold the content of an introduction preamble.
      [IScope scope], -- This line adds the Scope of Requirements section to SRS. You will need to define a constructor for scope in Body.hs with type 'Sentence'. 
```
3. Define a `scope` constructor (example below). This will hold the content of the introduction preamble and must be a `Sentence`.
```Haskell
scope :: Sentence
scope = foldlSent [S "dblpendulum is the subject" +:+. S "dblpendulum is the focus", phrase dblpendulum]
```
4. Define a `justification` constructor. It introduces the system you want to build.
```Haskell
justification :: Sentence
justification = foldlSent [ atStartNP (a_ pendulum), S "consists" `S.of_` phrase mass, ...]
```
5. Other sub-sections similar to `scope` in step 3 can also be included e.g. `Purpose`. Just make sure that you add to the imports and `symbMap` as required.
Here is a sample of the first few sections added for the Projectile project:
```Haskell
mkSRS :: SRSDecl
mkSRS = [
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IScope scope ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `S.the_ofThe` phrase projectile]]
```
6. Since we added a `scope` section, you will need to include the constructor name in the chunk database. `scope` is defined in the `doccon` constructor in `../drasil-data/lib/Data/Drasil/Concepts/Documentation`. The process of updating the chunk database may need to be repeated for other constructors that are created in other parts of the SRS.

![image](https://user-images.githubusercontent.com/43192745/94777925-4acd2900-0392-11eb-8dbe-85caf641fd05.png)

7. In the 3rd argument of `symbMap`, add the `doccon` and `doccon'` functions. The third argument should now look like:
```Haskell
(nw yourSoftwarePackageName : [nw program] ++ map nw doccon ++ map nw doccon')
```
You must also `import Data.Drasil.Concepts.Documentation as Doc (doccon, doccon')` at the top of `Body.hs`.

symbMap maps references the the chunk database. Please review the [GlassBR](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/glassbr/lib/Drasil/GlassBR/Body.hs), [NoPCM](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/nopcm/lib/Drasil/NoPCM/Body.hs), and [Projectile](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/projectile/lib/Drasil/Projectile/Body.hs) examples to understand how to populate it.   

8. You may choose to build your program every so often to keep track of errors easily.


## Adding Specific System Description (SSD) Section

SSD section constructors can be found in the [SSD Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#t:SSDSec). 
  
1. Before you add the SSD section, you will need to define the following parameters in `Body.hs`:

    - `prob` (`Sentence` type) defines the problem to be solved by the system.
    - `terms` (`ConceptChunk` type) holds the terms to be defined in the terminology section.
    - `physSystParts` (`Sentence` type) describes the physical parts of the system you intend to build.

2. Create a new file `Figures.hs` for your physical system diagram and define a function for your figure. A figure is only needed if adding a `PhySysDesc` subsection. Otherwise, skip this step.
    
    - Add a folder for your project in `../Drasil/code/datafiles`
    - Save your image in your project folder
    - Add your image to your function defined in `Figures.hs`
    - Add `Figures.hs` to your import list in `Body.hs`

3. Add SSD section and subsections to `Body.hs` – see excerpt from a sample project below.
```Haskell
SSDSec $ 
    SSDProg -- This adds a Specific system description section and an introductory blob.
      [ SSDProblem $ PDProg prob [] --  This adds a is used to define the problem your system will solve
        [ TermsAndDefs Nothing terms  -- This is used to define the terms to be defined in terminology sub section
      , PhySysDesc pendulumTitle physSystParts figMotion [] -- This defines the Physicalsystem sub-section, define the parts
                                                            -- of the system using physSysParts, figMotion is a function in figures for the image
```

## Adding Goal Statements section

1. Add a new subsection to `mkSRS` after `PhySysDesc` so that the SSD section looks like this:
```Haskell
SSDSec $ 
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
      , PhySysDesc pendulumTitle physSystParts figMotion []
      , Goals goalsInputs] -- This adds a goals section and goals input is defined for the preamble of the goal.
```
2. The `goalsInputs` function defines the preamble of your goal statement. You can define `goalsInputs` in `Body.hs` or in a new `Goals.hs` file. For this guide, we will use a new `Goals` module.
3. Create a new module `Goals.hs` in your working folder.
4. Define your goal statement using 'ConceptInstance' type.
5. You will require the `goalStmtDom` concept from `Data.Drasil.Concepts.Documentation` to be added to your import list in your `Goals.hs` module. You will use this function in defining your actual goal statement. Your `Goals.hs` file should look something like this simplified excerpt from the Pendulum example: 
```Haskell
module Drasil.DblPendulum.Goals (goals, goalsInputs, goalRefs) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (goalStmtDom)

goals :: [ConceptInstance]
goals = [motionMass]

goalsInputs :: [Sentence]
goalsInputs = [S "the mass and length of the rod, initial angle of the mass and the gravitational constant"]

motionMass :: ConceptInstance
motionMass = cic "motionMass" 
  (S "Calculate the motion of the mass")
  "Motion-of-the-mass" goalStmtDom -- Here is where we apply goalStmtDom
```

6. Add your module to the import list in `Body.hs` to display your Goal statement section and update `symbMap` as required.
7. Add `goals` to your `symbMap` or to `concIns` (which will be created in step 10 of the next section).
8. Add all other required parameters to your import list.

## Adding Assumptions Section

Note: Assumptions are added with the Assumptions constructor under the [SSDSolChSpec](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#v:SSDSolChSpec) constructor in the Specific System Description section.

1. Declare assumptions section – Add `Assumptions` section to `makeSRS` function in `Body.hs`.
2. Create a new module `Assumptions.hs` and add the necessary imports (`Language.Drasil`, possibly `Utils.Drasil`, and `assumpDom` from `Data.Drasil.Concepts.Documentation`). 
3. Determine and define your assumption functions in your assumption module. Each individual assumption should be a `ConceptInstance` type. This can be done using the `cic` constructor.
4. Use the `assumpDom` keyword in the last argument of the definition of your assumptions. `assumpDom` gives a domain (`UID`) to all the assumptions defined.
5. Then group them together into a list named `assumptions`. E.g.
```Haskell
assumptions :: [ConceptInstance]
assumptions = [pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin]

pend2DMotion, cartCoord, cartCoordRight, yAxisDir, startOrigin :: ConceptInstance
pend2DMotion    = cic "pend2DMotion"      pend2DMotionDesc    "pend2DMotion"    assumpDom
```

6. Remember to import any modules used (from step 2 and from those used for defining your assumptions) into `Assumptions.hs` including:
```Haskell
import Data.Drasil.Concepts.Documentation (assumpDom)
```
7. Import the `Assumptions.hs` module into `Body.hs`. 
8. To create a section for assumption in `Body.hs`, we will need to add `assumptions` to `symbMap`. An efficient way to do this is to create a function that will hold the assumptions and other sections of the SRS. So, we create `concIns` as a way of gathering these sections before putting them in our `symbMap`. Add `assumptions` and `goals` to `concIns` and then add `concIns` to `symbMap` in Body.hs. Note: As you continue to build your SRS, you will add more arguments to `concIns`.
```Haskell
concIns :: [ConceptInstance]
concIns = assumptions ++ goals
```

9. Run `make` to build your program.

Always remember to make references to the different parameters e.g assumptions, data definitions, theoretical models that you use throughout your project. It will help to populate your `refby` field where applicable. It will also be used for your traceability graphs at the end of your document. Make sure you point to the necessary elements. Use `refS` to refer to elements in your project.

## Adding Theoretical Models Section

Note: Theoretical Models are added with the TMs constructor under the [SSDSolChSpec](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#v:SSDSolChSpec) constructor in the Specific System Description section.

1. Declare the `TM` section in your `SRSDecl` (the `mkSRS` function)
2. Define the components to be used in construction of `TM` (e.g `Symbol`, `Units`, `DefiningEquation`) by adding an argument to define these variables. They should be of the type `Fields` and look similar to this:
```Haskell
        [ Assumptions
        , TMs [] (Label : stdFields)
```

3. Define the `stdFields` function in `Body.hs` as shown below. These functions will be used in building your theoretical models and other models used in the project.
```Haskell
stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]
```

4. Create a new `TMods.hs` file in your project folder. 
5. Where applicable, reuse theories from module `Data.Drasil.Theories`.
6. You can also define theories specific to your project by following the same format in `code/drasil-data/lib/Data/Drasil/Theories/Physics.hs`
7. Gather your theoretical models into a `tMods` function:
```Haskell
tMods :: [TheoryModel]
tMods = [theoryModel1, theoryModel2]
```
8. Add `tMods` to `symbMap`. Also add `tMods` to the last argument of `symbMap` after mapping them through the `ref` function.
9. If you have any references for your theoretical models, add a `References.hs` file to your working folder. See example projects for sample reference module.
10. Import the necessary modules.
11. Build your project.

## Adding General Definitions Section
- General Definitions usually include derivations of the equations used in your project. This is held by the general definition's constructor.
- You will be able to build your derivation equation and sentences and weave then together. 
- `/drasil-theory/lib/Theory/Drasil` contains the constructors that will be used for constructing your general definitions. `gd` and `gdNoRefs` (for general definitions with no references) are the two constructors used for defining general definitions.

Note: General Definitions are added with the GDs constructor under the [SSDSolChSpec](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#v:SSDSolChSpec) constructor in the Specific System Description section.

1. Add General definition section to `SRSDecl` constructor in `Body.hs` as shown:
```Haskell
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
```
2. Create a `Gendefs.hs` module in your working folder. This module will hold all your general definitions and derivations.
3. Define the functions to hold your general definitions with type `GenDefn`: 
```Haskell
genDefns :: [GenDefn]
genDefns = [genDef1, genDef2]
```
4. Define functions for your derivations sentences and equations.
5. Add descriptions to your derivation functions and equations.
6. You can use the `weave` function to connect your derivation sentences and equations to generate your derivation section for each item. Here is an excerpt from Pendulum. Note `angFrequencyDeriv`'s use of `weave`:
```Haskell
angFrequencyDeriv :: Derivation
angFrequencyDeriv = mkDerivName (phraseNP (angularFrequency `the_ofThe` pendulum)) (weave [angFrequencyDerivSents, map eS E.angFrequencyDerivEqns])

angFrequencyDerivSents :: [Sentence]
angFrequencyDerivSents = [angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
                      angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7]

angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
     angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7 :: Sentence
angFrequencyDerivSent1 = foldlSentCol [S "Consider the", phrase torque, S "on a", phrase pendulum +:+. definedIn'' newtonSLR,
                  S "The", phrase force, S "providing the restoring", phrase torque `S.is` phraseNP (the component `NP.of_`
                  (weight `ofThe` pendulum)), S "bob that acts along the" +:+. phrase arcLen,
                  (phrase torque `S.isThe` phrase len) `S.the_ofTheC` S "string", ch lenRod, S "multiplied by", phrase component
                  `S.the_ofThe` S "net", phrase force, S "that is perpendicular to", S "radius" `S.the_ofThe` (S "arc" !.),
                  S "The minus sign indicates the", phrase torque, S "acts in the opposite", phraseNP (direction `ofThe`angularDisplacement)]
angFrequencyDerivSent2 = S "So then"
angFrequencyDerivSent3 = S "Therefore,"
--etc.
```
7. Add your new module to `Body.hs` to display the components of this module in the corresponding section.
8. Add `genDefs` to `symbMap` in `Body.hs`, replacing the existing list `([] :: [GenDefn])` with `genDefs`.
9. You might need to add another module `Unitals.hs`. You will need to define some concepts, parameters, functions or variables locally in this new `Unitals.hs` module to be used specifically for your projects. These are often used in the `GenDefs` module and other modules. See example projects for a better understanding of unital files.
10. If you have used parameters from `Unitals.hs` in your `GenDefs` module, remember to add to the import list as required.
11. Populate the following parameters in the `si` function (in `Body.hs`) with the values defined as functions in your `Unitals` module. It should now look something like this:
```Haskell
  _purpose     = [],
  _quants      = symbols, -- used to define all the symbols for concepts used throughout your project
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = [],
  _configFiles = [],
  _inputs      = inputs, -- used to define all the input variable parameters
  _outputs     = outputs, -- used to define all the output variable parameters
  _defSequence = [] :: [Block QDefinition],
```

12. See sample below for an example from Pendulum of defining `symbols`, `inputs`, and `outputs`:
```Haskell
symbols :: [QuantityDict]
symbols = map qw unitalChunks ++ map qw unitless

inputs :: [QuantityDict]
inputs = map qw [LenRod, QP.force]

outputs :: [QuantityDict]
outputs = map qw [QP.position
```
13. Add `map nw symbols` to `usedDB` and `symbMap` functions in `Body.hs` to display all unit symbols.
14. Import all the required modules.
15. Build your project with `make` or the executable name in the cabal file. For example, `make diff_dblpendulum` for the DblPendulum project.



## Adding Data Definitions and Acronym Section

Note: Data Definitions are added with the DDs constructor under the [SSDSolChSpec](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#v:SSDSolChSpec) constructor in the Specific System Description section. Acronyms are defined in a new file. Link to two examples, one with them defined in Defs and another in Unitals.

1. Add data definition section to `mkSRS` in `Body.hs`:
```Haskell
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
```
2. Add a new new module to your project folder named `DataDefs.hs`.
3. The constructors for creating data definitions can be found here:
`/code/drasil-theory/Theory/Drasil/DataDefinition.hs`
4. Add the constructors to the list of imports.
5. In your new module, define a list of all your general definition functions using the `DataDefinition` type. E.g
```Haskell
dataDefs :: [DataDefinition]
dataDefs = [positionIX, positionIY]
```
6. Add the list function in step 6 above to `si` in `Body.hs` so that the parameter is filled in like so:
```Haskell
  _instModels  = iMods,
  _datadefs    = [],
  _configFiles = [],
```
7. Add all parameters, symbols and units used in `dataDefs` to `symbMap` accordingly. See the `Body.hs` modules from the example projects for more help.
8. Remember to add all symbols used in this module to symbols function in `Unitals.hs` and add to imports as applicable.
9. To `symbMap`, replace `([] :: [UnitDefn])` with:
```Haskell
(map unitWrapper [{-add the units used here, ex. metre, second, etc.-}])
```

10. If you have used acronyms (which you must, if you have created any of the above modules), create a function for the list of acronyms used. For example:
```Haskell
acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]
```

11. Add `acronyms` to `symbMap` in `Body.hs` and add appropriate imports.
12. add `acronyms` and `symbols` to `usedDB` if you have not already done so. It should look like this:
```Haskell
usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbols) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])
```
13. Build your project.

## Adding Instance Models Section

Note: Instance Models are added with the Ims constructor under the [SSDSolChSpec](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#v:SSDSolChSpec) constructor in the Specific System Description section.

1. Add the Instance Model section to `mkSRS` function in `Body.hs`. It should look similar to this:
```Haskell
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
```
2. Create a new module `IMods.hs`
3. In `symbMap`, replace `([] :: [InstanceModel])` with `iMods`.
4. In IMods.hs, create a new function `iMods` with type `[InstanceModel]` to declare your list of IM functions. E.g.
```Haskell
iMods :: [InstanceModel]
iMods = [iMod1, iMod2]
```
5. Start defining your IM functions. See `/code/drasil-theory/lib/Theory/Drasil/InstanceModel.hs` for various constructors you can use in defining your instance model. Also see example projects or [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/doc-index-All.html) for help. The arguments are used as follows: 
    - Declare your IM constructor and the IM relational concept in the first argument.
    - Define your input and output constraints in the next argument. Ensure that your input and output constraints are already declared or defined in your `Unitals.hs` file.
    - Your IMs should have derivations, declare the derivation function name and use it in the third argument.
    - Declare a reference name for your IM function as a `String` in the fourth argument
    - You can declare a list of functions for adding notes to your IM where applicable in the fifth argument.

Your instance model will look something like this excerpt from Pendulum:
```Haskell
angularDisplacementIM :: InstanceModel
angularDisplacementIM = imNoRefs (OthModel angularDisplacementRC)
  [qwC lenRod $ UpFrom (Exc, exactDbl 0)
  ,qwC initialPendAngle $ UpFrom (Exc, exactDbl 0)
  , qwC gravitationalAccel $ UpFrom (Exc, exactDbl 0)]
  (qw pendDisplacementAngle) [UpFrom (Exc, exactDbl 0)]
  (Just angularDisplacementDeriv) "calOfAngularDisplacement" [angularDispConstraintNote]
```

6. Define your `RelationalConcept` by defining the equation and a label for your IM. E.g 
```Haskell
angularDisplacementRC :: RelationConcept
angularDisplacementRC = makeRC "angularDisplacementRC" (nounPhraseSP "calculation of angular displacement") -- Label goes here.
  EmptyS $ apply1 pendDisplacementAngle time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time) -- Equation goes here.
```
7. Define functions for your derivation, declared in the third argument of your instance model. You will need the weave function to combine derivation sentences and equations. See example projects for more details. Here is an excerpt from Pendulum for the top-level `angularDisplacementDeriv` function (which is used in `angularDisplacementTM` as shown above):
```Haskell
angularDisplacementDeriv :: Derivation
angularDisplacementDeriv = mkDerivName (phrase angularDisplacement) (weave [angularDisplacementDerivSents, map eS angularDisplacementDerivEqns])

angularDisplacementDerivSents :: [Sentence]
angularDisplacementDerivSents = [angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
                             angularDisplacementDerivSent4, angularDisplacementDerivSent5]
```

8. Define separate functions for your derivation sentences and equations. 
9. Define functions for notes where applicable, as declared in the fifth argument of your instance model. E.g.
```Haskell
angularDispConstraintNote :: Sentence
angularDispConstraintNote = foldlSent [atStartNP (the constraint),
     eS (sy initialPendAngle $> exactDbl 0) `S.is` (S "required" !.),
     atStartNP (the angularFrequency) `S.is` definedIn'' angFrequencyGD]
```

10. Add all the required imports.
11. Update `symbols` function in `Unitals.hs` where applicable.
12. Update the chunk database (`symbMap`) as applicable.

## Adding Constraints and Properties of a Correct Solution Section

Note: Constraints are added with the Constraints constructor and Properties of a Correct Solution are added with the CorrSolnPpties constructor under the [SSDSolChSpec](https://jacquescarette.github.io/Drasil/docs/drasil-docLang-0.1.26.0/Drasil-SRSDocument.html#v:SSDSolChSpec) constructor in the Specific System Description section.

1. Declare the following sections after your definition and model sections in `mkSRS` (which is in `Body.hs`) as shown below:
```Haskell
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
       ]
     ],
```

2. Set `_constraints = inConstraints` in `si`.
3. Define the functions `inConstraints` and `outConstraints` in `Unitals.hs`. These functions represent constraints of your input and output parameters.
    - Declare constraints using the `ConstrConcept` type.
    - You can use constructors like `constrained'`, `constrainedNRV`, etc. to create your table of constraints. See example projects for reference.
4. Add all the required imports.
5. Add all the required parameters to your chunk database in `Body.hs`.


## Adding Requirements Section

1. Declare the requirements section in `mkSRS` (in `Body.hs`):
```Haskell
  ReqrmntSec $ ReqsProg
    [ FReqsSub EmptyS []
```
2. Create a `Requirements.hs` module.
3. Define a list function to declare all your functional requirements. See example in the code excerpt under step 5.
4. Define your requirement functions. You will need to tag every functional requirement with `funcReqDom` as shown below:
```Haskell
funcReqs :: [ConceptInstance]
funcReqs = [verifyInptVals, calcAngPos, outputValues]

verifyInptVals, calcAngPos, outputValues :: ConceptInstance

verifyInptVals = cic "verifyInptVals" verifyInptValsDesc "Verify-Input-Values" funcReqDom
calcAngPos  = cic "calcAngPos"   calcAngPosDesc   "Calculate-Angular-Position-Of-Mass" funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

verifyInptValsDesc, calcAngPosDesc, outputValuesDesc :: Sentence
```

5. Define the descriptions (as `Sentences`) for your functional requirements.
6. Add `funcReqs` to `concIns` in `Body.hs`.
```Haskell
concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ funcReqs
```
7. Add all necessary modules and parameters to the list of imports.


## Adding Non-Functional Requirements Section

1. Declare the non-functional requirements section in `mkSRS` function in `Body.hs`. The requirements declaration should now look like:
```Haskell
    ReqrmntSec $ ReqsProg
      [ FReqsSub EmptyS []
      , NonFReqsSub
    ],
```

2. Add non-functional requirements to `Requirements.hs`. 
3. Define a list function with type `ConceptInstance` to declare all your non- functional requirements. See example in the code excerpt below.
```Haskell
nonFuncReqs :: [ConceptInstance]
nonFuncReqs = [correct, portable]

correct :: ConceptInstance
correct = cic "correct" correctDesc "Correct" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" portableDesc "Portable" nonFuncReqDom

correctDesc, portableDesc :: Sentence
```

4. Define each non-functional requirement function (as `ConceptInstance` type) in the list. You will need the domain `nonfuncReqDom` to define every non-functional requirement. See above code excerpt.
5. Use concept instance constructor `cic` to define your non-functional requirements. See `code/drasil-lang/Language/Drasil/Chunk/Concept.hs` for constructor info.

6. Add `nonfuncReqs` to `concIns` in `Body.hs`.
7. Add all required parameters to import list.
8. If you have not already done so, add `srsDomains` to `symbMap (in the 3rd argument) in `Body.hs`.


## Adding Likely and Unlikely Changes Section

Please review [PDController's Body.hs](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/pdcontroller/lib/Drasil/PDController/Body.hs) to see an example of this section.

1. Follow above similar steps to add likely and unlikely changes sections where applicable. See sample below.
2. Add all the required imports.
3. Update `concIns` function in `Body.hs` to reflect likely and unlikely changes.
```Haskell
    NonFReqsSub
  ],
  LCsSec,
  UCsSec,
```

## Adding Other Sections

Please review [GamePhysics' Body.hs](https://github.com/JacquesCarette/Drasil/blob/master/code/drasil-example/gamephysics/lib/Drasil/GamePhysics/Body.hs) to see an example of this section.

1. You can add other sections not included explicitly in this manual into your SRS body. For example, to add an "Off the Shelf Solution" section use:
```Haskell
  UCsSec,
  OffShelfSonsSec $ OffShelfSolnsProg offShelfSols,
```
For more information regarding the different possible sections, please visit the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/drasil-docLang-0.1.26.0/Drasil-DocLang.html) for document language.


## Adding Traceability Section
1. Plug in the traceability section in mkSRS function
```Haskell
  OffShelfSonsSec $ OffShelfSolnsProg offShelfSols,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si
```

2. Traceability graphs should now automatically generate when you build your code.

## Notes

1. Ensure that all the units used in your project are added to unit map in `symbMap` function. The chunk database needs to know this information to print the SRS.
2. Helper function to build unitals are found here:
`Drasil-lang/language/Drasil/chunk/Unitals.hs`
3. Remember to create functions in `Unitals.hs` to store your input and output parameters.
4. If you have used any references, remember to below code into `Body.hs` to display your references:
```Haskell
refDB :: ReferenceDB
refDB = rdb citations concIns
```
5. Reminder to add `acronymns` and `symbols` to `usedDB` chunk database to display your acronymns and symbols tables:
```Haskell
usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronymn ++ map nw symbols) ([] :: [ConceptChunk])([] :: [UnitDefn]) [] [] [] [] [] [] []
```
6. Your `mkSRS` and `si` functions should now look something like this example from Projectile:
```Haskell
mkSRS :: SRSDecl
mkSRS = [
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IScope scope ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `S.the_ofThe` phrase projectile]]
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

si :: SystemInformation
si = SI {
  _sys         = projectileTitle,
  _kind        = Doc.srs,
  _authors     = [samCrawford, brooks, spencerSmith],
  _purpose     = [],
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = map cnstrw constrained,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}
```

# Code Generation

1. Please refer to `NoPCM` or `GlassBR` to generate code for your project from Drasil.
2. The `NoPCM` example can be referred to, if your project requires an ODE, otherwise, you can refer to the `GlassBR` example.
3. Before you generate your code, ensure your `SystemInformation` structure is correct, particularly the inputs, outputs, relations, and constraints should be filled in and correct.
4. The Drasil code generator requires that you set design variabilities for your code. An overview of design variabilities can be found in Chapter 8 of this [document](https://macsphere.mcmaster.ca/handle/11375/25542).

## Modify `Main.hs`
1. Navigate to `code/drasil-example/yourProjectName`. Then, mirror the `Main.hs` file from either `GlassBR` or `NoPCM`. Also create a new `Choices.hs` file and mirror that from `GlassBR` or `NoPCM`. The new `Choices.hs` file will let you augment your generated code depending on the choices you make.
2. Copy the content of the `Main.hs` file and paste in your `Main.hs` project file. Keep in mind there are some elements in the file that you already have in you your `Main.hs` file, so carefully delete duplicated or unwanted texts and modify the other content to suit your project.
3. Begin by modifying your design choices: see Chapter 8 of this [document](https://macsphere.mcmaster.ca/handle/11375/25542) for more information.
4. Update your import list as required in `Choices.hs`:
```Haskell
import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Modularity(..), Structure(..), ConstantStructure(..), ConstantRepr(..), 
  InputModule(..), AuxFile(..), Visibility(..), defaultChoices)
```
5. Implement the functions created from `Choices.hs` into `Main.hs`. `GlassBR` is a great example to figure out where these functions go.
6. Update the `MakeFile` in the `code` folder.
7. You may need to use some of the following functions, so add some or all of these imports to `Body.hs` or `Choices.hs` as applicable:
```Haskell
import Language.Drasil.Code (relToQD, quantvar, listToArray)
import Data.Drasil.ExternalLibraries.ODELibraries (scipyODESymbols, osloSymbols,
  arrayVecDepVar, apacheODESymbols, odeintSymbols)
import Database.Drasil (Block(Parallel), ChunkDB, ReferenceDB,
  SystemInformation(SI), cdb, rdb, refdb, _authors, _purpose, _concepts,
  _constants, _constraints, _datadefs, _instModels, _configFiles, _defSequence,
  _inputs, _kind, _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
```

8. Run `make`.
9. Your build folder should now contain a `src` folder.

