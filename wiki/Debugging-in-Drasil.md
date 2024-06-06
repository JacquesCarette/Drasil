# Contents
- [Issues related to `git`](#issues-related-to-git)
    - Also see [`git2know`](Git2Know)
- [Issues related to Drasil](#issues-related-to-drasil)
    - [Problem Analysis](#problem-analysis)
        - [`grep` Summary](#grep-summary)
        - [`make test`](#using-make-test)
        - [Generated Chunk Logs](#using-generated-chunk-logs)
    - [Example Problem](#example)
        - [`ChunkDB` Information](#now-what-is-the-chunkdb)
        - [Solution](#solution)
    - [Spelling/Grammatical Problems](#spellinggrammatical-problems)
    - [Errors of the form `Reference: not found in ReferenceMap`](#notes-for-reference-not-found-in-referencemap-errors)
- Also see the [Workflow](Workflow) page

# Issues related to `git`
Below are some of the commonly encountered issues or errors when working on Drasil:
Here are also some errors that have happened in the past but should be fixed (due to updates from stack, GHC, git, etc.):
- I made changes to a file on my local branch and built, the build was successful but when I checked to see my changes in the .html file, the changes I made were not visible. What can I do?
    - Make sure that your code change is valid and correct.
    - run `make stackArgs=-force-dirty`, this command will your machine to recompile, using the packages on your local branch.
- I made changes to the code and stable folder files on my local branch. The build build was successful. I staged and committed all changed files (in one pull request), but the GitHub Actions tests failed. What can I do?
    - See the [`make test` section](#using-make-test)
    - Also see this [comment](https://github.com/JacquesCarette/Drasil/pull/2151#issuecomment-635750650) on different types of Continuous Integration (CI) errors.


# Issues related to Drasil

## Problem Analysis
When errors are thrown from Drasil programs, we will be given specific error messages in the logs. If the error message isn't familiar to you, it's best to generally search the entire code repository for that message to see what kind of error(s) could have occurred. Using [`grep`](https://linux.die.net/man/1/grep), we can search through the repo fairly easily!

### `grep` Summary
A common and likely useful formation for using `grep` commands to find errors thrown by Drasil is to use the structure:
```Bash
grep "WordYouAreLookingFor" -r * --include "*.hs" 
```
which will recursively search all files ending with the letters `.hs` in the directory for occurrences of "WordYouAreLookingFor". There are some optional flags that may be useful:
- `-w` will only search for the whole word given. Whole words are determined by whitespace or non-letter characters. So words separated with a `.` will still appear. BASH users may note that they may write the `"*.hs"` as just `*.hs`. For example, using the command `grep "traceMGF" -r -w * --include "*.hs"` or  `grep "traceMGF" -rw * --include "*.hs"` will return:
```
drasil-docLang/Drasil/DocumentLanguage/TraceabilityGraph.hs:traceMGF :: [LabelledContent] -> [Sentence] -> [Contents] -> [Section] -> Section
drasil-docLang/Drasil/DocumentLanguage/TraceabilityGraph.hs:traceMGF refs trailing otherContents = SRS.traceyMandG (traceMIntro refs trailing : otherContents)
drasil-docLang/Drasil/DocumentLanguage.hs:import qualified Drasil.DocumentLanguage.TraceabilityGraph as TG (traceMGF)
drasil-docLang/Drasil/DocumentLanguage.hs:mkTraceabilitySec (TraceabilityProg progs) si = TG.traceMGF trace
```

- `--color` will highlight the word searched in a different colour. This is useful for larger `grep` searches, where each occurrence is not instantly apparent. 

### Using `make test`
Running the following set of commands may help you debug any elusive errors:
```
make clean
make test >& make.log
```
Check for any warnings or errors in the logs; these are treated as errors in GitHub Actions. Just remember to delete the log file once you are finished with it!

### Using Generated Chunk Logs
If a [chunk](Chunks) is not behaving as expected, try checking out the chunk logs (under `code/debug`) for the specific example. Specifically, keep an eye out for duplicate `UID`s attached to different terms or any other pieces of information. It may be a signal for a deeper problem. The first three tables contain traceability information from one chunk to the next, while the rest are listed from the fields of the `ChunkDB` type. `ChunkDB`s are usually stored in the `_sysinfodb` field of `SystemInformation`.

# Example

Let's say you are working on adding a new section to an example (specifically, we will be using `DblPendulum` for code excerpts, but the same concepts will apply across Drasil). When you try to compile, you receive the following error message:
```
Reference: parnasClements1986 not found in ReferenceMap
```
It will be quite difficult to manually search through all the files to find this exact error message, so let's try using `grep` on a small part of the error message. Using `grep "not found in" -r * --include "*.hs"` (meaning that I want to traverse all files, looking only through the `.hs` files for lines containing the characters "not found in") shows us that the errors starts in `code/drasil-database/Database/Drasil/ChunkDB.hs`:
```Haskell
 -- | Looks up a 'UID' in a 'UMap' table. If nothing is found, an error is thrown. 
 uMapLookup :: String -> String -> UID -> UMap a -> a 
 uMapLookup tys ms u t = getFM $ Map.lookup u t 
   where getFM = maybe (error $ tys ++ ": " ++ u ++ " not found in " ++ ms) fst 
  
 -- | Looks up a 'UID' in the symbol table from the 'ChunkDB'. If nothing is found, an error is thrown. 
 symbResolve :: ChunkDB -> UID -> QuantityDict 
 symbResolve m x = uMapLookup "Symbol" "SymbolMap" x $ symbolTable m
```

We can see that `uMapLookup` is the function that _actually_ throws the error, but it's thrown only when a lookup fails for some map (for example, the `symbResolve` function resolves `QuantityDict`s from the `SymbolMap` of a `ChunkDB` using `uMapLookup`, so it can also cause errors).

Looking around the surrounding code (which will not be added to the snippet above for the sake of conserving space), we can see that when "X not found in Y-Map" errors occur, it's usually due to something not being in _some_ `ChunkDB` (the `X` represents the `UID` of the missing item, and the `Y` represents the `Map` from which it is expected but does not exist in).

### *Now, what is the `ChunkDB`?*

Using a similar `grep` to the above (searching with `ChunkDB`), we find the type declaration in `code/drasil-database/Database/Drasil/ChunkDB.hs`:
```Haskell
 -- | Our chunk databases. \Must contain all maps needed in an example.\ 
 -- In turn, these maps must contain every chunk definition or concept  
 -- used in its respective example, else an error is thrown. 
 data ChunkDB = CDB { symbolTable :: SymbolMap 
                    , termTable :: TermMap  
                    , defTable  :: ConceptMap 
                    , _unitTable :: UnitMap 
                    , _traceTable :: TraceMap 
                    , _refbyTable :: RefbyMap 
                    , _dataDefnTable  :: DatadefnMap 
                    , _insmodelTable   :: InsModelMap 
                    , _gendefTable   :: GendefMap 
                    , _theoryModelTable :: TheoryModelMap 
                    , _conceptinsTable :: ConceptInstanceMap 
                    , _sectionTable :: SectionMap 
                    , _labelledcontentTable :: LabelledContentMap 
                    , _refTable :: ReferenceMap 
                    } --TODO: Expand and add more databases 
```

 The `ChunkDB` is the "Chunk Database", containing all "chunk definitions and concepts" for a specific "system" (example). It contains many kinds of `Map`s from UIDs to various chunks.

### So, what does this ultimately mean?

This means that some example (likely the named one throwing the error in the first place) is missing some chunk information in it's `ChunkDB`!

## Solution

To resolve this, we generally want to:
1. *Find where the thing we're missing was created* - Again, I like to do this using `grep` using `grep "X" -r * --include "*.hs"`. Taking the example of `parnasClements1986`, we get:
```
home:~/Programming/Drasil/code$ grep "parnasClements1986" -r * --include *.hs
drasil-data/Data/Drasil/Citations.hs:  parnasClements1986, smithLai2005, lineSource, pointSource :: Citation
drasil-data/Data/Drasil/Citations.hs:parnasClements1986 = cArticle [dParnas, pcClements] 
drasil-data/Data/Drasil/Citations.hs:  "parnasClements1986"
drasil-docLang/Drasil/Sections/Introduction.hs:import Data.Drasil.Citations (parnasClements1986)
drasil-docLang/Drasil/Sections/Introduction.hs:  S "Parnas and Clements point out", refS parnasClements1986 `sC`
drasil-example/Drasil/GlassBR/References.hs:import Data.Drasil.Citations (campidelli, koothoor2013, smithLai2005, parnasClements1986)
drasil-example/Drasil/GlassBR/References.hs:  astm2012, beasonEtAl1998, parnasClements1986]
drasil-example/Drasil/SSP/References.hs:import Data.Drasil.Citations (jnlCGJ, koothoor2013, parnasClements1986, smithLai2005)
drasil-example/Drasil/SSP/References.hs:citations = [chen2005, parnasClements1986, koothoor2013,
drasil-example/Drasil/NoPCM/References.hs:  parnasClements1986, smithLai2005)
drasil-example/Drasil/NoPCM/References.hs:citations = [incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986, smithLai2005]
drasil-example/Drasil/GamePhysics/References.hs:import Data.Drasil.Citations (cartesianWiki, koothoor2013, parnasClements1986,
drasil-example/Drasil/GamePhysics/References.hs:citations = [parnas1978, sciComp2013, chaslesWiki, parnasClements1986,
drasil-example/Drasil/SWHS/References.hs:  parnasClements1986, smithLai2005, citeRefs) where
drasil-example/Drasil/SWHS/References.hs:import Data.Drasil.Citations (koothoor2013, parnasClements1986, smithLai2005)
drasil-example/Drasil/SWHS/References.hs:citations = [bueche1986, incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986, 
```

Here, we can see that `parnasClements1986` was created in `drasil-data/Data/Drasil/Citations.hs` (we also see some examples of imports of it, so that will be helpful later too).

If no suitable results appeared, then it's likely that you will either need to create the desired function or that a `UID` reference was incorrect.
2. *Find where we were supposed to place it in the `ChunkDB` of that system/example* - For example, with DblPendulum, we instantiate our `ChunkDB` in `Body.hs` here:  
```Haskell
symbMap :: ChunkDB 
 symbMap = cdb (map qw iMods ++ map qw symbols) 
   (nw newtonSLR : nw pendulumTitle : nw mass : nw len : nw kilogram : nw inValue : nw newton : nw degree : nw radian 
     : nw unitVect : nw unitVectj : [nw errMsg, nw program] ++ map nw symbols ++ 
    map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw mathcon  ++ map nw physicCon' ++ 
    map nw physicscon ++ concepts ++ map nw physicalcon ++ map nw acronyms ++ map nw symbols ++ map nw [metre, hertz]) 
   (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz]) dataDefs 
   iMods genDefns tMods concIns [] [] allRefs 
```
3. *Figure out which list we should placing it in* - You will need to analyze the error message for which `Map` it is related to, and then you will need to know how the `cdb` smart constructor works. This can be done through the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/index.html) or by using `grep` again. For ease of reference, the declaration for `cdb` is as follows:
```Haskell
 cdb :: (Quantity q, MayHaveUnit q, Idea t, Concept c, IsUnit u) => 
     [q] -> [t] -> [c] -> [u] -> [DataDefinition] -> [InstanceModel] -> 
     [GenDefn] -> [TheoryModel] -> [ConceptInstance] -> [Section] -> 
     [LabelledContent] -> [Reference] -> ChunkDB 
 cdb s t c u d ins gd tm ci sect lc r = CDB (symbolMap s) (termMap t) (conceptMap c) 
   (unitMap u) Map.empty Map.empty (idMap d) (idMap ins) (idMap gd) (idMap tm) 
   (idMap ci) (idMap sect) (idMap lc) (idMap r) 
```
Looking at the type signature + surrounding comments are very helpful for this.
4. *Add it to that chunk map* - This step heavily depends on the example, but we usually can just prepend it to the list of references. In this case, there is an `allRefs` list inside of `Body.hs`:
```Haskell
allRefs :: [Reference]
allRefs = nub (assumpRefs ++ bodyRefs ++ figRefs ++ goalRefs ++ dataDefRefs ++ genDefRefs
  ++ iModRefs ++ tModRefs ++ citeRefs ++ reqRefs ++ secRefs)
```
We know `parnasClements1986` is a `Citation` from our above `grep` search, so we look for the corresponding reference function, `citeRefs`:
```Haskell
citeRefs :: [Reference]
citeRefs = map ref citations
```
We then look where `citations` is defined and finally add our desired reference:
```Haskell
citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki, parnasClements1986]
```

# Spelling/Grammatical Problems
Like learning any new language, getting started with grammar in Drasil is sometimes tough and a little confusing. To help new users jump in to working with Drasil, this section will note a variety of issues that can occur when starting out.

## Extra or Missing Periods
Often in Drasil, duplicate periods can crop up in seemingly mysterious ways. Through a mix of automatically generated content and the information that the user may input into an example, keeping track of punctuation has been an increasingly complicated task. The language of Drasil has seen consistent growth in this respect, and this section will demonstrate some of the common culprits of duplicate or missing periods.

The first step in any of these grammatically related problems should always be to double check the user input. Before we even get into the different kinds of functions available for use, making sure that a sneaky hard-coded period has not cropped up is key to having an enjoyable editing experience. This can be done in many different ways, most notably:
- Using `grep` or `ripgrep` to search for periods. It may also be worthwhile to use `grep` including the last word before the period or quotation marks after the period, as using grep on just `.` will also return all instances of the composition function native to Haskell.
- Use `grep` on some words near the offending period. Often, there will be functions that are used in place of literal strings (meaning that the `grep` may fail to work if you only look for one certain word or phrase that happened to be replaced by a function). Try to avoid looking with words like `of`, `a`, `and`, `the`, as those are often functions in the form of [combinators](https://github.com/JacquesCarette/Drasil/wiki/Combinator-Documentation) and will not pattern match with the `grep` search.
- Searching through the code manually and trying to find the spare period.

Next, we will introduce some common functions that include sentences in them. These are also very easy to mix up, so close attention is key:
- `!.` - This function should always be used in place of a literal period and as a [post-fix operator](https://downloads.haskell.org/~ghc/6.6/docs/html/users_guide/syntax-extns.html) (see bottom of linked page). It appends a period onto the `Sentence` in front of it. You may have to include the following at the top of the `.hs` file where it is used: `{#- LANGUAGE PostfixOperators -#}`. In addition, its (prefixed) argument and the function itself will need to be encased in brackets.
- `+:+` and `+:+.` - These two functions look extremely similar, so it may be difficult to see where they are used. A common case of this becoming a source of error is when `+:+.` is used in conjunction with `!.` or a literal period. The first one only adds a space between two `Sentences`, while the latter adds a space and then appends a period to the end of the second `Sentence`. When possible, using `+:+.` will give a cleaner result than using both `+:+` and `!.`, though they will both do the same thing.
- `foldLSent_` and `foldLSent` - Found in `Utils.Drasil`, these functions combine a list of `Sentences` into one long `Sentence` by placing spaces in between each `Sentence` element. However, the one with the underscore does not include a period at the end, while `foldLSent` does. These functions are often used with `+:+` and `+:+.`, so be on the lookout for double missing periods.

Other functions to watch out for that use the `+:+.` operator:
- [`definedIn`](https://jacquescarette.github.io/Drasil/docs/full/drasil-utils-0.1.1.0/Utils-Drasil.html#v:definedIn)
- [`definedIn'`](https://jacquescarette.github.io/Drasil/docs/full/drasil-utils-0.1.1.0/Utils-Drasil.html#v:definedIn')
- [`underConsidertn`](https://jacquescarette.github.io/Drasil/docs/full/drasil-utils-0.1.1.0/Utils-Drasil.html#v:underConsidertn)
- [`tAndDWAcc`](https://jacquescarette.github.io/Drasil/docs/full/drasil-utils-0.1.1.0/Utils-Drasil.html#v:tAndDWAcc)
- [`tAndDWSym`](https://jacquescarette.github.io/Drasil/docs/full/drasil-utils-0.1.1.0/Utils-Drasil.html#v:tAndDWSym)
- [`tAndDOnly`](https://jacquescarette.github.io/Drasil/docs/full/drasil-utils-0.1.1.0/Utils-Drasil.html#v:tAndDOnly)
## Capitalization
The primary forms of capitalization within Drasil (aside from hard-coding capital letters) are to use the functions [`atStart`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:atStart), [`atStartNP`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:atStartNP), [`titleize`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:titleize) and [`titleizeNP`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:titleizeNP) (and their plural variants). Often, these work as expected: `atStart` will capitalize the first letter of a `Sentence` (as if it were at the start of a sentence), and `titleize` will capitalize all words in a `Sentence`. However, there are some specific cases where using these functions in Drasil may cause odd behaviour. Sometimes, capitalizing a term with symbols may cause that term to render with lowercase letters only. This is currently being addressed in [#2651](https://github.com/JacquesCarette/Drasil/issues/2651), but for more information on the way Drasil works with capitalization, see `drasil-lang/Drasil/NounPhrase.hs` (specifically, the `cap` function). There were also some known issues with using combinators and capitalization, but were addressed in [#2635](https://github.com/JacquesCarette/Drasil/issues/2635) and [#2489](https://github.com/JacquesCarette/Drasil/issues/2489).
## Wrong Term Appearing
Often a rarer problem in Drasil, sometimes an unexpected term may pop up in an SRS. To investigate:

1. Find the unuxpected term in the generated HTML or PDF document. Remember the section name (eg. Specific System Description, General System Description, etc.)
2. Navigate through the example files to find the section where the term was used. If the section file is not obvious, try looking through `Body.hs` or the other sections around it. `CTRL+F` is your friend here.
3. If you have found the problem, try and address a proper fix for it. This may often be caused by using a wrong term or hardcoded text in the wrong spot.
4. If the problem has not been resolved, it may be have something to do with the files in `drasil-docLang`. Unfortunately, there is not really any good advice for this step other than to try and track where that term is used (using section names, `grep`, and `CTRL+F` should help some).
5. If the problem persists, it could be that a `UID` is being used for two different things. Double check to make sure each UID is unique to a specific Chunk.

# Notes for `Reference: not found in ReferenceMap` Errors
This type of error can also be quite tricky to resolve, as there are many different and ever-changing parts of Drasil that rely on all the References being in a Reference Map. In order of likely cause or places to look through first:

1. Start off by using `grep` on the `UID` that appears and briefly searching through the path that `UID` takes. Knowing where your functions are coming from and then following through to the places they are used will always be helpful. You might even find a mistake in one of your other functions that indirectly use that specific `UID`.
2. Check your `Main.hs` and `Body.hs` files and make sure that all the functions take the complete system information (`fullSI`) rather than the incomplete one (`si`). Since references are automatically collected, users need to make sure that the generator functions (notably `srs`, `printSetting`, `genDot`, and `code`) always take in the complete system information (defined as `fullSI` under most examples).
3. Check `drasil-docLang/DocumentLanguage.hs` to find more information on how exactly the references are gathered. This may help give you an idea of where a possible problem could arise.
4. Make sure all your `HasRefAddress` content is in the database. This includes data definitions, general definitions, theory models, instance models, labelled content, sections, concept instances, etc. For more information on the `HasRefAddress` class, please see the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:HasRefAddress).
5. If all else fails, you can manually add in your own `References` to the chunk database by placing them in the last argument of the `symbMap` function found in the `Body.hs` file of every example. For clarity, please see the excerpt from DblPendulum below:
```Haskell
symbMap :: ChunkDB
symbMap = cdb (map qw iMods ++ map qw symbols)
  (nw newtonSLR : nw pendulumTitle : nw mass : nw len : nw kilogram : nw inValue : nw newton : nw degree : nw radian
    : nw unitVect : nw unitVectj : [nw errMsg, nw program] ++ map nw symbols ++
   map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw mathcon ++ map nw mathcon' ++ map nw physicCon' ++
   map nw physicscon ++ concepts ++ map nw physicalcon ++ map nw acronyms ++ map nw symbols ++ map nw [metre, hertz] ++
   [nw algorithm] ++ map nw compcon ++ map nw educon ++ map nw prodtcon)
  (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz]) dataDefs
  iMods genDefns tMods concIns [] [] ([] :: [Reference]) -- add your references here
```
