{-# LANGUAGE TupleSections #-}

-- | Create the list of Generated Examples for the Drasil website.
module Drasil.Website.Example where

import Language.Drasil hiding (E)
import Drasil.System (System(..))
import Language.Drasil.Code (Choices(..), Lang(..))
import Data.Char (toLower)
import Language.Drasil.Printers (Format(..))

import qualified Drasil.DblPend.Body as DblPend (fullSI)
import qualified Drasil.GamePhysics.Body as GamePhysics (fullSI)
import qualified Drasil.GlassBR.Body as GlassBR (fullSI)
import qualified Drasil.HGHC.Body as HGHC (fullSI)
import qualified Drasil.SWHSNoPCM.Body as NoPCM (fullSI)
import qualified Drasil.PDController.Body as PDController (fullSI)
import qualified Drasil.Projectile.Body as Projectile (fullSI)
import qualified Drasil.SglPend.Body as SglPend (fullSI)
import qualified Drasil.SSP.Body as SSP (fullSI)
import qualified Drasil.SWHS.Body as SWHS (fullSI)

-- import choices for code generation
import qualified Drasil.DblPend.Choices as DblPend (choices)
import qualified Drasil.GlassBR.Choices as GlassBR (choices)
import qualified Drasil.SWHSNoPCM.Choices as NoPCM (choices)
import qualified Drasil.PDController.Choices as PDController (codeChoices)
import qualified Drasil.Projectile.Choices as Projectile (codedDirName, choiceCombos)
-- the other examples currently do not generate any code.


-- * Gather Example Information
--
-- $example
--
-- First gather all information needed to create an example. This includes system information, descriptions, and choices.
-- These will also be exported for use in CaseStudy.hs.

-- | Each Example gets placed in here.
data Example = E {
  -- | Example system information. Used to get the system name and abbreviation.
  systemE :: System,
  -- | Some examples have generated code with specific choices.
  -- They may also have more than one set of choices, so we need a list.
  choicesE :: [Choices],
  -- | Generated code path.
  codePath :: FilePath,
  -- | Generated documents & doxygen path
  srsDoxPath :: FilePath
}
-- TODO: Automate the gathering of system information, descriptions, and choices.

-- | Records example system information.
allExampleSI :: [System]
allExampleSI = [DblPend.fullSI, GamePhysics.fullSI, GlassBR.fullSI, HGHC.fullSI, NoPCM.fullSI, PDController.fullSI, Projectile.fullSI, SglPend.fullSI, SSP.fullSI, SWHS.fullSI]

-- To developer: Fill this list in when more examples can run code. The list
-- needs to be of this form since projectile comes with a list of choice combos.
-- | Records example choices. The order of the list must match up with
-- that in `allExampleSI`, or the Case Studies Table will be incorrect.
allExampleChoices :: [[Choices]]
allExampleChoices = [[DblPend.choices], [], [GlassBR.choices], [], [NoPCM.choices], [PDController.codeChoices], Projectile.choiceCombos, [], [], []]

-- | Combine system info, description, choices, and file paths into one nice package.
allExamples :: [System] -> [[Choices]] -> FilePath -> FilePath -> [Example]
allExamples si choi srsP doxP = zipWith (\x y -> E x y srsP doxP) si choi

-- | Calls 'allExamples' on 'allExampleSI', 'allExampleDesc', and 'allExampleChoices'.
-- Can be considered a "default" version of 'allExamples'.
examples :: FilePath -> FilePath -> [Example]
examples = allExamples allExampleSI allExampleChoices

-- * Functions to create the list of examples

-- | Create the full list of examples.
fullExList :: FilePath -> FilePath -> RawContent
fullExList codePth srsDoxPth = Enumeration $ Bullet $ map (, Nothing) (allExampleList $ examples codePth srsDoxPth)

-- | Create each example point and call 'individualExList' to do the rest.
allExampleList :: [Example] -> [ItemType]
allExampleList = map (\x -> Nested (nameAndDesc x) $ Bullet $ map (, Nothing) (individualExList x))
  where
    nameAndDesc E{systemE = SI{_sys = sys, _purpose = purp}} = S (abrv sys) +:+ S " - To" +:+. head purp

-- | Display the points for generated documents and call 'versionList' to display the code.
individualExList :: Example -> [ItemType]
-- No choices mean no generated code, so we do not need to display generated code and thus do not call versionList.
individualExList ex@E{systemE = SI{_sys = sys}, choicesE = [], codePath = srsP} = 
  [Flat $ namedRef (buildDrasilExSrcRef ex) (S "Drasil Source Code"),
  Flat $ S "SRS:" +:+ namedRef (getSRSRef srsP HTML $ programName sys) (S "[HTML]") 
  +:+ namedRef (getSRSRef srsP TeX $ programName sys) (S "[PDF]") 
  +:+ namedRef (getSRSRef srsP MDBook $ programName sys) (S "[mdBook]")]
-- Anything else means we need to display program information, so use versionList.
individualExList ex@E{systemE = SI{_sys = sys}, codePath = srsP} = 
  [Flat $ namedRef (buildDrasilExSrcRef ex) (S "Drasil Source Code"),
  Flat $ S "SRS:" +:+ namedRef (getSRSRef srsP HTML $ programName sys) (S "[HTML]") 
  +:+ namedRef (getSRSRef srsP TeX $ programName sys) (S "[PDF]")
  +:+ namedRef (getSRSRef srsP MDBook $ programName sys) (S "[mdBook]")
  +:+ namedRef (getSRSRef srsP Jupyter $ programName sys) (S "[Jupyter (HTML)]"),
  Nested (S generatedCodeTitle) $ Bullet $ map (, Nothing) (versionList getCodeRef ex),
  Nested (S generatedCodeDocsTitle) $ Bullet $ map (, Nothing) (versionList getDoxRef noSwiftJlEx)]
    where
      -- For now, swift does not generate any references using doxygen, so we pretend it doesn't exist in the doxygen list
      noSwiftJlEx = ex {choicesE = map (\x -> x {lang = filter 
        (\l -> l /= Swift && l /= Julia) $ lang x}) $ choicesE ex}

-- | Takes a function that gets the needed references (either references for the code or doxygen references)
-- and the example to create the list out of. For examples that have more than one version of generated code (more than one set of choices)
-- like Projectile, we generate the code and doxygen references for each.
versionList :: (Example -> Lang -> String -> Reference) -> Example -> [ItemType]
versionList _ E{choicesE = []} = [] -- If the choices are empty, then we don't do anything. This pattern should never
                                    -- match (this case should be caught in the function that calls this one),
                                    -- but it is here just to be extra careful.
versionList getRef ex@E{systemE = SI{_sys = sys}, choicesE = chcs} =
  map versionItem chcs 
  where
    -- Version item displays version name and appends the languages of generated code below.
    versionItem chc = Flat $ S (verName chc) +:+ foldlSent_ (map (makeLangRef chc) $ lang chc)
    -- Makes references to the generated languages and formats them nicely.
    makeLangRef chc lng = namedRef (getRef ex lng $ verName chc) $ S $ "[" ++ showLang lng ++ "]"

    -- Determine the version name based on the system name and if there is more than one set of choices.
    verName chc = case chcs of
      -- If there is one set of choices, then the program does not have multiple versions.
      [_] -> programName sys
      -- If the above two don't match, we have more than one set of choices and must display every version.
      _   -> Projectile.codedDirName (programName sys) chc

-- | Show function to display program languages to user.
showLang :: Lang -> String
showLang Cpp = "C++"
showLang CSharp = "C Sharp" -- Drasil printers dont like # symbol, so use full word instead.
showLang l = show l

-- * Examples Section Functions

-- | Example section function generator. Makes a list of examples and generated artifacts.
exampleSec :: FilePath -> FilePath -> Section
exampleSec codePth srsDoxPth = 
  section exampleTitle -- Title
  [mkParagraph exampleIntro, UlC $ ulcc $ fullExList codePth srsDoxPth] -- Contents
  [] $ makeSecRef "Examples" $ S "Examples" -- Section reference

-- | Example section title.
exampleTitle :: Sentence
exampleTitle = S "Generated Examples"

-- | Example section introduction.
exampleIntro :: Sentence
exampleIntro = S "The development of Drasil follows an example-driven approach, \
  \with a current focus on creating Software Requirement Specifications (SRS). \
  \More specifically, Drasil's knowledge of the domain of Physics has seen significant growth \
  \through the creation of these examples, ranging from mechanics to thermodynamics. Each of the case studies \
  \implemented in Drasil contains its own generated PDF and HTML reports, and in some cases, \
  \its own generated code to solve the problem defined in their respective SRS documents."

-- | Example list titles.
generatedCodeTitle, generatedCodeDocsTitle :: String
generatedCodeTitle = "Generated Code:"
generatedCodeDocsTitle = "Generated Code Documentation:"

-- * Helper functions in getting References for SRS, code folders, and Doxygen

-- | Similar to 'showLang', but for use within Drasil for Referencing and UIDs.
convertLang :: Lang -> String
convertLang Cpp = "cpp"
convertLang CSharp = "csharp"
convertLang Java = "java"
convertLang Python = "python"
convertLang Swift = "swift"
convertLang Julia = "julia"

-- | Generate a reference towards the code folder. Uses 'getCodePath' to find the code path.
getCodeRef :: Example -> Lang -> String -> Reference
-- We don't have to worry about the case of empty list when pattern matching
-- since that was checked in an earlier function.
--
-- Pattern matches so that examples that only have a single set of choices will be referenced one way.
getCodeRef ex@E{systemE=SI{_sys = sys}, choicesE = chcs} l verName = 
  makeURI refUID refURI refShortNm
  where
    -- Append system name and program language to ensure a unique id for each.
    refUID = "codeRef" ++ sysName ++ programLang
    -- Finds the folder path that holds code for the respective program and system.
    refURI = getCodePath (codePath ex) sysName programLang
    -- Shortname is the same as the UID, just converted to a Sentence.
    refShortNm = shortname' $ S refUID

    -- System name, different between one set of choices and multiple sets.
    sysName = case chcs of 
      [_] -> map toLower $ programName sys
      _   -> map toLower (programName sys) ++ "/" ++ verName
    -- Program language converted for use in file folder navigation.
    programLang = convertLang l

-- | Similar to 'getCodeRef', but builds the source code references
buildDrasilExSrcRef :: Example -> Reference
buildDrasilExSrcRef ex@E{systemE=SI{_sys = sys}} = 
  makeURI refUID refURI refShortNm
  where
    refUID = "srcCodeRef" ++ sysName
    refURI = path ++ "code/drasil-example/" ++ sysName
    refShortNm = shortname' $ S refUID
    sysName = map toLower $ programName sys
    path = codePath ex

-- | Similar to 'getCodeRef', but gets the doxygen references and uses 'getDoxRef' instead.
getDoxRef :: Example -> Lang -> String -> Reference
getDoxRef ex@E{systemE=SI{_sys = sys}, choicesE = chcs} l verName = 
  makeURI refUID refURI refShortNm
  where
    refUID = "doxRef" ++ progName ++ programLang
    refURI = getDoxPath (srsDoxPath ex) progName programLang
    refShortNm = shortname' $ S refUID

    progName = programName sys
    -- Here is the only difference from getCodeRef. When there is more than one set of choices,
    -- we append version name to program language since the organization of folders follows this way.
    programLang = case chcs of 
      [_] -> convertLang l
      _   -> map toLower verName ++ "/" ++ convertLang l

-- | Make references for each of the generated SRS files.
getSRSRef :: FilePath -> Format -> String -> Reference
getSRSRef path format ex = makeURI refUID (getSRSPath path format ex) $ shortname' $ S refUID
  where
    refUID = show format ++ "Ref" ++ ex

-- | Get the paths of where each reference exist for SRS files. Some example abbreviations have spaces,
-- so we just filter those out.
getSRSPath :: FilePath -> Format -> String -> FilePath
getSRSPath path format ex = path ++ map toLower ex ++ "/SRS/" ++ show format ++ "/" ++ sufx format
  where
    sufx MDBook  = "book"
    sufx HTML    = ex ++ "_SRS.html"
    sufx TeX     = ex ++ "_SRS.pdf"
    sufx Jupyter = ex ++ "_SRS.html"
    sufx Plain   = error "Plain SRS display is not supported." 

-- | Get the file paths for generated code and doxygen locations.
getCodePath, getDoxPath :: FilePath -> String -> String -> FilePath
-- | Uses 'repoRt' path (codePath in this module).
getCodePath path ex programLang = path ++ "code/stable/" ++ map toLower ex ++ "/src/" ++ programLang -- need repoCommit path
-- | Uses 'exRt' path (srsDoxPath in this module).
getDoxPath path ex programLang = path ++ map toLower ex ++ "/doxygen/" ++ programLang ++ "/index.html" -- need example path

-- | Gather all references used in making the Examples section.
exampleRefs :: FilePath -> FilePath -> [Reference]
exampleRefs codePth srsDoxPth = 
  concatMap getCodeRefDB (examples codePth srsDoxPth) ++ 
  concatMap getDoxRefDB (examples codePth srsDoxPth) ++ 
  map (getSRSRef srsDoxPth HTML . getAbrv) (examples codePth srsDoxPth) ++ 
  map (getSRSRef srsDoxPth TeX . getAbrv) (examples codePth srsDoxPth) ++ 
  map (getSRSRef srsDoxPth MDBook . getAbrv) (examples codePth srsDoxPth) ++ 
  map (getSRSRef srsDoxPth Jupyter . getAbrv) (examples codePth srsDoxPth) ++ 
  map buildDrasilExSrcRef (examples codePth srsDoxPth)

-- | Helpers to pull code and doxygen references from an example.
-- Creates a reference for every possible choice in every possible language.
getCodeRefDB, getDoxRefDB :: Example -> [Reference]
getCodeRefDB ex = concatMap (\x -> map (\y -> getCodeRef ex y $ verName x) $ lang x) $ choicesE ex
  where
    verName = Projectile.codedDirName (getAbrv ex)
getDoxRefDB ex = concatMap (\x -> map (\y -> getDoxRef ex y $ verName x) $ lang x) $ choicesE ex
  where
    verName = Projectile.codedDirName (getAbrv ex)

-- | Helper to pull the system name (abbreviation) from an 'Example'.
getAbrv :: Example -> String
getAbrv E{systemE = SI{_sys=sys}} = programName sys
