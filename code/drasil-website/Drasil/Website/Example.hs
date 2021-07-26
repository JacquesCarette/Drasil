module Drasil.Website.Example (exampleSec, exampleRefs)where

import Language.Drasil hiding (C, E)
import Database.Drasil (SystemInformation(..))
import Language.Drasil.Code (Choices(..), Lang(..))
import Utils.Drasil
import Data.Char (toLower, isSpace)

import qualified Drasil.DblPendulum.Body as DblPendulum (fullSI)
import qualified Drasil.GamePhysics.Body as GamePhysics (fullSI)
import qualified Drasil.GlassBR.Body as GlassBR (fullSI)
import qualified Drasil.HGHC.Body as HGHC (fullSI)
import qualified Drasil.NoPCM.Body as NoPCM (fullSI)
import qualified Drasil.PDController.Body as PDController (fullSI)
import qualified Drasil.Projectile.Body as Projectile (fullSI)
import qualified Drasil.SglPendulum.Body as SglPendulum (fullSI)
import qualified Drasil.SSP.Body as SSP (fullSI)
import qualified Drasil.SWHS.Body as SWHS (fullSI)
--import qualified Drasil.Template.Body as Template (fullSI)

-- import choices for code generation
import qualified Drasil.GlassBR.Choices as GlassBR (choices)
import qualified Drasil.NoPCM.Choices as NoPCM (choices)
import qualified Drasil.PDController.Choices as PDController (codeChoices)
import qualified Drasil.Projectile.Choices as Projectile (codedDirName, choiceCombos)
-- the other examples currently do not generate any code.


-- This may contain some repeated stuff from case studies, but I think this might hold more info, so we could define everything here and then import to CaseStudy.hs

----- First gather all information needed to create an example. This includes system information, descriptions, and choices.
-- | Each Example gets placed in here.
data Example = E {
  sysInfoE :: SystemInformation,
  descE :: Sentence,
  choicesE :: [Choices],
  srsPath :: FilePath,
  doxPath :: FilePath
}

-- | Records example system information.
allExampleSI :: [SystemInformation]
allExampleSI = [DblPendulum.fullSI, GamePhysics.fullSI, GlassBR.fullSI, HGHC.fullSI, NoPCM.fullSI, PDController.fullSI, Projectile.fullSI, SglPendulum.fullSI, SSP.fullSI, SWHS.fullSI]

-- | Records example descriptions.
allExampleDesc :: [Sentence]
allExampleDesc = [dblPendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc, projectileDesc, sglPendulumDesc, sspDesc, swhsDesc]

-- TODO: Automate this somehow. It seems a little too hard-coded.
-- To developer: Fill this list in when more examples can run code. The list
-- needs to be of this form since projectile comes with a list of choice combos.
-- | Records example choices. The order of the list must match up with
-- that in `allExampleSI`, or the Case Studies Table will be incorrect.
allExampleChoices :: [[Choices]]
allExampleChoices = [[], [], [GlassBR.choices], [], [NoPCM.choices], [PDController.codeChoices], Projectile.choiceCombos, [], [], []]

-- | Combine system info, description, choices, and file paths into one nice package.
allExamples :: [SystemInformation] -> [Sentence] -> [[Choices]] -> FilePath -> FilePath -> [Example]
allExamples si desc choi srsP doxP = zipWith3 (\x y z -> E x y z srsP doxP) si desc choi

-- | Calls 'allExamples' on 'allExampleSI', 'allExampleDesc', and 'allExampleChoices'.
-- Can be considered a "default" version of 'allExamples'.
examples :: FilePath -> FilePath -> [Example]
examples = allExamples allExampleSI allExampleDesc allExampleChoices


-------------------------------------------
-- Functions to create the list of examples
-------------------------------------------

-- | Create the full list of examples.
fullExList :: FilePath -> FilePath -> RawContent
fullExList p1 p2 = Enumeration $ Bullet $ zip (allExampleList $ examples p1 p2) $ repeat Nothing

-- | Create each example point and call 'individualExList' to do the rest.
allExampleList :: [Example] -> [ItemType]
allExampleList = map (\x -> Nested (nameAndDesc x) $ Bullet $ zip (individualExList x) $ repeat Nothing)
  where
    nameAndDesc E{sysInfoE = SI{_sys = sys}, descE = desc} = S (abrv sys) +:+ desc

-- | Display the points for generated documents and call 'versionList' to display the code.
individualExList :: Example -> [ItemType]
-- No choices mean no generated code, so we do not need versionList.
individualExList E{sysInfoE = SI{_sys = sys}, choicesE = [], srsPath = srsP} = 
  [Flat $ S (abrv sys ++ "_SRS") +:+ namedRef (getHTMLRef srsP (abrv sys)) (S "[HTML]") +:+ namedRef (getPDFRef srsP (abrv sys)) (S "[PDF]")]
-- Anything else means we need to display program information, so use versionList.
individualExList ex@E{sysInfoE = SI{_sys = sys}, srsPath = srsP} = 
  [Flat $ S (abrv sys ++ "_SRS") +:+ namedRef (getHTMLRef srsP (abrv sys)) (S "[HTML]") +:+ namedRef (getPDFRef srsP (abrv sys)) (S "[PDF]"),
  Nested (S generatedCodeTitle) $ Bullet $ zip (versionList getCodeRef ex) $ repeat Nothing,
  Nested (S generatedCodeDocsTitle) $ Bullet $ zip (versionList getDoxRef noSwiftEx) $ repeat Nothing]
    where
      -- For now, swift does not generate any references using doxygen, so we pretend it doesn't exist in the doxygen list
      noSwiftEx = ex {choicesE = map (\x -> x {lang = filter (/= Swift) $ lang x}) $ choicesE ex}

-- | Takes a function that gets the needed references (either references for the code or doxygen references)
-- and the example to create the list out of. For examples that have more than one version of generated code (more than one set of choices)
-- like Projectile, we generate the code and doxygen references for each.
versionList :: (Example -> Lang -> String -> Reference) -> Example -> [ItemType]
-- If the choices are empty, then we don't do anything. This pattern should never
-- match (this case should be caught in the function that calls this one),
-- but it is here just to be extra careful.
versionList _ E{choicesE = []} = []
-- If there is one set of choices, then the program does not have multiple versions.
versionList getRef ex@E{sysInfoE = SI{_sys = sys}, choicesE = [chs]} =
  [Flat $ S (abrv sys) +:+ foldlSent_ (map (\x -> namedRef (getRef ex x "") $ S $ "[" ++ showLang x ++ "]") $ lang chs)]
-- If the above two don't match, we have more than one set of choices and must display every version.
versionList getRef ex@E{sysInfoE = SI{_sys = sys}, choicesE = chs} =
  map (\x -> Flat $ S (verName x) +:+ foldlSent_ (map (\y -> namedRef (getRef ex y $ verName x) $ S $ "[" ++ showLang y ++ "]") $ lang x)) chs
  where 
    verName = Projectile.codedDirName (abrv sys)

-- | Show function to display program languages to user.
showLang :: Lang -> String
showLang Cpp = "C++"
showLang CSharp = "C Sharp" -- Drasil printers dont like # symbol, so use full word instead.
showLang l = show l


-----------------------------
-- Examples Section Functions
-----------------------------

-- | Example section function generator. Makes a list of examples and generated artifacts.
exampleSec :: FilePath -> FilePath -> Section
exampleSec p1 p2 = section exampleTitle [mkParagraph exampleIntro, UlC $ ulcc $ fullExList p1 p2] [] exampleSecRef

-- | Example section title.
exampleTitle :: Sentence
exampleTitle = S "Generated Examples"

-- | Example section introduction.
exampleIntro :: Sentence
exampleIntro = S "Each of the case studies contain their own generated PDF and HTML reports," +:+
  S "and in some cases, their own generated code."

-- | Project descriptions.
sglPendulumDesc, dblPendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc,
  projectileDesc, sspDesc, swhsDesc :: Sentence

dblPendulumDesc  = S "describes the motion of a double pendulum in 2-D dimension."
gamePhysDesc     = S "describes the modeling of an open source 2D rigid body physics library used for games."
glassBRDesc      = S "predicts whether a given glass slab is likely to resist a specified blast."
hghcDesc         = S "describes heat transfer coefficients related to clad."
noPCMDesc        = S "describes the modelling of a solar water heating system without phase change material."
pdControllerDesc = S ""
projectileDesc   = S "describes the motion of a projectile object in free space."
sglPendulumDesc  = S "describes the motion of a single pendulum in 2-D dimension."
sspDesc          = S "describes the requirements of a slope stability analysis program."
swhsDesc         = S "describes the modelling of a solar water heating system with phase change material."
-- templateDesc     = S "is an empty template document."

-- | Example list titles.
generatedCodeTitle, generatedCodeDocsTitle :: String
generatedCodeTitle = "Generated Code:"
generatedCodeDocsTitle = "Generated Code Documentation:"

----------------------------------------------------------------------------
-- Helper functions in getting References for SRS, code folders, and Doxygen
----------------------------------------------------------------------------

-- | Similar to 'showLang', but for use within Drasil for Referencing and UIDs.
convertLang :: Lang -> String
convertLang Cpp = "cpp"
convertLang CSharp = "csharp"
convertLang Java = "java"
convertLang Python = "python"
convertLang Swift = "swift"

-- | Generate a reference towards the code folder. Uses 'getCodePath' to find the code path.
getCodeRef :: Example -> Lang -> String -> Reference
-- We don't have to worry about the case of empty list, since that was checked in an earlier function.
-- Pattern matches so that examples that only have a single set of choices will be referenced one way.
getCodeRef ex@E{sysInfoE=SI{_sys = sys}, choicesE = [_]} l _ = 
  Reference ("codeRef" ++ sysName ++ programLang) (URI $ getCodePath (srsPath ex) sysName programLang) $ shortname' $ S $ "codeRef" ++ sysName ++ programLang
  where
    sysName = map toLower $ filter (not.isSpace) $ abrv sys
    programLang = convertLang l
-- And all the other Examples must have more than one set of choices, so reference a different way.
getCodeRef ex@E{sysInfoE=SI{_sys = sys}} l verName = 
  Reference ("codeRef" ++ sysName ++ programLang) (URI $ getCodePath (srsPath ex) sysName programLang) $ shortname' $ S $ "codeRef" ++ sysName ++ programLang
  where
    sysName = map toLower (filter (not.isSpace) $ abrv sys) ++ "/" ++ verName
    programLang = convertLang l

-- | Similar to 'getCodeRef', but gets the doxygen references and uses 'getDoxRef' instead.
getDoxRef :: Example -> Lang -> String -> Reference
getDoxRef ex@E{sysInfoE=SI{_sys = sys}, choicesE = [_]} l _ = 
  Reference ("doxRef" ++ sysName ++ programLang) (URI $ getDoxPath (doxPath ex) sysName programLang) $ shortname' $ S $ "doxRef" ++ sysName ++ programLang
  where
    sysName = filter (not.isSpace) $ abrv sys
    programLang = convertLang l
getDoxRef ex@E{sysInfoE=SI{_sys = sys}} l verName = 
  Reference ("doxRef" ++ sysName ++ programLang) (URI $ getDoxPath (doxPath ex) sysName programLang) $ shortname' $ S $ "doxRef" ++ sysName ++ programLang
  where
    sysName = filter (not.isSpace) (abrv sys)
    -- Append version name to program language since the organization of folders follows this way.
    programLang = verName ++ "/" ++ convertLang l

-- | Make references for each of the generated SRS files.
getHTMLRef, getPDFRef :: FilePath -> String -> Reference
getHTMLRef path ex = Reference ("htmlRef" ++ ex) (URI (getHTMLPath path ex)) (shortname' $ S ("htmlRef" ++ ex))
getPDFRef path ex = Reference ("pdfRef" ++ ex) (URI (getPDFPath path ex)) (shortname' $ S ("pdfRef" ++ ex))

-- | Get the paths of where each reference exist for SRS files.
getHTMLPath, getPDFPath :: FilePath -> String -> FilePath
getHTMLPath path ex = path ++ filter (not.isSpace) ex ++ "/srs/" ++ filter (not.isSpace) ex ++ "_SRS.html"
getPDFPath path ex = path ++ filter (not.isSpace) ex ++ "/srs/" ++ filter (not.isSpace) ex ++ "_SRS.pdf"

-- | Get the file paths for generated code and doxygen locations.
getCodePath, getDoxPath :: FilePath -> String -> String -> FilePath
-- | Uses 'repoRt' path (srsPath in this module).
getCodePath path ex programLang = path ++ "code/stable/" ++ filter (not.isSpace) ex ++ "/src/" ++ programLang -- need repoCommit path
-- | Uses 'exRt' path (doxPath in this module).
getDoxPath path ex programLang = path ++ filter (not.isSpace) ex ++ "/doxygen/" ++ programLang ++ "/index.html" -- need example path


-- | Gather all references used in making the Examples section.
exampleRefs :: FilePath -> FilePath -> [Reference]
exampleRefs p1 p2 = [exampleSecRef, ref $ exampleSec p1 p2] ++
  concatMap getCodeRefDB (examples p1 p2) ++
  concatMap getDoxRefDB (examples p1 p2) ++
  map (getHTMLRef p2 . getAbrv) (examples p1 p2) ++ map (getPDFRef p2 . getAbrv) (examples p1 p2)

-- | Helpers to pull code and doxygen references from an example.
getCodeRefDB, getDoxRefDB :: Example -> [Reference]
getCodeRefDB ex = concatMap (\x -> map (\y -> getCodeRef ex y $ verName x) $ lang x) $ choicesE ex
  where
    verName z = Projectile.codedDirName (getAbrv ex) z
getDoxRefDB ex = concatMap (\x -> map (\y -> getDoxRef ex y $ verName x) $ lang x) $ choicesE ex
  where
    verName z = Projectile.codedDirName (getAbrv ex) z

-- | Helper to pull the system name (abbreviation) from an 'Example'.
getAbrv :: Example -> String
getAbrv E{sysInfoE = SI{_sys=sys}} = abrv sys

-- | Example section reference.
exampleSecRef :: Reference
exampleSecRef = makeSecRef "Examples" $ S "Examples"