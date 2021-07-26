{-# LANGUAGE PackageImports #-}
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
import qualified Drasil.Template.Body as Template (fullSI)

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
allExampleSI = [DblPendulum.fullSI, GamePhysics.fullSI, GlassBR.fullSI, HGHC.fullSI, NoPCM.fullSI, PDController.fullSI, Projectile.fullSI, SglPendulum.fullSI, SSP.fullSI, SWHS.fullSI, Template.fullSI]

-- | Records example descriptions.
allExampleDesc :: [Sentence]
allExampleDesc = [dblPendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc, projectileDesc, sglPendulumDesc, sspDesc, swhsDesc, templateDesc]

-- TODO: Automate this somehow. It seems a little too hard-coded.
-- To developer: Fill this list in when more examples can run code. The list
-- needs to be of this form since projectile comes with a list of choice combos.
-- | Records example choices. The order of the list must match up with
-- that in `allExampleSI`, or the Case Studies Table will be incorrect.
allExampleChoices :: [[Choices]]
allExampleChoices = [[], [], [GlassBR.choices], [], [NoPCM.choices], [PDController.codeChoices], Projectile.choiceCombos, [], [], [], []]

-- | Zip system info, description, choices, and file paths for the examples.
allExamples :: [SystemInformation] -> [Sentence] -> [[Choices]] -> FilePath -> FilePath -> [Example]
allExamples si desc choi srsP doxP = zipWith3 (\x y z -> E x y z srsP doxP) si desc choi

-- | Calls 'allExamples' on 'allExampleSI', 'allExampleDesc', and 'allExampleChoices'.
examples :: FilePath -> FilePath -> [Example]
examples = allExamples allExampleSI allExampleDesc allExampleChoices

fullExList :: FilePath -> FilePath -> RawContent
fullExList p1 p2 = Enumeration $ Bullet $ zip (allExampleList $ examples p1 p2) $ repeat Nothing

allExampleList :: [Example] -> [ItemType]
allExampleList = map (\x -> Nested (nameAndDesc x) $ Bullet $ zip (individualExList x) $ repeat Nothing)
  where
    nameAndDesc E{sysInfoE = SI{_sys = sys}, descE = desc} = S (abrv sys) +:+ desc


individualExList :: Example -> [ItemType]
individualExList E{sysInfoE = SI{_sys = sys}, choicesE = [], srsPath = srsP} = 
  [Flat $ S (abrv sys ++ "_SRS") +:+ namedRef (getHTMLRef srsP (abrv sys)) (S "[HTML]") +:+ namedRef (getPDFRef srsP (abrv sys)) (S "[PDF]")]
individualExList ex@E{sysInfoE = SI{_sys = sys}, srsPath = srsP} = 
  [Flat $ S (abrv sys ++ "_SRS") +:+ namedRef (getHTMLRef srsP (abrv sys)) (S "[HTML]") +:+ namedRef (getPDFRef srsP (abrv sys)) (S "[PDF]"),
  Nested (S generatedCodeTitle) $ Bullet $ zip (versionList getCodeRef ex) $ repeat Nothing,
  Nested (S generatedCodeDocsTitle) $ Bullet $ zip (versionList getDoxRef noSwiftEx) $ repeat Nothing]
    where
      noSwiftEx = ex {choicesE = map (\x -> x {lang = filter (/= Swift) $ lang x}) $ choicesE ex}

-- Takes a function that gets the needed references (may be for code itself or doxygen references)
versionList :: (Example -> Lang -> String -> Reference) -> Example -> [ItemType]
versionList _ E{choicesE = []} = []
versionList getRef ex@E{sysInfoE = SI{_sys = sys}, choicesE = [chs]} =
  [Flat $ S (abrv sys) +:+ foldlSent_ (map (\x -> namedRef (getRef ex x "") $ S $ "[" ++ showLang x ++ "]") $ lang chs)]
versionList getRef ex@E{sysInfoE = SI{_sys = sys}, choicesE = chs} =
  map (\x -> Flat $ S (verName x) +:+ foldlSent_ (map (\y -> namedRef (getRef ex y $ verName x) $ S $ "[" ++ showLang y ++ "]") $ lang x)) chs
  where 
    verName = Projectile.codedDirName (abrv sys)

showLang :: Lang -> String
showLang Cpp = "C++"
showLang CSharp = "C Sharp" -- Drasil printers dont like # symbol, so use full word instead.
showLang l = show l

convertLang :: Lang -> String
convertLang Cpp = "cpp"
convertLang CSharp = "csharp"
convertLang Java = "java"
convertLang Python = "python"
convertLang Swift = "swift"

getCodeRef :: Example -> Lang -> String -> Reference
getCodeRef ex@E{sysInfoE=SI{_sys = sys}, choicesE = [_]} l _ = 
  Reference ("codeRef" ++ sysName ++ programLang) (URI $ getCodePath (srsPath ex) sysName programLang) $ shortname' $ S $ "codeRef" ++ sysName ++ programLang
  where
    sysName :: String
    sysName = map toLower $ filter (not.isSpace) $ abrv sys
    programLang :: String
    programLang = convertLang l
getCodeRef ex@E{sysInfoE=SI{_sys = sys}} l verName = 
  Reference ("codeRef" ++ sysName ++ programLang) (URI $ getCodePath (srsPath ex) sysName programLang) $ shortname' $ S $ "codeRef" ++ sysName ++ programLang
  where
    sysName :: String
    sysName = (map toLower $ filter (not.isSpace) $ abrv sys) ++ "/" ++ verName
    programLang :: String
    programLang = convertLang l

getDoxRef :: Example -> Lang -> String -> Reference
-- dont have to worry about the case of empty list, since that was checked in an earlier function.
getDoxRef ex@E{sysInfoE=SI{_sys = sys}, choicesE = [_]} l _ = 
  Reference ("doxRef" ++ sysName ++ programLang) (URI $ getDoxPath (doxPath ex) sysName programLang) $ shortname' $ S $ "doxRef" ++ sysName ++ programLang
  where
    sysName :: String
    sysName = filter (not.isSpace) $ abrv sys
    programLang :: String
    programLang = convertLang l
getDoxRef ex@E{sysInfoE=SI{_sys = sys}} l verName = 
  Reference ("doxRef" ++ sysName ++ programLang) (URI $ getDoxPath (doxPath ex) sysName programLang) $ shortname' $ S $ "doxRef" ++ sysName ++ programLang
  where
    sysName :: String
    sysName = filter (not.isSpace) (abrv sys)
    programLang :: String
    programLang = verName ++ "/" ++ convertLang l
--------------------------
-- Examples Section
--------------------------

exampleSec :: FilePath -> FilePath -> Section
exampleSec p1 p2 = section (S exampleTitle) [mkParagraph exampleIntro, UlC $ ulcc $ fullExList p1 p2] [] exampleSecRef

exampleTitle :: String
exampleTitle = "Generated Examples"

exampleIntro :: Sentence
exampleIntro = S "Each of the case studies contain their own generated PDF and HTML reports," +:+
  S "and in some cases, their own generated code."

-- Make references for each of the generated SRS files
getHTMLRef, getPDFRef :: FilePath -> String -> Reference
getHTMLRef path ex = Reference ("htmlRef" ++ ex) (URI (getHTMLPath path ex)) (shortname' $ S ("htmlRef" ++ ex))
getPDFRef path ex = Reference ("pdfRef" ++ ex) (URI (getPDFPath path ex)) (shortname' $ S ("pdfRef" ++ ex))
getHTMLPath, getPDFPath :: FilePath -> String -> FilePath
getHTMLPath path ex = path ++ filter (not.isSpace) ex ++ "/srs/" ++ filter (not.isSpace) ex ++ "_SRS.html"
getPDFPath path ex = path ++ filter (not.isSpace) ex ++ "/srs/" ++ filter (not.isSpace) ex ++ "_SRS.pdf"


getCodePath :: FilePath -> String -> String -> FilePath
getDoxPath :: FilePath -> String -> String -> FilePath
getCodePath path ex programLang = path ++ "code/stable/" ++ filter (not.isSpace) ex ++ "/src/" ++ programLang -- need repoCommit path
getDoxPath path ex programLang = path ++ filter (not.isSpace) ex ++ "/doxygen/" ++ programLang ++ "/index.html" -- need example path

-- Project descriptions
sglPendulumDesc, dblPendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc,
  projectileDesc, sspDesc, swhsDesc, templateDesc :: Sentence

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
templateDesc     = S "is an empty template document."

generatedCodeTitle, generatedCodeDocsTitle :: String
generatedCodeTitle = "Generated Code:"
generatedCodeDocsTitle = "Generated Code Documentation:"

exampleRefs :: FilePath -> FilePath -> [Reference]
exampleRefs p1 p2 = [exampleSecRef, ref $ exampleSec p1 p2] ++
  concatMap getCodeRefDB (examples p1 p2) ++
  concatMap getDoxRefDB (examples p1 p2) ++
  map (getHTMLRef p2) (map getAbrv $ examples p1 p2) ++ map (getPDFRef p2) (map getAbrv $ examples p1 p2)

getCodeRefDB, getDoxRefDB :: Example -> [Reference]
getCodeRefDB ex = concatMap (\x -> map (\y -> getCodeRef ex y $ verName x) $ lang x) $ choicesE ex
  where
    verName z = Projectile.codedDirName (getAbrv ex) z
getDoxRefDB ex = concatMap (\x -> map (\y -> getDoxRef ex y $ verName x) $ lang x) $ choicesE ex
  where
    verName z = Projectile.codedDirName (getAbrv ex) z

getAbrv :: Example -> String
getAbrv E{sysInfoE = SI{_sys=sys}} = abrv sys

exampleSecRef :: Reference
exampleSecRef = makeSecRef "Examples" $ S "Examples"