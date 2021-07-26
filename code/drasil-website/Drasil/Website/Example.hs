{-# LANGUAGE PackageImports #-}
module Drasil.Website.Example (exampleSec, exampleRefs)where

import Data.List (zipWith4, isPrefixOf)
import Language.Drasil hiding (C, E)
import Database.Drasil (SystemInformation(..))
import Language.Drasil.Code (Choices(..), Lang(..))
import Utils.Drasil
import Data.Char (toUpper, toLower, isSpace)
import qualified Data.List.Split as L


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

allExampleDesc :: [Sentence]
allExampleDesc = [dblPendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc, projectileDesc, sglPendulumDesc, sspDesc, swhsDesc, templateDesc]

-- TODO: Automate this somehow. It seems a little too hard-coded.
-- To developer: Fill this list in when more examples can run code. The list
-- needs to be of this form since projectile comes with a list of choice combos.
-- | Records example choices. The order of the list must match up with
-- that in `allExampleSI`, or the Case Studies Table will be incorrect.
allExampleChoices :: [[Choices]]
allExampleChoices = [[], [], [GlassBR.choices], [], [NoPCM.choices], [PDController.codeChoices], Projectile.choiceCombos, [], [], [], []]

-- | Zip system info, description, and choices from the examples.
allExamples :: [SystemInformation] -> [Sentence] -> [[Choices]] -> FilePath -> FilePath -> [Example]
allExamples si desc choi srsP doxP = zipWith3 (\x y z -> E x y z srsP doxP) si desc choi

examplesTest :: FilePath -> FilePath -> [Example]
examplesTest = allExamples allExampleSI allExampleDesc allExampleChoices

fullExList :: FilePath -> FilePath -> RawContent
fullExList p1 p2 = Enumeration $ Bullet $ zip (allExampleList $ examplesTest p1 p2) $ repeat Nothing

allExampleList :: [Example] -> [ItemType]
allExampleList = map (\x -> Nested (nameAndDesc x) $ Bullet $ zip (individualExList x) $ repeat Nothing)
  where
    nameAndDesc E{sysInfoE = SI{_sys = sys}, descE = desc} = S (abrv sys) +:+ desc


individualExList :: Example -> [ItemType]
individualExList ex@E{sysInfoE = SI{_sys = sys}, choicesE = [], srsPath = srsP} = 
  [Flat $ S (abrv sys ++ "_SRS") +:+ namedRef (getHTMLRef srsP (abrv sys)) (S "[HTML]") +:+ namedRef (getPDFRef srsP (abrv sys)) (S "[PDF]")]
individualExList ex@E{sysInfoE = SI{_sys = sys}, srsPath = srsP} = 
  [Flat $ S (abrv sys ++ "_SRS") +:+ namedRef (getHTMLRef srsP (abrv sys)) (S "[HTML]") +:+ namedRef (getPDFRef srsP (abrv sys)) (S "[PDF]"),
  Nested (S generatedCodeTitle) $ Bullet $ zip (versionList getCodeRef ex) $ repeat Nothing,
  Nested (S generatedCodeDocsTitle) $ Bullet $ zip (versionList getDoxRef noSwiftEx) $ repeat Nothing]
    where
      noSwiftEx = ex {choicesE = map (\x -> x {lang = filter (/= Swift) $ lang x}) $ choicesE ex}

-- Takes a function that gets the needed references (may be for code itself or doxygen references)
versionList :: (Example -> Lang -> Reference) -> Example -> [ItemType]
versionList _ E{choicesE = []} = []
versionList getRef ex@E{sysInfoE = SI{_sys = sys}, choicesE = [chs]} =
  [Flat $ S (abrv sys) +:+ foldlSent_ (map (\x -> namedRef (getRef ex x) $ S $ "[" ++ showLang x ++ "]") $ lang chs)]
versionList getRef ex@E{sysInfoE = SI{_sys = sys}, choicesE = chs} =
  map (\x -> Flat $ S (Projectile.codedDirName (abrv sys) x) +:+ foldlSent_ (map (\y -> namedRef (getRef ex y) $ S $ "[" ++ showLang y ++ "]") $ lang x)) chs

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

getCodeRef :: Example -> Lang -> Reference
getCodeRef ex@E{sysInfoE=SI{_sys = sys}} l = 
  Reference ("codeRef" ++ sysName ++ lang) (URI $ getCodePath (srsPath ex) sysName lang) $ shortname' $ S $ "codeRef" ++ sysName ++ lang
  where
    sysName :: String
    sysName = map toLower $ filter (not.isSpace) $ abrv sys
    lang :: String
    lang = convertLang l
getDoxRef :: Example -> Lang -> Reference
getDoxRef ex@E{sysInfoE=SI{_sys = sys}} l = 
  Reference ("doxRef" ++ sysName ++ lang) (URI $ getDoxPath (doxPath ex) sysName lang) $ shortname' $ S $ "doxRef" ++ sysName ++ lang
  where
    sysName :: String
    sysName = filter (not.isSpace) $ abrv sys
    lang :: String
    lang = convertLang l
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

{-- Put the list into RawContent form.
mkExampleList :: FilePath -> FilePath -> RawContent
mkExampleList p1 p2 = Enumeration $ exampleList p1 p2

-- Put the examples into a list.
exampleList :: FilePath -> FilePath -> ListType
exampleList repoPth exPth = Bullet $ zip (zipWith4 (mkExampleListFunc exPth) exampleTitles exampleDescs (exampleCodeRefs repoPth) (exampleDoxRefs exPth)) $ repeat Nothing

-- Organize the examples into a bulleted list.
mkExampleListFunc :: FilePath -> String -> String -> [(String, [(Sentence, Reference)])] -> [(String, [(Sentence, Reference)])] -> ItemType
mkExampleListFunc path exmpl desc codePth doxPth
  | map (map snd . snd) codePth == [[]] && map (map snd . snd) doxPth == [[]] = Nested (S exmpl +:+ S desc) $ Bullet [(Flat (S (exmpl ++ "_SRS") +:+ namedRef (getHTMLRef path exmpl) (S "[HTML]") +:+ namedRef (getPDFRef path exmpl) (S "[PDF]")), Nothing)]
  | map (map snd . snd) doxPth == [[]]                            = Nested (S exmpl +:+ S desc) $ Bullet $ zip [Flat $ S (exmpl ++ "_SRS") +:+ namedRef (getHTMLRef path exmpl) (S "[HTML]") +:+ namedRef (getPDFRef path exmpl) (S "[PDF]"),
                                                                       Nested (S generatedCodeTitle) $ Bullet $ mkCodeList codePth] $ repeat Nothing
  | otherwise                                         = Nested (S exmpl +:+ S desc) $ Bullet $ zip [Flat $ S (exmpl ++ "_SRS") +:+ namedRef (getHTMLRef path exmpl) (S "[HTML]") +:+ namedRef (getPDFRef path exmpl) (S "[PDF]"),
                                                                       Nested (S generatedCodeTitle) $ Bullet $ mkCodeList codePth,
                                                                       Nested (S generatedCodeDocsTitle) $ Bullet $ mkCodeList doxPth] $ repeat Nothing

-- References come in the form of [(Project version), [(Display name, Reference for generated code or documents)]]
mkCodeList :: [(String, [(Sentence, Reference)])] -> [(ItemType, Maybe String)]
mkCodeList = map
      (\ r
         -> (Flat
               $ foldlSent_
                   $ S (fst r) : map (uncurry (flip namedRef)) (snd r),
             Nothing))

exampleTitles, exampleDescs :: [String]
-- Sorts the references for mkCodeList.
exampleCodeRefs, exampleDoxRefs :: FilePath -> [[(String, [(Sentence, Reference)])]]
-- example titles
exampleTitles = [dblPendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, sglPendulum, ssp, swhs, template]
-- example descriptions (used in the list of examples)
exampleDescs = [dblPendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc, projectileDesc, sglPendulumDesc, sspDesc, swhsDesc, templateDesc]
exampleCodeRefs path =[
                  [(dblPendulum, [])],
                  [(gamePhys, [])],
                  [(glassBR, map (getCodeRef path $ map toLower glassBR) glassBRCode)],
                  [(hghc, [])],
                  [(noPCM, map (getCodeRef path $map toLower noPCM) noPCMCode)],
                  [(pdController, map (getCodeRef path $ map toLower pdController) pdControllerCode)],
                  [(projectileC1, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC1)) projectileCase1Code),
                  (projectileC2, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC2)) projectileCase2Code),
                  (projectileC3, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC3)) projectileCase3Code),
                  (projectileC4, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC4)) projectileCase4Code),
                  (projectileC5, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC5)) projectileCase5Code)],
                  [(sglPendulum, [])],
                  [(ssp, [])],
                  [(swhs, [])],
                  [(template, [])]]
exampleDoxRefs path =[
                 [(dblPendulum, [])],
                 [(gamePhys, [])],
                 [(glassBR, map (getDoxRef path glassBR) glassBRDox)],
                 [(hghc, [])],
                 [(noPCM, map (getDoxRef path noPCM) noPCMDox)],
                 [(pdController, map (getDoxRef path pdController) pdControllerDox)],
                 [(projectileC1, map (\x -> getDoxRef path projectile (projectileC1 ++ "/" ++ x)) projectileCase1Dox),
                 (projectileC2, map (\x -> getDoxRef path projectile (projectileC2 ++ "/" ++ x)) projectileCase2Dox),
                 (projectileC3, map (\x -> getDoxRef path projectile (projectileC3 ++ "/" ++ x)) projectileCase3Dox),
                 (projectileC4, map (\x -> getDoxRef path projectile (projectileC4 ++ "/" ++ x)) projectileCase4Dox),
                 (projectileC5, map (\x -> getDoxRef path projectile (projectileC5 ++ "/" ++ x)) projectileCase5Dox)],
                 [(sglPendulum, [])],
                 [(ssp, [])],
                 [(swhs, [])],
                 [(template, [])]]

--example names, maybe make a unique type to accept fields of documents, gen code, and doxygen?
sglPendulum, dblPendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, projectileC1,
  projectileC2, projectileC3, projectileC4, projectileC5, ssp, swhs, template :: String

sglPendulum = "SglPendulum"
dblPendulum = "DblPendulum"
gamePhys = "GamePhysics"
glassBR = "GlassBR"
hghc = "HGHC"
noPCM = "NoPCM"
pdController = "PDController"
projectile = "Projectile"
projectileC1 = "Projectile_C_P_NoLB_U_V_D"
projectileC2 = "Projectile_S_L_NoL_U_U_V_F"
projectileC3 = "Projectile_U_P_L_B_B_C_D"
projectileC4 = "Projectile_U_P_NoL_U_WI_V_D"
projectileC5 = "Projectile_U_P_L_B_WI_V_F"
ssp = "SSP"
swhs = "SWHS"
template = "Template"

{--example names, maybe make a unique type to accept fields of documents, gen code, and doxygen?
pendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, ssp, swhs, template :: SystemInformation
projectileC1, projectileC2, projectileC3, projectileC4, projectileC5 :: String
dblPendulum = DblPendulum.fullSI
gamePhys = GamePhysics.fullSI
glassBR = GlassBR.fullSI
hghc = HGHC.fullSI
noPCM = NoPCM.fullSI
pdController = PDController.fullSI
projectile = Projectile.fullSI
projectileC1 = "Projectile_C_P_NoLB_U_V_D"
projectileC2 = "Projectile_S_L_NoL_U_U_V_F"
projectileC3 = "Projectile_U_P_L_B_B_C_D"
projectileC4 = "Projectile_U_P_NoL_U_WI_V_D"
projectileC5 = "Projectile_U_P_L_B_WI_V_F"
sglPendulum = SglPendulum.fullSI
ssp = SSP.fullSI
swhs = SWHS.fullSI
template = Template.fullSI-}

-- list that states what languages the generated code/doxygen docs exist in.
glassBRCode, glassBRDox, noPCMCode, noPCMDox, pdControllerCode, pdControllerDox, projectileCase1Code, projectileCase2Code,
  projectileCase3Code, projectileCase4Code, projectileCase5Code, projectileCase1Dox, projectileCase2Dox,
  projectileCase3Dox, projectileCase4Dox, projectileCase5Dox :: [String]

glassBRCode          = ["cpp", "csharp", "java", "python", "swift"]
glassBRDox           = ["cpp", "csharp", "java", "python"]
noPCMCode            = ["cpp", "csharp", "java", "python"]
noPCMDox             = ["cpp", "csharp", "java", "python"]
pdControllerCode     = ["python"]
pdControllerDox      = []
projectileCase1Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase2Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase3Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase4Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase5Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase1Dox   = ["cpp", "csharp", "java", "python"]
projectileCase2Dox   = ["cpp", "csharp", "java", "python"]
projectileCase3Dox   = ["cpp", "csharp", "java", "python"]
projectileCase4Dox   = ["cpp", "csharp", "java", "python"]
projectileCase5Dox   = ["cpp", "csharp", "java", "python"]
-}
-- Make references for each of the generated SRS files
getHTMLRef, getPDFRef :: FilePath -> String -> Reference
getHTMLRef path ex = Reference ("htmlRef" ++ ex) (URI (getHTMLPath path ex)) (shortname' $ S ("htmlRef" ++ ex))
getPDFRef path ex = Reference ("pdfRef" ++ ex) (URI (getPDFPath path ex)) (shortname' $ S ("pdfRef" ++ ex))
getHTMLPath, getPDFPath :: FilePath -> String -> FilePath
getHTMLPath path ex = path ++ filter (not.isSpace) ex ++ "/srs/" ++ filter (not.isSpace) ex ++ "_SRS.html"
getPDFPath path ex = path ++ filter (not.isSpace) ex ++ "/srs/" ++ filter (not.isSpace) ex ++ "_SRS.pdf"

{-- Make display names and references for generated code and docs
getCodeRef, getDoxRef :: FilePath -> String -> String -> (Sentence, Reference)
getCodeRef path ex lang = (S ("[" ++ convertlang ++ "]"), Reference ("codeRef" ++ ex ++ lang) (URI (getCodePath path ex lang)) (shortname' $ S ("codeRef" ++ ex ++ lang)))
  where
    convertlang
      | lang == "cpp" = "C++"
      | lang == "csharp" = "C Sharp" -- Drasil printers dont like the # symbol, so we use the full word.
      | otherwise = (toUpper.head) lang : tail lang
getDoxRef path ex lang = (S ("[" ++ convertlang lang ++ "]"), Reference ("doxRef" ++ ex ++ lang) (URI (getDoxPath path ex lang)) (shortname' $ S ("doxRef" ++ ex ++ lang)))
  where
    convertlang l
      | l == "cpp" = "C++"
      | l == "csharp" = "C Sharp"
      | "Projectile" `isPrefixOf` l = convertlang $ concat $ tail $ L.splitOn "/" l
      | otherwise = (toUpper.head) l : tail l
-}
getCodePath :: FilePath -> String -> String -> FilePath
getDoxPath :: FilePath -> String -> String -> FilePath
getCodePath path ex lang = path ++ "code/stable/" ++ filter (not.isSpace) ex ++ "/src/" ++ lang -- need repoCommit path
getDoxPath path ex lang = path ++ filter (not.isSpace) ex ++ "/doxygen/" ++ lang ++ "/index.html" -- need example path

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
  concatMap getCodeRefDB (examplesTest p1 p2) ++
  concatMap getDoxRefDB (examplesTest p1 p2) ++ 
  --concatMap (map getCodeRef) (map getAbrv $ examplesTest p1 p2) ++
  --(map getDoxRef $ examplesTest p1 p2) ++
  map (getHTMLRef p2) (map getAbrv $ examplesTest p1 p2) ++ map (getPDFRef p2) (map getAbrv $ examplesTest p1 p2)

getCodeRefDB, getDoxRefDB :: Example -> [Reference]
getCodeRefDB ex = concatMap (\x -> map (getCodeRef ex) $ lang x) $ choicesE ex
getDoxRefDB ex = concatMap (\x -> map (getDoxRef ex) $ lang x) $ choicesE ex

getAbrv :: Example -> String
getAbrv E{sysInfoE = SI{_sys=sys}} = abrv sys

exampleSecRef :: Reference
exampleSecRef = makeSecRef "Examples" $ S "Examples"