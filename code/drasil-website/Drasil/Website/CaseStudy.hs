module Drasil.Website.CaseStudy (caseStudySec, caseStudyRefs) where

import Language.Drasil hiding (C)


-----------------------------
-- Case Studies Section
-----------------------------

caseStudySec :: Section
caseStudySec = section (S caseStudiesTitle) [mkParagraph $ S caseStudiesDesc, mkFig caseStudyTabRef mkCaseTable, UlC $ ulcc caseStudyLegend] [] caseStudySecRef

caseStudyRefs :: [Reference]
caseStudyRefs = [caseStudySecRef, ref caseStudySec, ref caseStudyTabRef]

caseStudySecRef :: Reference
caseStudySecRef = makeSecRef "CaseStudy" $ S "Case Study"

caseStudiesTitle, caseStudiesDesc :: String
caseStudiesTitle = "Case Studies"
caseStudiesDesc = "Drasil allows some design decisions to be made by the user when generating \
  \code. The table below summarizes the design decisions made for each case \
  \study, followed by a guide giving the meaning of the short-forms used in the \
  \table:"

-- case studies table
mkCaseTable :: RawContent
mkCaseTable = Table headerRow tableBody EmptyS False

headerRow :: [Sentence]
headerRow = map S ["Case Study", modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle, conRepTitle, realNumRepTitle]

tableBody :: [[Sentence]]
tableBody = mkTable [\(CS x _ _ _ _ _ _ _) -> S x,
                     \(CS _ x _ _ _ _ _ _) -> S $ show x,
                     \(CS _ _ x _ _ _ _ _) -> S $ show x,
                     \(CS _ _ _ x _ _ _ _) -> S $ show x,
                     \(CS _ _ _ _ x _ _ _) -> S $ show x,
                     \(CS _ _ _ _ _ x _ _) -> S $ show x,
                     \(CS _ _ _ _ _ _ x _) -> S $ show x,
                     \(CS _ _ _ _ _ _ _ x) -> S $ show x]
                     [glassBRCase, noPCMCase, pdControllerCase, projectileCase1, projectileCase2, projectileCase3, projectileCase4, projectileCase5]

caseStudyTabRef :: Reference
caseStudyTabRef = makeTabRef "CaseStudy"

type Name = String
-- Modularity of the code
data Modularity = CMod | SMod | UMod
data ImplementType = LIm | PIm
data Logging = L | NoL deriving (Show)
-- Input Structure
data InStruct = BIn | UIn
--Constant structure
data ConStruct = B | I | U | WI deriving (Show)
-- Constant number representation
data ConRep = C | V deriving (Show)
-- Real number representation
data RealNumRep = D | F deriving (Show)

instance Show Modularity where
  show CMod = "C"
  show SMod = "S"
  show UMod = "U"

instance Show ImplementType where
  show LIm = "L"
  show PIm = "P"

instance Show InStruct where
  show BIn = "B"
  show UIn = "U"

-- Holds all required information for an entry on the case study table
data CaseStudy = CS Name Modularity ImplementType Logging InStruct ConStruct ConRep RealNumRep

-- Entries in the table for each case study
glassBRCase, noPCMCase, pdControllerCase, projectileCase1, projectileCase2,
  projectileCase3, projectileCase4, projectileCase5 :: CaseStudy

glassBRCase      = CS "GlassBR"                     SMod PIm L   BIn I  C D
noPCMCase        = CS "NoPCM"                       CMod PIm NoL UIn B  C D
pdControllerCase = CS "PDController"                CMod PIm NoL UIn B  C D
projectileCase1  = CS "Projectile_C_P_NoL_B_U_V_D"  CMod PIm NoL BIn U  V D
projectileCase2  = CS "Projectile_S_L_NoL_U_U_V_F"  SMod LIm NoL UIn U  V F
projectileCase3  = CS "Projectile_U_P_L_B_B_C_D"    UMod PIm L   BIn B  C D
projectileCase4  = CS "Projectile_U_P_NoL_U_WI_V_D" UMod PIm NoL UIn WI V D
projectileCase5  = CS "Projectile_U_P_L_B_WI_V_F"   UMod PIm L   BIn WI V F

-- case studies symbol legend
modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle,
  conRepTitle, realNumRepTitle :: String
modPt1, modPt2, modPt3, implementPt1, implementPt2,
  logPt1, logPt2, inStructPt1, inStructPt2, conStructPt1, conStructPt2, conStructPt3,
  conStructPt4, conRepPt1, conRepPt2, realNumRepPt1, realNumRepPt2 :: String
modSymbPt1, modSymbPt2, modSymbPt3, implementSymbPt1, implementSymbPt2,
  logSymbPt1, logSymbPt2, inStructSymbPt1, inStructSymbPt2, conStructSymbPt1, conStructSymbPt2, conStructSymbPt3,
  conStructSymbPt4, conRepSymbPt1, conRepSymbPt2, realNumRepSymbPt1, realNumRepSymbPt2 :: String

caseStudyLegend :: RawContent
caseStudyLegend = Enumeration caseStudyList

caseStudyList :: ListType
caseStudyList = Bullet $ zip (zipWith3 mkLegendListFunc legendTitles legendSymbs legendConts) $ repeat Nothing

mkLegendListFunc :: Sentence -> [Sentence] -> [Sentence] -> ItemType
mkLegendListFunc t symbs conts = Nested t $ Bullet $ zip (zipWith mkTandDSent symbs conts) $ repeat Nothing

mkTandDSent :: Sentence -> Sentence -> ItemType
mkTandDSent s def = Flat $ s +:+ S "-" +:+ def

legendTitles :: [Sentence]
legendTitles = map S [modularityTitle, implementTypeTitle, loggingTitle, inStructTitle, conStructTitle, conRepTitle, realNumRepTitle]

legendSymbs :: [[Sentence]]
legendSymbs = map (map S) [[modSymbPt1, modSymbPt2, modSymbPt3],
                           [implementSymbPt1, implementSymbPt2],
                           [logSymbPt1, logSymbPt2],
                           [inStructSymbPt1, inStructSymbPt2],
                           [conStructSymbPt1, conStructSymbPt2, conStructSymbPt3, conStructSymbPt4],
                           [conRepSymbPt1, conRepSymbPt2],
                           [realNumRepSymbPt1, realNumRepSymbPt2]]

legendConts :: [[Sentence]]
legendConts = map (map S) [[modPt1, modPt2, modPt3],
                           [implementPt1, implementPt2],
                           [logPt1, logPt2],
                           [inStructPt1, inStructPt2],
                           [conStructPt1, conStructPt2, conStructPt3, conStructPt4],
                           [conRepPt1, conRepPt2],
                           [realNumRepPt1, realNumRepPt2]]

modularityTitle = "Modularity"
implementTypeTitle = "Implementation Type"
loggingTitle = "Logging"
inStructTitle = "Input Structure"
conStructTitle = "Constant Structure"
conRepTitle = "Constant Representation"
realNumRepTitle = "Real Number Representation"

modPt1 = "Unmodular"
modPt2 = "Modular with Combined input module"
modPt3 = "Modular with Separated input module"
modSymbPt1 = "U"
modSymbPt2 = "C"
modSymbPt3 = "S"

implementPt1 = "Program"
implementPt2 = "Library"
implementSymbPt1 = "P"
implementSymbPt2 = "L"

logPt1 = "No Logging statements"
logPt2 = "Logging statements included"
logSymbPt1 = "NoL"
logSymbPt2 = "L"

inStructPt1 = "Inputs are Bundled in a class"
inStructPt2 = "Inputs are Unbundled"
inStructSymbPt1 = "B"
inStructSymbPt2 = "U"

conStructPt1 = "Constant values are Inlined"
conStructPt2 = "Constants are stored With the Inputs"
conStructPt3 = "Constants are stored in variables that are Bundled in a class"
conStructPt4 = "Constants are stored in variables that are Unbundled"
conStructSymbPt1 = "I"
conStructSymbPt2 = "WI"
conStructSymbPt3 = "B"
conStructSymbPt4 = "U"

conRepPt1 = "Constants are stored as Variables"
conRepPt2 = "Constants are stored as Constants"
conRepSymbPt1 = "V"
conRepSymbPt2 = "C"

realNumRepPt1 = "Real numbers are represented as Doubles"
realNumRepPt2 = "Real numbers are represented as Floats"
realNumRepSymbPt1 = "D"
realNumRepSymbPt2 = "F"
