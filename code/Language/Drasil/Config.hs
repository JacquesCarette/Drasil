module Language.Drasil.Config where
import Language.Drasil.Output.Formats
import Language.Drasil.CCode.AST (Lang(..)) -- this seems wrong

outLang :: Lang
outLang = CLang

expandSymbols :: Bool
expandSymbols = True

--data SRSParams = SRSParams DocClass UsePackages
data LPMParams = LPMParams DocClass UsePackages ExDoc

--srsTeXParams :: SRSParams
--srsTeXParams = defaultSRSparams

lpmTeXParams :: LPMParams
lpmTeXParams = defaultLPMparams

tableWidth :: Double -- in cm
tableWidth = 10.5

fontSize :: Int
fontSize = 12

verboseDDDescription :: Bool
verboseDDDescription = True

numberedDDEquations :: Bool
numberedDDEquations = False -- Does not affect HTML

numberedTMEquations :: Bool
numberedTMEquations = False -- TeX only

numberedSections :: Bool -- TeX only
numberedSections = True

--TeX Document Parameter Defaults (can be modified to affect all documents OR
  -- you can create your own parameter function and replace the one above.
--defaultSRSparams :: SRSParams
--defaultSRSparams = SRSParams
--  (DocClass  Nothing "article")
--  (UsePackages ["fullpage","booktabs","longtable","listings","graphics","hyperref","caption",
--  "amsmath"])

defaultLPMparams :: LPMParams
defaultLPMparams = LPMParams
  (DocClass (Just "article") "cweb-hy")
  (UsePackages ["xr"])
  (ExDoc (Just "L-") "hghc_SRS")
  
--column width for data definitions (fraction of LaTeX textwidth)
colAwidth, colBwidth :: Double
colAwidth = 0.2
colBwidth = 0.73

--settings for hyperref
hyperSettings :: String
hyperSettings =
     "bookmarks=true,"     -- show bookmarks bar?
  ++ "colorlinks=true,"    -- false: boxed links; true: colored links
  ++ "linkcolor=red,"      -- color of internal links
  ++ "citecolor=blue,"     -- color of links to bibliography
  ++ "filecolor=magenta,"  -- color of file links
  ++ "urlcolor=cyan"       -- color of external links
