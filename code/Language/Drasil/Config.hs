module Language.Drasil.Config where
import Language.Drasil.Output.Formats (DocParams(..))
import Language.Drasil.CCode.AST (Lang(..)) -- this seems wrong

outLang :: Lang
outLang = CLang

expandSymbols :: Bool
expandSymbols = True

srsTeXParams,lpmTeXParams :: [DocParams]
srsTeXParams = defaultSRSparams

lpmTeXParams = defaultLPMparams

tableWidth :: Double --in cm
tableWidth = 10.5

verboseDDDescription :: Bool
verboseDDDescription = True

numberedDDEquations :: Bool
numberedDDEquations = False --Does not affect HTML

numberedTMEquations :: Bool
numberedTMEquations = False -- TeX only

numberedSections :: Bool -- TeX only
numberedSections = True

--TeX Document Parameter Defaults (can be modified to affect all documents OR
  -- you can create your own parameter function and replace the one above.
defaultSRSparams :: [DocParams]
defaultSRSparams = [
  DocClass  [] "article",
  UsePackages ["booktabs","longtable","listings","graphics","hyperref","caption",
  "amsmath"]
  ]

defaultLPMparams :: [DocParams]
defaultLPMparams = [
  DocClass "article" "cweb-hy",
  UsePackages ["xr"],
  ExDoc "L-" "hghc_SRS"
  ]
  
--column width for data definitions (fraction of LaTeX textwidth)
colAwidth, colBwidth :: Double
colAwidth = 0.2
colBwidth = 0.73
