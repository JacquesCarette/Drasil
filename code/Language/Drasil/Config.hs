-- | Global configuration for output
-- (This should be deprecated in the future as Recipes evolve)
module Language.Drasil.Config where
import Language.Drasil.Output.Formats

data LPMParams = LPMParams DocClass UsePackages ExDoc

-- TeX only - Parameters for rendering literate programmer's manual
lpmTeXParams :: LPMParams
lpmTeXParams = defaultLPMparams

-- | TeX table width in cm
tableWidth :: Double
tableWidth = 10.5

-- | TeX font size
fontSize :: Int
fontSize = 12

-- | Print verbose data definition descriptions?
verboseDDDescription :: Bool
verboseDDDescription = True

-- | TeX Only - Number Data Definition equations?
numberedDDEquations :: Bool
numberedDDEquations = False -- Does not affect HTML

-- | TeX Only - Number Theoretical Model equations?
numberedTMEquations :: Bool
numberedTMEquations = False

-- | TeX Only - Numbered sections?
numberedSections :: Bool
numberedSections = True

--TeX Document Parameter Defaults (can be modified to affect all documents OR
  -- you can create your own parameter function and replace the one above.
--defaultSRSparams :: SRSParams
--defaultSRSparams = SRSParams
--  (DocClass  Nothing "article")
--  (UsePackages ["fullpage","booktabs","longtable","listings","graphics","hyperref","caption",
--  "amsmath"])

-- | default parameters for a generated literate programmer's manual 
defaultLPMparams :: LPMParams
defaultLPMparams = LPMParams
  (DocClass (Just "article") "cweb-hy")
  (UsePackages ["xr"])
  (ExDoc (Just "L-") "hghc_SRS")
  
-- | TeX Only - column width for data definitions 
-- (fraction of LaTeX textwidth)
colAwidth, colBwidth :: Double
colAwidth = 0.2
colBwidth = 0.73

-- | TeX Only - Settings for hyperref
hyperSettings :: String
hyperSettings =
     "bookmarks=true,"     -- show bookmarks bar?
  ++ "colorlinks=true,"    -- false: boxed links; true: colored links
  ++ "linkcolor=red,"      -- color of internal links
  ++ "citecolor=blue,"     -- color of links to bibliography
  ++ "filecolor=magenta,"  -- color of file links
  ++ "urlcolor=cyan"       -- color of external links

-- | Split up generated code into source files based on modules
splitSource :: Bool
splitSource = True

-- | The bibliography format
data StyleGuide = MLA | APA | Chicago

useStyleTeX :: StyleGuide -> String
useStyleTeX MLA = "ieeetr"
useStyleTeX APA = "apalike"
useStyleTeX Chicago = "plain"

bibStyle :: String
bibStyle = useStyleTeX MLA --This will be an input for the user eventually

-- | Used to name the BibTeX file
bibFname :: String
bibFname = "bibfile" --needed in TeX/Print and TeX/Preamble