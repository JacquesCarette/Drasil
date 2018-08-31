-- | Global configuration for output
-- (This should be deprecated in the future as Recipes evolve)
module Language.Drasil.Config(numberedSections,hyperSettings,fontSize,bibFname,
  verboseDDDescription,StyleGuide(..),bibStyleH,bibStyleT,colAwidth,colBwidth,
  numberedDDEquations,numberedTMEquations) where

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

-- | The bibliography format
data StyleGuide = MLA | APA | Chicago

useStyleTeX :: StyleGuide -> String
useStyleTeX MLA = "ieeetr"
useStyleTeX APA = "apalike"
useStyleTeX Chicago = "plain"

bibStyleT :: String
bibStyleT = useStyleTeX bibStyle --This will be an input for the user eventually

bibStyleH :: StyleGuide
bibStyleH = bibStyle

bibStyle :: StyleGuide
bibStyle = MLA

-- | Used to name the BibTeX file
bibFname :: String
bibFname = "bibfile" --needed in TeX/Print and TeX/Preamble
