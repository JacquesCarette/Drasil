module Language.Drasil.TeX.Preamble(genPreamble) where

import Data.List (nub)

import Language.Drasil.Config (hyperSettings, fontSize,bibFname)
import Language.Drasil.TeX.Monad
import Language.Drasil.TeX.AST
import Language.Drasil.TeX.Helpers


data Preamble = PreP Package
              | PreD Def
              deriving Eq

data Package = AMSMath
             | BookTabs
             | Caption
             | FullPage
             | Graphics
             | HyperRef
             | Listings
             | LongTable
             | Tikz
             | Dot2Tex
             | AdjustBox
             | AMSsymb
             | Breqn
             | FileContents
             | BibLaTeX
             deriving Eq

addPackage :: Package -> D
addPackage AMSMath   = usepackage "amsmath"
addPackage BookTabs  = usepackage "booktabs"
addPackage Caption   = usepackage "caption"
addPackage FullPage  = usepackage "fullpage"
addPackage Graphics  = usepackage "graphics"
addPackage HyperRef  = usepackage "hyperref" %%
                       command "hypersetup" hyperSettings
addPackage Listings  = usepackage "listings"
addPackage LongTable = usepackage "longtable"
addPackage Tikz      = usepackage "tikz" %%
                       command "usetikzlibrary" "arrows.meta, shapes"
addPackage Dot2Tex   = usepackage "dot2texi"
addPackage AdjustBox = usepackage "adjustbox"
addPackage AMSsymb   = usepackage "amssymb"
addPackage Breqn     = usepackage "breqn"
addPackage FileContents = usepackage "filecontents"
addPackage BibLaTeX  = command1o "usepackage" (Just "backend=bibtex") "biblatex"

data Def = AssumpCounter
         | LCCounter
         | ModCounter
         | ReqCounter
         | UCCounter
         | Bibliography
         | UseEps
         deriving Eq

addDef :: Def -> D
addDef AssumpCounter = count "assumpnum" %%
                       comm "atheassumpnum" "A\\theassumpnum" Nothing
addDef LCCounter     = count "lcnum" %%
                       comm "lcthelcnum" "LC\\thelcnum" Nothing
addDef ModCounter    = count "modnum" %%
                       comm "mthemodnum" "M\\themodnum" Nothing
addDef ReqCounter    = count "reqnum" %%
                       comm "rthereqnum" "R\\thereqnum" Nothing
addDef UCCounter     = count "ucnum" %%
                       comm "uctheucnum" "UC\\theucnum" Nothing
addDef Bibliography  = command "bibliography" bibFname 
addDef UseEps        = comm "useEps" "\\ensuremath{\\epsilon{}}" Nothing

genPreamble :: [LayoutObj] -> D
genPreamble los = let preamble = parseDoc los
  in docclass (Just $ (show fontSize) ++ "pt") "article" %%
     (vcat $ listpackages preamble) %% (vcat $ listdefs preamble)
  where listpackages :: [Preamble] -> [D]
        listpackages []            = []
        listpackages ((PreP p):ps) = addPackage p:listpackages ps
        listpackages (_:ps)        = listpackages ps
        listdefs :: [Preamble] -> [D]
        listdefs []            = []
        listdefs ((PreD d):ds) = addDef d:listdefs ds
        listdefs (_:ds)        = listdefs ds

parseDoc :: [LayoutObj] -> [Preamble]
parseDoc los' = [PreP FullPage, PreP HyperRef, PreP AMSMath, PreP AMSsymb,
  PreP Breqn, PreP FileContents, PreP BibLaTeX, PreD Bibliography, PreD UseEps] ++
  (nub $ parseDoc' los')
  where parseDoc' [] = []
        parseDoc' ((Table _ _ _ _):los) =
          (PreP LongTable):(PreP BookTabs):(PreP Caption):parseDoc' los
        parseDoc' ((Section _ _ slos _):los) =
          (parseDoc' slos ++ parseDoc' los)
   --     parseDoc' ((CodeBlock _):los) =
   --       (PreP Listings):parseDoc' los
        parseDoc' ((Definition ps _):los) =
          (concat $ map parseDoc' (map snd ps)) ++
          (PreP LongTable):(PreP BookTabs):parseDoc' los
        parseDoc' ((Figure _ _ _):los) =
          (PreP Graphics):(PreP Caption):parseDoc' los
        parseDoc' ((Module _ _):los) =
          (PreD ModCounter):parseDoc' los
        parseDoc' ((Requirement _ _):los) =
          (PreD ReqCounter):parseDoc' los
        parseDoc' ((Assumption _ _):los) =
          (PreD AssumpCounter):parseDoc' los
        parseDoc' ((LikelyChange _ _):los) =
          (PreD LCCounter):parseDoc' los
        parseDoc' ((UnlikelyChange _ _):los) =
          (PreD UCCounter):parseDoc' los
        parseDoc' ((Graph _ _ _ _ _):los) =
          (PreP Caption):(PreP Tikz):(PreP Dot2Tex):(PreP AdjustBox):
          parseDoc' los  {-FIXME: add the two packages bellow if there is a bibliography-}
        -- parseDoc' ((Bibliography _):los) = (PreP FileContents):(PreP Cite):
          -- parseDoc' los
        parseDoc' (_:los) =
          parseDoc' los
