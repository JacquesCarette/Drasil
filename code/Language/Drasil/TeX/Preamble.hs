module Language.Drasil.TeX.Preamble(genPreamble) where

import Data.List (nub)

import Language.Drasil.Config (hyperSettings, fontSize,bibFname)
import Language.Drasil.TeX.Monad
import Language.Drasil.TeX.AST
import Language.Drasil.TeX.Helpers

-- FIXME: this really shouldn't be in code, it should be data!
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
             | AMSsymb --displays bold math sets (reals, naturals, etc.)
             | Breqn --line breaks long equations automaticly
             | FileContents --creates .bib file within .tex file
             | BibLaTeX
             | Tabu --adds auto column width feature for tables 
             | Mathtools --line breaks for long fractions and cases
             | URL
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
addPackage Tabu      = usepackage "tabu"
addPackage Mathtools = usepackage "mathtools"
addPackage URL       = usepackage "url"

data Def = AssumpCounter
         | LCCounter
         -- | ModCounter
         | ReqCounter
         | UCCounter
         | Bibliography
         | TabuLine
         deriving Eq

addDef :: Def -> D
addDef AssumpCounter = count "assumpnum" %%
                       comm "atheassumpnum" "A\\theassumpnum" Nothing
addDef LCCounter     = count "lcnum" %%
                       comm "lcthelcnum" "LC\\thelcnum" Nothing
-- addDef ModCounter    = count "modnum" %%
                       -- comm "mthemodnum" "M\\themodnum" Nothing
addDef ReqCounter    = count "reqnum" %%
                       comm "rthereqnum" "R\\thereqnum" Nothing
addDef UCCounter     = count "ucnum" %%
                       comm "uctheucnum" "UC\\theucnum" Nothing
addDef Bibliography  = command "bibliography" bibFname
addDef TabuLine      = command0 "global\\tabulinesep=1mm"

genPreamble :: [LayoutObj] -> D
genPreamble los = let (pkgs, defs) = parseDoc los
  in docclass (Just $ (show fontSize) ++ "pt") "article" %%
     (vcat $ map addPackage pkgs) %% (vcat $ map addDef defs)

parseDoc :: [LayoutObj] -> ([Package], [Def])
parseDoc los' = ([FullPage, HyperRef, AMSMath, AMSsymb, Mathtools, Breqn] ++ 
   (nub $ concat $ map fst res), nub $ concat $ map snd res)
  where 
    res = map parseDoc' los'
    parseDoc' :: LayoutObj -> ([Package], [Def])
    parseDoc' (Table _ _ _ _ _) = ([Tabu,LongTable,BookTabs,Caption], [TabuLine])
    parseDoc' (HDiv _ slos _) = 
      let res1 = map parseDoc' slos in
      let pp = concat $ map fst res1 in
      let dd = concat $ map snd res1 in
      (pp, dd)
    parseDoc' (Definition _ ps _) =
      let res1 = concat $ map (map parseDoc' . snd) ps in
      let pp = concat $ map fst res1 in
      let dd = concat $ map snd res1 in
      (Tabu:LongTable:BookTabs:pp,TabuLine:dd)
    parseDoc' (Figure _ _ _ _) = ([Graphics,Caption],[])
    parseDoc' (ALUR Requirement _ _) = ([], [ReqCounter])
    parseDoc' (ALUR Assumption _ _) = ([], [AssumpCounter])
    parseDoc' (ALUR LikelyChange _ _) = ([], [LCCounter])
    parseDoc' (ALUR UnlikelyChange _ _) = ([], [UCCounter])
    parseDoc' (Graph _ _ _ _ _) = ([Caption,Tikz,Dot2Tex,AdjustBox],[])
    parseDoc' (Bib _) = ([FileContents,BibLaTeX,URL],[Bibliography])
    parseDoc' (Header _ _ _) = ([], [])
    parseDoc' (Paragraph _)  = ([], [])
    parseDoc' (List _)       = ([], [])
    parseDoc' (EqnBlock _)   = ([], [])
