-- | Contains logic to create a DOT file (for producing a visual graph) from a Story. This module is story-specific.
module GOOL.Auxil.DotOutput (
    dotFileFromStory
) where

import GOOL.Auxil.DataTypes
import GOOL.Auxil.Helper (makeVarNameValid,vibmap)
import GOOL.Auxil.Printing (blank,doubleQuoted,doubleQuotedText,oneTabbed,vertical,verticalNewLine)

import Data.List ((\\),intersperse)
import System.IO
import Text.PrettyPrint.HughesPJ

labelText :: Doc
labelText = text "label ="

arrow :: Doc
arrow = text "->"

-- | Creates an ASCII text file with .dot extension. The file describes the Story in the DOT 
--   language, which when ran through a DOT layout engine (dot, neato, twopi) produces a 
--   visual graph of the Story's flow
dotFileFromStory :: Story -> IO ()
dotFileFromStory s = do
    h <- openFile (makeVarNameValid (storyLabel s) ++ ".dot") WriteMode
    hPutStrLn h (render $ dotTextFromStory s)
    hClose h

dotTextFromStory :: Story -> Doc
dotTextFromStory s = vcat [
    text "digraph" <+> doubleQuoted storyLabel s,
    lbrace,
    oneTabbed [
        labelText <+> doubleQuoted storyLabel s,
        blank,
        dotTextFromSections $ storyNodes s,
        blank,
        dotTextFromStoryTransitions $ storyTrans s],
    rbrace]

dotTextFromSection :: Section -> Doc
dotTextFromSection s = vcat [
    text "subgraph" <+> (doubleQuotedText $ "cluster " ++ sectionLabel s),
    lbrace,
    oneTabbed [
        labelText <+> doubleQuoted sectionLabel s,
        blank,
        dotTextFromSubsections (sectionSubsects s) (sectionTrans s)],
    rbrace]

dotTextFromSubsections :: [Subsection] -> [NodeTransition] -> Doc
dotTextFromSubsections subs ts = vcat [
    dotTextFromSectionTransitions otherTrans,
    blank,
    subsectTransDoc]
    where subTrans s = filter (`inSubsection` s) ts
          otherTrans = ts \\ concatMap subTrans subs
          subsectTransDoc = vibmap (\sub -> dotTextFromSubsection sub (subTrans sub)) subs

dotTextFromSubsection :: Subsection -> [NodeTransition] -> Doc
dotTextFromSubsection s ts = vcat [
    text "subgraph" <+> (doubleQuotedText $ "cluster " ++ subsectionLabel s),
    lbrace,
    oneTabbed [
        labelText <+> doubleQuoted subsectionLabel s,
        blank,
        dotTextFromSectionTransitions currentDepthTrans,
        dotTextFromSubsections (subsectionSubsects s) nestedTrans],
    rbrace]
    where currentDepthTrans = ts \\ concatMap subTrans subs
          nestedTrans = ts \\ currentDepthTrans
          subs = subsectionSubsects s
          subTrans s = filter (`inSubsection` s) ts

dotTextFromSections :: [Section] -> Doc
dotTextFromSections = verticalNewLine dotTextFromSection

dotTextFromStoryTransition :: SectionTransition -> Doc
dotTextFromStoryTransition t = 
    let condPrefix = concat $ intersperse " AND " $ map eventLabel $ sectionTransEvents t
        preL = doubleQuoted nodeLabel $ sectionTransPreNode t
        postL = doubleQuoted nodeLabel $ sectionTransPostNode t
        eventL = doubleQuotedText condPrefix
    in preL <+> arrow <+> postL <+> (brackets $ labelText <+> eventL)

dotTextFromStoryTransitions :: [SectionTransition] -> Doc
dotTextFromStoryTransitions = vertical dotTextFromStoryTransition

dotTextFromSectionTransition :: NodeTransition -> Doc
dotTextFromSectionTransition t = 
    let condPrefix = concat $ intersperse " AND " $ map eventLabel $ nodeTransEvents t
        preL = doubleQuoted nodeLabel $ nodeTransPreNode t
        postL = doubleQuoted nodeLabel $ nodeTransPostNode t
        eventL = doubleQuotedText condPrefix
    in preL <+> arrow <+> postL <+> (brackets $ labelText <+> eventL)

dotTextFromSectionTransitions :: [NodeTransition] -> Doc
dotTextFromSectionTransitions = vertical dotTextFromSectionTransition

-- | Helper function: checks whether both nodes of a given transition are within a subsection
inSubsection :: NodeTransition -> Subsection -> Bool
inSubsection t sub =
    inSubSubsects
    || (preNodeIsElemOf currentDepthNodes && postNodeIsElemOf currentDepthNodes)
    || (preNodeIsElemOf currentDepthNodes && postNodeIsElemOf (nestedNodes sub))
    || (preNodeIsElemOf (nestedNodes sub) && postNodeIsElemOf currentDepthNodes)
    where currentDepthNodes = subsectionNodes sub
          nestedNodes s = concat $ (map subsectionNodes $ subsectionSubsects s) ++ (map nestedNodes $ subsectionSubsects s)
          inSubSubsects = or $ map (inSubsection t) $ subsectionSubsects sub
          preNodeIsElemOf = elem $ nodeTransPreNode t
          postNodeIsElemOf = elem $ nodeTransPostNode t
