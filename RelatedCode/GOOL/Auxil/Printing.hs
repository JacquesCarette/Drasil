module GOOL.Auxil.Printing (
    blank,spc,oneTabbed,oneTab,vertical,verticalComma,verticalNewLine,
    angles,doubleQuoted,doubleQuotedText,
) where

import GOOL.Auxil.DataTypes

import Data.List (intersperse,sort)
import Text.PrettyPrint.HughesPJ

renderFunc :: (a -> Doc) -> a -> String
renderFunc f = render . f

blank :: Doc
blank = text ""

spc :: Doc
spc = text " "

oneTabbed :: [Doc] -> Doc
oneTabbed = vcat . map oneTab

oneTab :: Doc -> Doc
oneTab = nest 4

vertical :: (a -> Doc) -> [a] -> Doc
vertical f = vcat . map f

verticalComma :: (a -> Doc) -> [a] -> Doc
verticalComma f = vcat . punctuate comma . map f

verticalNewLine :: (a -> Doc) -> [a] -> Doc
verticalNewLine f = vcat . punctuate (blank $+$ blank) . map f

angles :: Doc -> Doc
angles d = char '<' <> d <> char '>'

doubleQuotedText :: String -> Doc
doubleQuotedText = doubleQuotes . text

doubleQuoted :: (a -> String) -> a -> Doc
doubleQuoted labeller = doubleQuotedText . labeller

parensLength :: [a] -> Doc
parensLength = parens . int . length

---------- Story-specific:

instance Show Node where
    show = renderFunc showNode

instance Show Event where
    show = renderFunc showEvent

instance Show NodeTransition where
    show = renderFunc showNodeTransition

instance Show Section where
    show = renderFunc showStoryNode

instance Show SectionTransition where
    show = renderFunc showSectionTransition

instance Show Story where
    show = renderFunc showStory

showStory :: Story -> Doc
showStory s = vcat [
    text "Label" <> colon <+> (text $ storyLabel s),
    text "StoryNodes" <+> (parensLength $ storyNodes s) <> colon,
    showStoryNodes $ storyNodes s,
    text "Head" <> colon <+> text "Node" <+> (text $ nodeLabel $ storyHead s),
    text "StoryTransitions" <+> (parensLength $ storyTrans s) <> colon,
    showSectionTransitions $ storyTrans s]

showStoryNode :: Section -> Doc
showStoryNode s = vcat [
    text "Label" <> colon <+> (text $ sectionLabel s),
    text "SectionNodes" <+> (parensLength $ sectionNodes s) <> colon,
    showNodes $ sort $ sectionNodes s,
    text "SectionTransitions" <+> (parensLength $ sectionTrans s) <> colon,
    showNodeTransitions $ sort $ sectionTrans s,
    text "SectionSubsections" <+> (parensLength $ sectionSubsects s) <> colon,
    oneTab $ showSubsections $ sectionSubsects s]

showStoryNodes :: [Section] -> Doc
showStoryNodes = verticalComma showStoryNode

showSubsection :: Subsection -> Doc
showSubsection sub = vcat [
    text "Label" <> colon <+> (text $ subsectionLabel sub),
    text "SubsectionNodes" <+> (parensLength $ subsectionNodes sub) <> colon,
    showNodes $ sort $ subsectionNodes sub,
    text "SubsectionSubsections" <+> (parensLength $ subsectionSubsects sub) <> colon,
    oneTab $ showSubsections $ subsectionSubsects sub]

showSubsections :: [Subsection] -> Doc
showSubsections = verticalComma showSubsection

showNode :: Node -> Doc
showNode node = text "Node" <+> doubleQuoted nodeLabel node

showNodes :: [Node] -> Doc
showNodes = verticalComma showNode

showSectionTransition :: SectionTransition -> Doc
showSectionTransition t = let eventInfo = parens $ text "through" <+> (showEvents $ sectionTransEvents t)
    in text "SectionTrans" <+> doubleQuoted sectionTransLabel t <+> eventInfo

showSectionTransitions :: [SectionTransition] -> Doc
showSectionTransitions = verticalComma showSectionTransition

showNodeTransition :: NodeTransition -> Doc
showNodeTransition t = let eventInfo = parens $ text "through" <+> (showEvents $ nodeTransEvents t)
    in text "NodeTrans" <+> doubleQuoted nodeTransLabel t <+> eventInfo

showNodeTransitions :: [NodeTransition] -> Doc
showNodeTransitions = verticalComma showNodeTransition

showEvent :: Event -> Doc
showEvent e = text "Event" <+> doubleQuoted eventLabel e

showEvents :: [Event] -> Doc
showEvents es = hsep $ intersperse comma (map (\e -> showEvent e) es)
