{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.MD.Render (
  renderMarkdown, MDRenderOptions(..), MDFlavour(..), TableStyle(..)
) where

import Drasil.Data.Formats.MD.Core (Markdown(..), ListType(..))
import Prettyprinter (
  vsep, brackets, parens, (<+>), pretty, line, punctuate, pipe, hcat, fill, vcat
  )
import qualified Prettyprinter as PNew (Doc)
import Data.List (transpose)
import Data.Text (Text, pack, singleton)
import qualified Data.Text as T (concatMap)

-- | Options for rendering 'Markdown'.
data MDRenderOptions = MDRO {
    mdFlavour :: MDFlavour,
    tableStyle :: TableStyle,
    formatChar :: Char
}

-- | Markdown Flavour: Markdown extension/variant
data MDFlavour = Pandoc
  deriving (Eq, Show)

data TableStyle = Pretty | Minified
  deriving (Eq, Show)

-- | Render 'Markdown' to a 'Doc'
renderMarkdown :: MDRenderOptions -> [Markdown] -> PNew.Doc ann
renderMarkdown rOpt element = vsep (fmap (renderMDElem rOpt) element)

-- | Render a single 'Markdown' element
renderMDElem :: MDRenderOptions -> Markdown -> PNew.Doc ann
renderMDElem rOpt (Heading n idOpt ch)
  | n < 1 || n > 6 = error "Illegal header (header weight must be between 1 and 6)."
  | otherwise      = pretty (replicate n '#') <+> hcat (fmap (renderMDElem rOpt) ch)
    <> maybeId idOpt rOpt
renderMDElem rOpt (Div idOpt ch) =
  case mdFlavour rOpt of
    Pandoc -> vcat $ [":::" <> maybeId (Just idOpt) rOpt]
      <> fmap (renderMDElem rOpt) ch <> [":::"]
renderMDElem _ (Code langOpt code) = "```" <> foldMap pretty langOpt
  <> line <> pretty code <> line <> "```"
renderMDElem rOpt (Quote ch) = hcat (fmap (\c -> "> " <> renderMDElem rOpt c) ch)
renderMDElem rOpt (Link url ch) = renderRef rOpt ch url
renderMDElem rOpt (Image src ch idOpt) =
  "!" <> renderRef rOpt ch src <> maybeId idOpt rOpt
renderMDElem rOpt (List tp items) = vsep (zipWith (renderListItem rOpt tp) [1..] items) <> line
renderMDElem rOpt (Table headerRows dataRows captionOpt idOpt) =
  line <> vsep (renderedHeaders <> [separatorRow rOpt colWidths] <> renderedData)
  <> renderCaption captionOpt idOpt rOpt
  where
    colWidths = fmap (max 3 . maximum) . transpose $ fmap (fmap cellWidth) (headerRows <> dataRows)
    cellWidth c = length (show (renderMDElem rOpt c))
    renderedHeaders = fmap (renderRow rOpt colWidths) headerRows
    renderedData    = fmap (renderRow rOpt colWidths) dataRows
renderMDElem rOpt (Paragraph ch) = line <> hcat (fmap (renderMDElem rOpt) ch) <> line
renderMDElem rOpt (Bold ch) = pretty (replicate 2 (formatChar rOpt)) <>
  hcat (fmap (renderMDElem rOpt) ch) <> pretty (replicate 2 (formatChar rOpt))
renderMDElem rOpt (Italic ch) =
  pretty (formatChar rOpt) <> hcat (fmap (renderMDElem rOpt) ch) <> pretty (formatChar rOpt)
renderMDElem _ (RawText t) = pretty (escapeMDText t)
renderMDElem _ Line = "----"

renderListItem :: MDRenderOptions -> ListType -> Int -> [Markdown] -> PNew.Doc ann
renderListItem opt Unordered _ itemContent =
  "- " <> hcat (fmap (renderMDElem opt) itemContent)
renderListItem opt Ordered index itemContent =
  pretty index <> ". " <> hcat (fmap (renderMDElem opt) itemContent)

renderRef :: MDRenderOptions -> [Markdown] -> Text -> PNew.Doc ann
renderRef rOpt ch ref = brackets (hcat (fmap (renderMDElem rOpt) ch)) <> parens (pretty ref)

-- | Internal: Add ID if Pandoc-flavoured
maybeId :: Maybe Text -> MDRenderOptions -> PNew.Doc ann
maybeId idOpt opt =
  if mdFlavour opt == Pandoc
    then foldMap (\idStr -> " {#" <> pretty idStr <> "}") idOpt
    else mempty

renderRow :: MDRenderOptions -> [Int] -> [Markdown] -> PNew.Doc ann
renderRow rOpt widths row =
  pipe <> hcat (punctuate pipe (zipWith renderCell widths row)) <> pipe
  where
    renderCell w cell =
      case tableStyle rOpt of
        Pretty -> " " <> fill w (renderMDElem rOpt cell) <> " "
        Minified -> renderMDElem rOpt cell

separatorRow :: MDRenderOptions -> [Int] -> PNew.Doc ann
separatorRow rOpt widths =
  pipe <> hcat (punctuate pipe separator) <> pipe
  where
    separator =
      case tableStyle rOpt of
        Pretty -> fmap (\w -> ":" <> pretty (replicate (w + 1) '-')) widths
        Minified -> replicate (length widths) ":---"

renderCaption :: Maybe [Markdown] -> Maybe Text -> MDRenderOptions -> PNew.Doc ann
renderCaption Nothing Nothing _ = mempty
renderCaption capOpt idOpt opt =
  line <> line <> ":" <> renderedCap <> maybeId idOpt opt
  where
    renderedCap = case capOpt of
      Just ch -> " " <> hcat (fmap (renderMDElem opt) ch)
      Nothing -> mempty

-- | Internal: Escape characters
escapeMDText :: Text -> Text
escapeMDText = T.concatMap escapeChar
  where
    escapeChar :: Char -> Text
    escapeChar c
      | c `elem` ("\\`*_{}[]()#+!<>" :: [Char]) = pack ['\\', c]
      | otherwise = singleton c
