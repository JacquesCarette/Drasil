{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.HTML.Render
  ( renderHTML,
    renderHTMLFragment,
    escapeHTMLText,
    HTMLRenderOptions (..),
    HTMLLayout (..),
    defaultHTMLRO,
  )
where

import Control.Monad.Trans.Reader (Reader, asks, runReader)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Drasil.Data.Formats.HTML.Core
  ( Attr (..),
    Cell (..),
    CustomTag (..),
    DItem (..),
    Format (..),
    HeadingLevel (..),
    HTML (..),
    HTMLBody (..),
    HTMLHead (..),
    LItem (..),
    ListType (..),
    Row (..),
    TagType (..),
  )
import Prettyprinter
  ( Doc,
    angles,
    dquotes,
    equals,
    hcat,
    hsep,
    indent,
    pretty,
    space,
    vcat,
  )

-- | Layout within HTML elements and body fragments.
data HTMLLayout
  = VerticalLayout -- ^ Preserve the usual per-element layout.
  | InlineLayout -- ^ Add no line breaks or indentation; preserve content whitespace.

data HTMLRenderOptions = HTMLRO
  { -- | What 'TagType' is each 'CustomTag'?
    customElementTagTypes :: M.Map CustomTag TagType,
    -- | The number of spaces to use for each level of indentation.
    indentationSize :: Int,
    -- | Layout inherited by nested elements and fragment siblings.
    htmlLayout :: HTMLLayout
  }

-- | Default 'HTMLRenderOptions' with standard indentation and no custom tags.
defaultHTMLRO :: HTMLRenderOptions
defaultHTMLRO = HTMLRO M.empty 2 VerticalLayout

-- | Render 'HTML' to a 'Doc'
renderHTML :: HTMLRenderOptions -> HTML -> Doc ann
renderHTML opt (HTML heads bodies) = flip runReader opt $ do
  contents <- sequence [renderHeadSec heads, renderBodySec bodies]
  html <- wrapBlock "html" [] contents
  pure $ vcat ["<!DOCTYPE html>", html]

-- | Render body nodes without the doctype, @html@, @head@ or @body@ wrappers.
-- Adjacent text nodes are normalized using the full-document renderer's rules.
renderHTMLFragment :: HTMLRenderOptions -> [HTMLBody] -> Doc ann
renderHTMLFragment opt = flip runReader opt . renderFragment

renderFragment :: [HTMLBody] -> Reader HTMLRenderOptions (Doc ann)
renderFragment bodies = do
  docs <- traverse renderBody (normalizeBody bodies)
  layout <- asks htmlLayout
  pure $ case layout of
    VerticalLayout -> vcat docs
    InlineLayout -> hcat docs

-- | Internal: Render the 'head' section
renderHeadSec :: [HTMLHead] -> Reader HTMLRenderOptions (Doc ann)
renderHeadSec heads = traverse renderHead heads >>= wrapBlock "head" []

-- | Internal: Render the 'body' section
renderBodySec :: [HTMLBody] -> Reader HTMLRenderOptions (Doc ann)
renderBodySec bodies = do
  contents <- renderFragment bodies
  wrapBlock "body" [] [contents]

-- | Internal: Render 'head' elements
renderHead :: HTMLHead -> Reader HTMLRenderOptions (Doc ann)
renderHead (Link relation file attrs) =
  pure $ angles ("link" <> renderAttrs (Attr "rel" relation : Attr "href" file : attrs))
renderHead (Title txt) = pure $ wrapLine "title" [] [pretty (escapeHTMLText txt)]
renderHead (Meta attrs) = pure $ angles ("meta" <> renderAttrs attrs)
renderHead (Script attrs txt) = wrapBlock "script" attrs [pretty txt | not (T.null txt)]

-- | Internal: Render 'body' elements
renderBody :: HTMLBody -> Reader HTMLRenderOptions (Doc ann)
renderBody (Div attrs ch) = renderBlock "div" attrs ch
renderBody (Paragraph attrs ch) = renderBlockInline "p" attrs ch
renderBody (List Unordered attrs items) = traverse renderIList items >>= wrapBlock "ul" attrs
  where
    renderIList (LItem iAttrs ch) = renderLine "li" iAttrs ch
renderBody (List Ordered attrs items) = traverse renderIList items >>= wrapBlock "ol" attrs
  where
    renderIList (LItem iAttrs ch) = renderBlockInline "li" iAttrs ch
renderBody (Section attrs ch) = renderBlock "section" attrs ch
renderBody (DescriptionList attrs items) = traverse renderDItem items >>= wrapBlock "dl" attrs
  where
    renderDItem (DTerm iAttrs ch) = renderLine "dt" iAttrs ch
    renderDItem (DDetails iAttrs ch) = renderBlockInline "dd" iAttrs ch
renderBody (Table attrs rows) = traverse renderRow rows >>= wrapBlock "table" attrs
  where
    renderRow (Row rAttrs cells) = traverse renderCell cells >>= wrapBlock "tr" rAttrs
    renderCell (THeader cAttrs ch) = renderLine "th" cAttrs ch
    renderCell (TData cAttrs ch) = renderLine "td" cAttrs ch
renderBody (Figure attrs ch) = renderBlock "figure" attrs ch
renderBody (FigCaption attrs ch) = renderLine "figcaption" attrs ch
renderBody (TextFormat fmt attrs ch) = renderLine (fmtTag fmt) attrs ch
renderBody (Heading lvl attrs ch) = renderLine (headTag lvl) attrs ch
renderBody (Anchor url attrs ch) = renderLine "a" (Attr "href" url : attrs) ch
renderBody (Img source altTxt attrs) =
  pure $ angles ("img" <> renderAttrs (Attr "src" source : Attr "alt" altTxt : attrs))
renderBody (RawText txt) = pure $ pretty (escapeHTMLText txt)
renderBody (Custom (CT tagName) attrs ch) = do
  tagTypes <- asks customElementTagTypes
  case M.lookup (CT tagName) tagTypes of
    Just Void -> pure $ angles (pretty tagName <> renderAttrs attrs <> " /")
    _ -> renderBlock tagName attrs ch
renderBody (Comment cmmnt) = pure $ "<!-- " <> pretty cmmnt <> " -->"

-- | Internal: gets tag from text format
fmtTag :: Format -> Text
fmtTag Bold = "b"
fmtTag Emphasis = "em"
fmtTag Subscript = "sub"
fmtTag Superscript = "sup"
fmtTag Span = "span"

-- | Internal: gets tag from heading level
headTag :: HeadingLevel -> Text
headTag H1 = "h1"
headTag H2 = "h2"
headTag H3 = "h3"
headTag H4 = "h4"
headTag H5 = "h5"
headTag H6 = "h6"

-- | Internal: Render the element and its children in the same line
renderLine :: Text -> [Attr] -> [HTMLBody] -> Reader HTMLRenderOptions (Doc ann)
renderLine tag attrs ch = wrapLine tag attrs <$> traverse renderBody ch

-- | Internal: Render the children breaking lines
renderBlock :: Text -> [Attr] -> [HTMLBody] -> Reader HTMLRenderOptions (Doc ann)
renderBlock tag attrs ch = traverse renderBody ch >>= wrapBlock tag attrs

-- | Internal: Render the element as a block, but keep all children on a single indented line
renderBlockInline :: Text -> [Attr] -> [HTMLBody] -> Reader HTMLRenderOptions (Doc ann)
renderBlockInline tag attrs [] = pure $ wrapLine tag attrs []
renderBlockInline tag attrs ch = traverse renderBody ch >>= wrapBlockInline tag attrs

-- | Internal: Wrap an element with tag and its children breaking lines
wrapBlock :: Text -> [Attr] -> [Doc ann] -> Reader HTMLRenderOptions (Doc ann)
wrapBlock = wrapBlockWith vcat

-- | Internal: Wrap an element with tag and its children in the same line
wrapLine :: Text -> [Attr] -> [Doc ann] -> Doc ann
wrapLine tag attrs docs =
  angles (pretty tag <> renderAttrs attrs) <> hcat docs <> angles ("/" <> pretty tag)

-- | Internal: Wrap an element with tags on separate lines, but children on the same line
wrapBlockInline :: Text -> [Attr] -> [Doc ann] -> Reader HTMLRenderOptions (Doc ann)
wrapBlockInline = wrapBlockWith hcat

-- | Internal: Wraps block elements using a document combining function.
wrapBlockWith :: ([Doc ann] -> Doc ann) -> Text -> [Attr] -> [Doc ann] -> Reader HTMLRenderOptions (Doc ann)
wrapBlockWith catDocs tag attrs docs = do
  layout <- asks htmlLayout
  case layout of
    InlineLayout -> pure $ wrapLine tag attrs docs
    VerticalLayout -> do
      indentation <- asks indentationSize
      pure $ vcat
        [ angles (pretty tag <> renderAttrs attrs),
          indent indentation (catDocs docs),
          angles ("/" <> pretty tag)
        ]

-- | Internal: Render attribute in the format 'key="value"'
renderAttrs :: [Attr] -> Doc ann
renderAttrs [] = mempty
renderAttrs attrs = space <> hsep (rAttr <$> attrs)
  where
    rAttr (Attr k v) = pretty k <> rValue v
    rValue value =
      case value of
        "" -> mempty
        _ -> equals <> dquotes (pretty value)

-- | Internal: Escapes a character for encoding in HTML
escapeHTMLText :: Text -> Text
escapeHTMLText = T.concatMap escapeChar
  where
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar c    = T.singleton c

-- | Internal: Normalizes body elements, merging adjacent 'RawText'
normalizeBody :: [HTMLBody] -> [HTMLBody]
normalizeBody [] = []
normalizeBody (RawText a : RawText b : rest) = normalizeBody (RawText (a <> b) : rest)
normalizeBody (x : xs) = normalizeNode x : normalizeBody xs

-- | Internal: Normalizes children from each node
normalizeNode :: HTMLBody -> HTMLBody
normalizeNode (Div attrs ch) = Div attrs (normalizeBody ch)
normalizeNode (Paragraph attrs ch) = Paragraph attrs (normalizeBody ch)
normalizeNode (List t attrs items) = List t attrs (normItem <$> items)
  where
    normItem (LItem a ch) = LItem a (normalizeBody ch)
normalizeNode (DescriptionList attrs items) = DescriptionList attrs (normDItem <$> items)
  where
    normDItem (DTerm a ch) = DTerm a (normalizeBody ch)
    normDItem (DDetails a ch) = DDetails a (normalizeBody ch)
normalizeNode (Table attrs rows) = Table attrs (normRow <$> rows)
  where
    normRow (Row a cells) = Row a (normCell <$> cells)
    normCell (THeader a ch) = THeader a (normalizeBody ch)
    normCell (TData a ch) = TData a (normalizeBody ch)
normalizeNode (Figure attrs ch) = Figure attrs (normalizeBody ch)
normalizeNode (FigCaption attrs ch) = FigCaption attrs (normalizeBody ch)
normalizeNode (TextFormat fmt attrs ch) = TextFormat fmt attrs (normalizeBody ch)
normalizeNode (Heading lvl attrs ch) = Heading lvl attrs (normalizeBody ch)
normalizeNode (Anchor url attrs ch) = Anchor url attrs (normalizeBody ch)
normalizeNode (Custom ct attrs ch) = Custom ct attrs (normalizeBody ch)
normalizeNode node = node
