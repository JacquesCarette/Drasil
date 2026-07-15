{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.HTML.Render (
    renderHTML,
    HTMLRenderOptions(..)
) where

import Prettyprinter (
  Doc, hcat, hsep, indent, vcat, angles, dquotes, equals, space, pretty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import Drasil.Data.Formats.HTML.Core (
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), CustomTag(..),
    Format(..), HLevel(..), Row(..), Cell(..), LItem(..), DItem(..), ListType(..),
    Attr(..)
  )

newtype HTMLRenderOptions = HTMLRO {
  -- | What 'TagType' is each 'CustomTag'?
  customElementTagTypes :: M.Map CustomTag TagType
  }

-- | Render 'HTML' to a 'Doc'
renderHTML :: HTMLRenderOptions ->  HTML -> Doc ann
renderHTML opt(HTML heads bodies) =
  vcat ["<!DOCTYPE html>", angles "html",
    indent 2 $ renderHeadSec heads,
    indent 2 $ renderBodySec opt bodies,
    angles "/html"]

-- | Render the 'head' section
renderHeadSec :: [HTMLHead] -> Doc ann
renderHeadSec heads = wrapBlock "head" [] (map renderHead heads)

-- | Render the 'body' section
renderBodySec :: HTMLRenderOptions -> [HTMLBody] -> Doc ann
renderBodySec opt bodies = wrapBlock "body" [] (map (renderBody opt) bodies)

-- | Render 'head' elements
renderHead :: HTMLHead -> Doc ann
renderHead (Link relation file attrs) =
  angles ("link" <> renderAttrs (Attr "rel" relation : Attr "href" file : attrs))
renderHead (Title txt)        = wrapBlock "title" [] [pretty (escapeHTMLText txt)]
renderHead (Meta attrs)       = angles ("meta" <> renderAttrs attrs)
renderHead (Script attrs txt) = wrapBlock "script" attrs [pretty txt]

-- | Render 'body' elements
renderBody :: HTMLRenderOptions -> HTMLBody -> Doc ann
renderBody opt  (Div attrs ch)       = renderBlock opt "div" attrs ch
renderBody opt  (Paragraph attrs ch) = renderBlockInline opt "p" attrs ch

renderBody opt  (List Unordered attrs items) = wrapBlock "ul" attrs (map renderIList items)
  where renderIList (LItem iAttrs ch) = renderBlockInline opt "li" iAttrs ch
renderBody opt  (List Ordered attrs items) = wrapBlock "ol" attrs (map renderIList items)
  where renderIList (LItem iAttrs ch) = renderBlockInline opt "li" iAttrs ch

renderBody opt  (DescriptionList attrs items) = wrapBlock "dl" attrs (map renderDItem items)
  where renderDItem (DTerm iAttrs ch)    = renderLine opt "dt" iAttrs ch
        renderDItem (DDetails iAttrs ch) = renderBlockInline opt "dd" iAttrs ch

renderBody opt  (Table attr rows)    = wrapBlock "table" attr (map renderRow rows)
  where renderRow (Row attrs cells)    = wrapBlock "tr" attrs (map renderCell cells)
        renderCell (THeader cAttrs ch) = renderLine opt "th" cAttrs ch
        renderCell (TData cAttrs ch)   = renderBlockInline opt "td" cAttrs ch

renderBody opt  (Figure attrs ch)     = renderBlock opt "figure" attrs ch
renderBody opt  (FigCaption attrs ch) = renderLine opt "figcaption" attrs ch

renderBody opt  (TextFormat fmt attrs ch) = renderLine opt (fmtTag fmt) attrs ch
renderBody opt  (Heading lvl attrs ch)    = renderLine opt (headTag lvl) attrs ch
renderBody opt  (Anchor url attrs ch)     = renderLine opt "a" (Attr "href" url : attrs) ch
renderBody _  (Img source altTxt attrs) = angles ("img" <> renderAttrs (Attr "src" source : Attr "alt" altTxt : attrs))
renderBody _  (RawText txt)             = pretty (escapeHTMLText txt)

renderBody opt (Custom (CT tagName) attrs ch)
  | Just Void <- M.lookup (CT tagName) (customElementTagTypes opt) = angles (pretty tagName <> renderAttrs attrs <> " /")
  | otherwise = renderBlock opt tagName attrs ch

renderBody _ (Comment cmmnt) = "<!-- " <> pretty cmmnt <> "-->"

-- | Internal: gets tag from text format
fmtTag :: Format -> Text
fmtTag Bold = "b"
fmtTag Emphasis = "em"
fmtTag Subscript = "sub"
fmtTag Superscript = "sup"
fmtTag Span = "span"

-- | Internal: gets tag from heading level
headTag :: HLevel -> Text
headTag H1 = "h1"
headTag H2 = "h2"
headTag H3 = "h3"
headTag H4 = "h4"
headTag H5 = "h5"
headTag H6 = "h6"

-- | Render the element and its children in the same line
renderLine :: HTMLRenderOptions -> Text -> [Attr] -> [HTMLBody] -> Doc ann
renderLine opt tag attrs = wrapLine tag attrs . map (renderBody opt)

-- | Render the children breaking lines
renderBlock :: HTMLRenderOptions -> Text -> [Attr] -> [HTMLBody] -> Doc ann
renderBlock opt tag attrs = wrapBlock tag attrs . map (renderBody opt)

-- | Render the element as a block, but keep all children on a single indented line
renderBlockInline :: HTMLRenderOptions -> Text -> [Attr] -> [HTMLBody] -> Doc ann
renderBlockInline opt tag attrs = wrapBlockInline tag attrs . map (renderBody opt)

-- | Wrap an element with tag and its children breaking lines
wrapBlock :: Text -> [Attr] -> [Doc ann] -> Doc ann
wrapBlock tag attrs docs =
  vcat [angles (pretty tag <> renderAttrs attrs), indent 2 (vcat docs),
  angles ("/" <> pretty tag)]

-- | Wrap an element with tag and its children in the same line
wrapLine :: Text -> [Attr] -> [Doc ann] -> Doc ann
wrapLine tag attrs docs =
  angles (pretty tag <> renderAttrs attrs) <> hcat docs <> angles ("/" <> pretty tag)

-- | Wrap an element with tags on separate lines, but children on the same line
wrapBlockInline :: Text -> [Attr] -> [Doc ann] -> Doc ann
wrapBlockInline tag attrs docs =
  vcat [angles (pretty tag <> renderAttrs attrs), indent 2 (hcat docs),
  angles ("/" <> pretty tag)]

-- | Render attribute in the format 'key="value"'
renderAttrs :: [Attr] -> Doc ann
renderAttrs [] = mempty
renderAttrs attrs = space <> hsep (map rAttr attrs)
  where
    rAttr (Attr k v) = pretty k <> rValue v
    rValue value =
      case value of
        "" -> mempty
        _  -> equals <> dquotes (pretty value)

-- | Internal: Escapes a character for encoding in HTML
escapeHTMLText :: Text -> Text
escapeHTMLText = T.concatMap escapeChar
  where
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar '/'  = "&#x2F;"
    escapeChar c    = T.singleton c
