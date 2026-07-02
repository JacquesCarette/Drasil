{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.HTML.Render (
    renderHTML
) where

import Prettyprinter (
  Doc, hcat, hsep, indent, vcat, angles, dquotes, equals, space, pretty)
import Data.Text (Text)
import qualified Data.Text as T

import Drasil.Data.Formats.HTML.Core (
    HTML(..), HTMLBody(..), HTMLHead(..), TagType(..), Format(..), HLevel(..),
    Row(..), Cell(..), LItem(..), DItem(..), ListType(..), Attr(..)
  )

-- | A 'CustomTag' is either (a) an ill-supported HTML-spec. node (ill-supported
-- by 'HTMLBody', that is) or (b) a purely custom one.
newtype CustomTag = CT Text

customTag :: Text -> CustomTag
customTag t
  -- | tag names are used within element start tags and end tags to give the
  -- element’s name. HTML elements all have names that only use characters in
  -- the range 0–9, a–z, and A–Z.
  | isSanitary t = CT t
  | otherwise = error "bad custom tag name"

data HTMLRenderOptions = HTMLRO {
  -- | What 'TagType' is each 'CustomTag'?
  customElementTagTypes :: M.Map CustomTag TagType.

  -- FIXME: All referenced 'CustomTag's need to (a) be in this map and (b)
  -- declare whether the custom tag is 'Void' or 'Standard'.
}

-- | Render 'HTML' to a 'Doc'
renderHTML :: HTML -> Doc ann
renderHTML (HTML heads bodies) =
  vcat ["<!DOCTYPE html>", angles "html", renderHeadSec heads, renderBodySec bodies, angles "/html"]

-- | Render the 'head' section
renderHeadSec :: [HTMLHead] -> Doc ann
renderHeadSec heads = wrapBlock "head" [] (map renderHead heads)

-- | Render the 'body' section
renderBodySec :: [HTMLBody] -> Doc ann
renderBodySec bodies = wrapBlock "body" [] (map renderBody bodies)

-- | Render 'head' elements
renderHead :: HTMLHead -> Doc ann
renderHead (Link relation file attrs) =
  angles ("link" <> renderAttrs (Attr "rel" relation : Attr "href" file : attrs))
renderHead (Title txt)        = wrapBlock "title" [] [pretty (escapeHTMLText txt)]
renderHead (Meta attrs)       = angles ("meta" <> renderAttrs attrs)
renderHead (Script attrs txt) = angles ("script" <> renderAttrs attrs) <> pretty txt <> angles "/script"

-- | Render 'body' elements
renderBody :: HTMLBody -> Doc ann
renderBody (Div attrs ch)       = renderBlock "div" attrs ch
renderBody (Paragraph attrs ch) = renderLine "p" attrs ch

renderBody (List Unordered attrs items) = wrapBlock "ul" attrs (map renderIList items)
  where renderIList (LItem iAttrs ch) = renderBlock "li" iAttrs ch
renderBody (List Ordered attrs items) = wrapBlock "ol" attrs (map renderIList items)
  where renderIList (LItem iAttrs ch) = renderBlock "li" iAttrs ch

renderBody (DescriptionList attrs items) = wrapBlock "dl" attrs (map renderDItem items)
  where renderDItem (DTerm iAttrs ch)    = renderLine "dt" iAttrs ch
        renderDItem (DDetails iAttrs ch) = renderBlock "dd" iAttrs ch

renderBody (Table attr rows)    = wrapBlock "table" attr (map renderRow rows)
  where renderRow (Row attrs cells)    = wrapBlock "tr" attrs (map renderCell cells)
        renderCell (THeader cAttrs ch) = renderLine "th" cAttrs ch
        renderCell (TData cAttrs ch)   = renderLine "td" cAttrs ch

renderBody (Figure attrs ch)     = renderBlock "figure" attrs ch
renderBody (FigCaption attrs ch) = renderLine "figcaption" attrs ch

renderBody (TextFormat fmt attrs ch) = renderLine (fmtTag fmt) attrs ch
renderBody (Heading lvl attrs ch)    = renderLine (headTag lvl) attrs ch
renderBody (Anchor url attrs ch)     = renderLine "a" (Attr "href" url : attrs) ch
renderBody (Img source altTxt attrs) = angles ("img" <> renderAttrs (Attr "src" source : Attr "alt" altTxt : attrs))
renderBody (RawText txt)             = pretty (escapeHTMLText txt)

renderBody (CustomTag tagName Standard attrs ch) = renderBlock tagName attrs ch
renderBody (CustomTag tagName Void attrs _)      = angles (pretty tagName <> renderAttrs attrs)

renderBody (Comment cmmnt) = "<!-- " <> pretty cmmnt <> "-->"

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
renderLine :: Text -> [Attr] -> [HTMLBody] -> Doc ann
renderLine tag attrs = wrapLine tag attrs . map renderBody

-- | Render the children breaking lines
renderBlock :: Text -> [Attr] -> [HTMLBody] -> Doc ann
renderBlock tag attrs = wrapBlock tag attrs . map renderBody

-- | Wrap an element with tag and its children breaking lines
wrapBlock :: Text -> [Attr] -> [Doc ann] -> Doc ann
wrapBlock tag attrs docs =
  vcat [ angles (pretty tag <> renderAttrs attrs), indent 2 (vcat docs),
  angles ("/" <> pretty tag) ]

-- | Wrap an element with tag and its children in the same line
wrapLine :: Text -> [Attr] -> [Doc ann] -> Doc ann
wrapLine tag attrs docs =
  angles (pretty tag <> renderAttrs attrs) <> hcat docs <> angles ("/" <> pretty tag)

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
