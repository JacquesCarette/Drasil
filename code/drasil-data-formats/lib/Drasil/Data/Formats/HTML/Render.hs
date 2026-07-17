{-# LANGUAGE OverloadedStrings #-}

module Drasil.Data.Formats.HTML.Render (
    renderHTML,
    HTMLGenOptions(..)
) where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Drasil.Data.Formats.HTML.Core
  (Attr (..), Cell (..), CustomTag (..), DItem (..), Format (..), HLevel (..),
  HTML (..), HTMLBody (..), HTMLHead (..), LItem (..), ListType (..), Row (..),
  TagType (..),
  )
import Prettyprinter
  (vcat, Doc, angles, dquotes, equals, hcat, hsep, indent, pretty, space)

data HTMLGenOptions = HTMLBO
  { -- | What 'TagType' is each 'CustomTag'?
    customElementTagTypes :: M.Map CustomTag TagType,
    -- | The number of spaces to use for each level of indentation.
    indentationSize :: Int
  }

-- | Render 'HTML' to a 'Doc'
renderHTML :: HTMLGenOptions -> HTML -> Doc ann
renderHTML opt (HTML heads bodies) =
  vcat
    [ "<!DOCTYPE html>",
      angles "html",
      indent (indentationSize opt) $ renderHeadSec opt heads,
      indent (indentationSize opt) $ renderBodySec opt bodies,
      angles "/html"
    ]

-- | Render the 'head' section
renderHeadSec :: HTMLGenOptions -> [HTMLHead] -> Doc ann
renderHeadSec opt heads = wrapBlock opt "head" [] (map (renderHead opt) heads)

-- | Render the 'body' section
renderBodySec :: HTMLGenOptions -> [HTMLBody] -> Doc ann
renderBodySec opt bodies = wrapBlock opt "body" [] (map (renderBody opt) bodies)

-- | Render 'head' elements
renderHead :: HTMLGenOptions -> HTMLHead -> Doc ann
renderHead _ (Link relation file attrs) =
  angles ("link" <> renderAttrs (Attr "rel" relation : Attr "href" file : attrs))
renderHead opt (Title txt) = wrapBlock opt "title" [] [pretty (escapeHTMLText txt)]
renderHead _ (Meta attrs) = angles ("meta" <> renderAttrs attrs)
renderHead opt (Script attrs txt) = wrapBlock opt "script" attrs [pretty txt]

-- | Render 'body' elements
renderBody :: HTMLGenOptions -> HTMLBody -> Doc ann
renderBody opt (Div attrs ch) = renderBlock opt "div" attrs ch
renderBody opt (Paragraph attrs ch) = renderBlockInline opt "p" attrs ch
renderBody opt (List Unordered attrs items) = wrapBlock opt "ul" attrs (map renderIList items)
  where
    renderIList (LItem iAttrs ch) = renderBlockInline opt "li" iAttrs ch
renderBody opt (List Ordered attrs items) = wrapBlock opt "ol" attrs (map renderIList items)
  where
    renderIList (LItem iAttrs ch) = renderBlockInline opt "li" iAttrs ch
renderBody opt (DescriptionList attrs items) = wrapBlock opt "dl" attrs (map renderDItem items)
  where
    renderDItem (DTerm iAttrs ch) = renderLine opt "dt" iAttrs ch
    renderDItem (DDetails iAttrs ch) = renderBlockInline opt "dd" iAttrs ch
renderBody opt (Table attr rows) = wrapBlock opt "table" attr (map renderRow rows)
  where
    renderRow (Row attrs cells) = wrapBlock opt "tr" attrs (map renderCell cells)
    renderCell (THeader cAttrs ch) = renderLine opt "th" cAttrs ch
    renderCell (TData cAttrs ch) = renderBlockInline opt "td" cAttrs ch
renderBody opt (Figure attrs ch) = renderBlock opt "figure" attrs ch
renderBody opt (FigCaption attrs ch) = renderLine opt "figcaption" attrs ch
renderBody opt (TextFormat fmt attrs ch) = renderLine opt (fmtTag fmt) attrs ch
renderBody opt (Heading lvl attrs ch) = renderLine opt (headTag lvl) attrs ch
renderBody opt (Anchor url attrs ch) = renderLine opt "a" (Attr "href" url : attrs) ch
renderBody _ (Img source altTxt attrs) = angles ("img" <> renderAttrs (Attr "src" source : Attr "alt" altTxt : attrs))
renderBody _ (RawText txt) = pretty (escapeHTMLText txt)
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
renderLine :: HTMLGenOptions -> Text -> [Attr] -> [HTMLBody] -> Doc ann
renderLine opt tag attrs = wrapLine tag attrs . map (renderBody opt)

-- | Render the children breaking lines
renderBlock :: HTMLGenOptions -> Text -> [Attr] -> [HTMLBody] -> Doc ann
renderBlock opt tag attrs = wrapBlock opt tag attrs . map (renderBody opt)

-- | Render the element as a block, but keep all children on a single indented line
renderBlockInline :: HTMLGenOptions -> Text -> [Attr] -> [HTMLBody] -> Doc ann
renderBlockInline _ tag attrs [] = wrapLine tag attrs []
renderBlockInline opt tag attrs ch = wrapBlockInline opt tag attrs (map (renderBody opt) ch)

-- | Wrap an element with tag and its children breaking lines
wrapBlock :: HTMLGenOptions -> Text -> [Attr] -> [Doc ann] -> Doc ann
wrapBlock opt tag attrs docs =
  vcat
    [ angles (pretty tag <> renderAttrs attrs),
      indent (indentationSize opt) (vcat docs),
      angles ("/" <> pretty tag)
    ]

-- | Wrap an element with tag and its children in the same line
wrapLine :: Text -> [Attr] -> [Doc ann] -> Doc ann
wrapLine tag attrs docs =
  angles (pretty tag <> renderAttrs attrs) <> hcat docs <> angles ("/" <> pretty tag)

-- | Wrap an element with tags on separate lines, but children on the same line
wrapBlockInline :: HTMLGenOptions -> Text -> [Attr] -> [Doc ann] -> Doc ann
wrapBlockInline opt tag attrs docs =
  vcat [ angles (pretty tag <> renderAttrs attrs),
  indent (indentationSize opt) (hcat docs), angles ("/" <> pretty tag)]

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
