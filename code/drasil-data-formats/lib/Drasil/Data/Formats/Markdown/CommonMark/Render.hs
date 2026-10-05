{-# LANGUAGE OverloadedStrings #-}

-- | Internal CommonMark rendering foundations. This module does not yet render
-- a CommonMark document and is not re-exported by the public facade.
module Drasil.Data.Formats.Markdown.CommonMark.Render
  ( EmphasisDelimiter (..),
    CommonMarkRenderOptions (..),
    defaultCommonMarkRO,
    RenderedLines,
    normalizeLineEndings,
    emptyInlineLines,
    singletonLine,
    appendRenderedLines,
    wrapRenderedLines,
    addToFirstLine,
    addToLastLine,
    textToRenderedLines,
    docToRenderedLines,
    prefixBlockQuote,
    separateRenderedBlocks,
    concatenateRenderedLines,
    renderedLinesToDoc,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Drasil.Data.Formats.HTML qualified as HTML
import Prettyprinter (Doc, concatWith, defaultLayoutOptions, hardline, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

-- | Marker used to delimit emphasized CommonMark text.
data EmphasisDelimiter
  = -- | Use an asterisk marker.
    Asterisk
  | -- | Use an underscore marker.
    Underscore
  deriving (Eq, Show)

-- | Options controlling CommonMark rendering.
data CommonMarkRenderOptions
  = -- | Configure inline delimiters and embedded HTML rendering.
    CommonMarkRO
      { -- | Marker surrounding emphasized contents.
        emphasisDelimiter :: EmphasisDelimiter,
        -- | Marker, repeated twice, surrounding strongly emphasized contents.
        strongDelimiter :: EmphasisDelimiter,
        -- | Options used when rendering structured HTML fallbacks.
        htmlRenderOptions :: HTML.HTMLRenderOptions
      }

-- | Default CommonMark rendering options.
--
-- Both emphasis forms use asterisks and HTML uses its default renderer.
defaultCommonMarkRO :: CommonMarkRenderOptions
defaultCommonMarkRO =
  CommonMarkRO
    { emphasisDelimiter = Asterisk,
      strongDelimiter = Asterisk,
      htmlRenderOptions = HTML.defaultHTMLRO
    }

-- True marks a syntactically empty line, which must not acquire indentation.
data PhysicalLine ann = PhysicalLine Bool (Doc ann)

-- | Physical lines retained until the final prettyprinter layout.
newtype RenderedLines ann = RenderedLines [PhysicalLine ann]

-- | Replace CRLF and CR with LF without changing the number of lines.
normalizeLineEndings :: Text -> Text
normalizeLineEndings = T.replace "\r" "\n" . T.replace "\r\n" "\n"

-- | An empty line ready to receive inline content.
emptyInlineLines :: RenderedLines ann
emptyInlineLines = RenderedLines [blankPhysicalLine]

blankPhysicalLine :: PhysicalLine ann
blankPhysicalLine = PhysicalLine True mempty

contentPhysicalLine :: Doc ann -> PhysicalLine ann
contentPhysicalLine = PhysicalLine False

textPhysicalLine :: Text -> PhysicalLine ann
textPhysicalLine contents = PhysicalLine (T.null contents) (pretty contents)

-- | Wrap a single-line document as physical content.
singletonLine :: Doc ann -> RenderedLines ann
singletonLine = RenderedLines . pure . contentPhysicalLine

renderedLines :: RenderedLines ann -> [PhysicalLine ann]
renderedLines (RenderedLines physicalLines) = physicalLines

-- | Join inline pieces, merging only their touching boundary lines.
appendRenderedLines :: RenderedLines ann -> RenderedLines ann -> RenderedLines ann
appendRenderedLines (RenderedLines leftLines) (RenderedLines rightLines) =
  case (reverse leftLines, rightLines) of
    ([], _) -> RenderedLines rightLines
    (_, []) -> RenderedLines leftLines
    (leftLast : leftRest, rightFirst : rightRest) ->
      RenderedLines
        (reverse leftRest <> (combinePhysicalLines leftLast rightFirst : rightRest))

combinePhysicalLines :: PhysicalLine ann -> PhysicalLine ann -> PhysicalLine ann
combinePhysicalLines (PhysicalLine leftBlank leftDoc) (PhysicalLine rightBlank rightDoc) =
  PhysicalLine (leftBlank && rightBlank) (leftDoc <> rightDoc)

-- | Add a marker before the first line and after the last line.
wrapRenderedLines :: Doc ann -> RenderedLines ann -> RenderedLines ann
wrapRenderedLines marker = addToLastLine marker . addToFirstLine marker

-- | Add a prefix to the first line, creating a line if needed.
addToFirstLine :: Doc ann -> RenderedLines ann -> RenderedLines ann
addToFirstLine prefix (RenderedLines []) = singletonLine prefix
addToFirstLine prefix (RenderedLines (PhysicalLine _ contents : remainingLines)) =
  RenderedLines (contentPhysicalLine (prefix <> contents) : remainingLines)

-- | Add a suffix to the last line, creating a line if needed.
addToLastLine :: Doc ann -> RenderedLines ann -> RenderedLines ann
addToLastLine suffix (RenderedLines []) = singletonLine suffix
addToLastLine suffix (RenderedLines physicalLines) =
  RenderedLines (reverse (addSuffix (reverse physicalLines)))
  where
    addSuffix (PhysicalLine _ contents : remainingLines) =
      contentPhysicalLine (contents <> suffix) : remainingLines
    addSuffix [] = [contentPhysicalLine suffix]

-- | Split normalized text into physical lines, preserving empty lines.
textToRenderedLines :: Text -> RenderedLines ann
textToRenderedLines =
  RenderedLines . fmap textPhysicalLine . T.splitOn "\n" . normalizeLineEndings

-- | Lay out an HTML fragment and retain its physical lines.
docToRenderedLines :: Doc ann -> RenderedLines otherAnn
docToRenderedLines = textToRenderedLines . renderStrict . layoutPretty defaultLayoutOptions

-- | Prefix every physical line; empty lines receive a bare @>@.
prefixBlockQuote :: RenderedLines ann -> RenderedLines ann
prefixBlockQuote (RenderedLines []) = singletonLine ">"
prefixBlockQuote (RenderedLines physicalLines) =
  RenderedLines (prefixLine <$> physicalLines)
  where
    prefixLine (PhysicalLine True _) = contentPhysicalLine ">"
    prefixLine (PhysicalLine False contents) = contentPhysicalLine ("> " <> contents)

-- | Separate nonempty block results by one blank line; skip absent blocks.
separateRenderedBlocks :: [RenderedLines ann] -> RenderedLines ann
separateRenderedBlocks =
  RenderedLines . concat . intersperse [blankPhysicalLine]
    . filter (not . null) . fmap renderedLines

-- | Join line sequences without inserting separators.
concatenateRenderedLines :: [RenderedLines ann] -> RenderedLines ann
concatenateRenderedLines = RenderedLines . concatMap renderedLines

-- | Join physical lines with hard breaks that cannot flatten into spaces.
renderedLinesToDoc :: RenderedLines ann -> Doc ann
renderedLinesToDoc (RenderedLines physicalLines) =
  concatWith
    (\left right -> left <> hardline <> right)
    (physicalLineDoc <$> physicalLines)

physicalLineDoc :: PhysicalLine ann -> Doc ann
physicalLineDoc (PhysicalLine _ contents) = contents
