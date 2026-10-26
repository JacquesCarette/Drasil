{-# LANGUAGE OverloadedStrings #-}

-- | Render the supported CommonMark subset, including structured HTML fallbacks.
module Drasil.Data.Formats.Markdown.CommonMark.Render
  ( EmphasisDelimiter (..),
    CodeFenceStyle (..),
    CommonMarkRenderOptions (..),
    defaultCommonMarkRO,
    renderCommonMark,
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
    renderInlines,
    renderCodeSpan,
    renderCodeFence,
  )
where

import Data.Char (isAlphaNum, isAscii, isPunctuation, isSpace, isSymbol, ord)
import Data.List (intercalate, unsnoc)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (swap)
import Drasil.Data.Formats.HTML qualified as HTML
import Drasil.Data.Formats.HTML.Render (escapeHTMLText)
import Drasil.Data.Formats.Markdown.CommonMark.Core
  (Block (..), CommonMark (..), Inline (..), ListItem (..))
import Numeric (showHex)
import Prettyprinter (Doc, concatWith, defaultLayoutOptions, hardline, layoutPretty, pretty, surround)
import Prettyprinter.Render.Text (renderStrict)

-- | Marker used to delimit emphasized CommonMark text.
data EmphasisDelimiter
  = -- | Use an asterisk marker.
    Asterisk
  | -- | Use an underscore marker.
    Underscore
  deriving (Eq, Show)

-- | Preferred marker for fenced code blocks.
data CodeFenceStyle
  = -- | Use backticks, falling back to tildes when the info string contains a backtick.
    BacktickFence
  | -- | Use tildes.
    TildeFence
  deriving (Eq, Show)

-- | Options controlling CommonMark rendering.
data CommonMarkRenderOptions
  = -- | Configure inline delimiters, code fences and embedded HTML rendering.
    CommonMarkRO
      { -- | Marker surrounding emphasized contents.
        emphasisDelimiter :: EmphasisDelimiter,
        -- | Marker, repeated twice, surrounding strongly emphasized contents.
        strongDelimiter :: EmphasisDelimiter,
        -- | Preferred fence style; backticks in an info string require tildes.
        codeFenceStyle :: CodeFenceStyle,
        -- | Options used when rendering structured HTML fallbacks.
        htmlRenderOptions :: HTML.HTMLRenderOptions
      }

-- | Default CommonMark rendering options.
--
-- Both emphasis forms use asterisks, code fences prefer backticks, and HTML
-- uses its default renderer.
defaultCommonMarkRO :: CommonMarkRenderOptions
defaultCommonMarkRO =
  CommonMarkRO
    { emphasisDelimiter = Asterisk,
      strongDelimiter = Asterisk,
      codeFenceStyle = BacktickFence,
      htmlRenderOptions = HTML.defaultHTMLRO
    }

-- | Render a document without a trailing newline. Empty paragraphs, lists and
-- HTML fragments are omitted; empty quotes and list items retain their markers.
-- Lists use loose spacing so multi-paragraph items keep their block structure.
-- Inline contents must not contain blank lines, block HTML or nested links.
-- HTML comments and custom raw-text elements must not contain blank lines: HTML
-- block separators are guarded with comments to keep their Markdown context.
-- HTML fragments beginning with inline content receive a @div@ wrapper, so
-- their text is interpreted as HTML rather than Markdown.
-- At paragraph boundaries, source line breaks are omitted and hard breaks use
-- inline HTML. Headings are validated by @mkHeading@ before rendering; their
-- leading and trailing spaces and tabs follow CommonMark's trimming rules.
renderCommonMark :: CommonMarkRenderOptions -> CommonMark -> Doc ann
renderCommonMark opt (CommonMark blocks) = renderedLinesToDoc (renderBlocks opt blocks)

renderBlocks :: CommonMarkRenderOptions -> [Block] -> RenderedLines ann
renderBlocks opt = snd . renderBlocksWithFirst opt

-- Retain the first visible source block alongside the shared rendered results,
-- so list items can inspect their opening syntax without rendering it twice.
renderBlocksWithFirst :: CommonMarkRenderOptions -> [Block] -> (Maybe Block, RenderedLines ann)
renderBlocksWithFirst opt blocks =
  (fst <$> listToMaybe rendered, separateRenderedBlocks (separateLists rendered))
  where
    rendered = filter present (fmap render blocks)
    render block = (block, renderBlock opt block)
    present (_, RenderedLines ls) = not (null ls)
    separateLists [] = []
    separateLists [(_, contents)] = [contents]
    separateLists ((block, contents) : remaining@((next, _) : _)) =
      contents : [singletonLine "<!-- -->" | sameList block next] <> separateLists remaining
    sameList (OrderedList _) (OrderedList _) = True
    sameList (UnorderedList _) (UnorderedList _) = True
    sameList _ _ = False

renderBlock :: CommonMarkRenderOptions -> Block -> RenderedLines ann
renderBlock opt (Heading level contents) =
  singletonLine (pretty marker <> if T.null text then mempty else " " <> pretty text)
  where
    marker = T.replicate (fromEnum level + 1) "#"
    -- Inline HTML can introduce layout newlines even when its contents have none.
    text = T.dropAround (\c -> c == ' ' || c == '\t') $
      T.replace "\n" " " (renderInlineText opt contents)
renderBlock opt (Paragraph contents) =
  nonemptyText $ T.intercalate "\n" $
    protectStart . protectTrailingWhitespace <$> trimLines (T.splitOn "\n" (renderParagraphText opt contents))
  where
    trimLines = reverse . dropWhile T.null . reverse . dropWhile T.null
    protectStart line = case T.uncons line of
      -- A comment can interrupt a paragraph even when another tag follows it.
      Just ('<', _) | "<!--" `T.isPrefixOf` line -> "<span></span>" <> line
      -- A standalone inline tag must not open a CommonMark HTML block.
      Just ('<', _) | not (T.null tagEnd) && T.null (T.strip (T.drop 1 tagEnd)) ->
        line <> "<!-- -->"
      _ -> protectLeadingWhitespace line
      where (_, tagEnd) = T.breakOn ">" line
renderBlock opt (BlockQuote blocks) = prefixBlockQuote (renderBlocks opt blocks)
renderBlock opt (CodeBlock info contents) = renderCodeFence (codeFenceStyle opt) info contents
renderBlock opt (OrderedList items) = renderList opt [T.pack (show n) <> "." | n <- [1 :: Int ..]] items
renderBlock opt (UnorderedList items) = renderList opt (repeat "-") items
renderBlock _ ThematicBreak = singletonLine "---"
renderBlock opt (HTMLBlock bodies)
  | null nonemptyBodies = RenderedLines []
  | otherwise = nonemptyText (protectHTMLBlankLines rendered)
  where
    nonemptyBodies = filter notEmpty bodies
    notEmpty (HTML.RawText t) = not (T.null t)
    notEmpty _ = True
    blockBodies = case nonemptyBodies of
      first : _ | startsHTMLBlock first -> nonemptyBodies
      _ -> [HTML.Div [] nonemptyBodies]
    rendered = renderStrict (layoutPretty defaultLayoutOptions
      (HTML.renderHTMLFragment (htmlRenderOptions opt) blockBodies))

-- These tags establish raw HTML block context until the next blank line.
-- Inline tags, comments and custom tags need a wrapper for following text.
startsHTMLBlock :: HTML.HTMLBody -> Bool
startsHTMLBlock (HTML.Div _ _) = True
startsHTMLBlock (HTML.Paragraph _ _) = True
startsHTMLBlock HTML.Heading{} = True
startsHTMLBlock HTML.List{} = True
startsHTMLBlock (HTML.Section _ _) = True
startsHTMLBlock (HTML.Table _ _) = True
startsHTMLBlock (HTML.DescriptionList _ _) = True
startsHTMLBlock (HTML.Figure _ _) = True
startsHTMLBlock (HTML.FigCaption _ _) = True
startsHTMLBlock _ = False

-- A comment prevents a blank line from ending an ordinary CommonMark HTML
-- block. Custom raw-text elements should be supplied without blank lines:
-- comments inside their literal contents would change that text.
protectHTMLBlankLines :: Text -> Text
protectHTMLBlankLines input
  | T.null input = input
  | otherwise = T.intercalate "\n" (protect <$> T.splitOn "\n" (normalizeLineEndings input))
  where
    protect line | T.all (`elem` [' ', '\t']) line = line <> "<!-- -->"
                 | otherwise = line

nonemptyText :: Text -> RenderedLines ann
nonemptyText text
  | T.null text = RenderedLines []
  | otherwise = textToRenderedLines text

renderList :: CommonMarkRenderOptions -> [Text] -> [ListItem] -> RenderedLines ann
renderList opt markers = separateRenderedBlocks . zipWith renderItem markers
  where
    -- A marker-only line avoids thematic-break priority for @- ---@ and
    -- for nested empty unordered lists such as @- - -@.
    startsWithDashes ThematicBreak = True
    startsWithDashes (UnorderedList _) = True
    startsWithDashes _ = False
    renderItem marker (ListItem blocks) =
      case renderedLines contents of
        [] -> singletonLine (pretty marker)
        ls@(PhysicalLine _ first : rest)
          | any startsWithDashes firstBlock -> RenderedLines
              (contentPhysicalLine (pretty marker) : fmap indentLine ls)
          | otherwise -> RenderedLines
              (contentPhysicalLine (pretty marker <> " " <> first) : fmap indentLine rest)
      where
        (firstBlock, contents) = renderBlocksWithFirst opt blocks
        padding = pretty (T.replicate (T.length marker + 1) " ")
        indentLine (PhysicalLine True _) = blankPhysicalLine
        indentLine (PhysicalLine False line) = contentPhysicalLine (padding <> line)

-- A final backslash-newline is not a CommonMark hard break at a block boundary.
-- Render only boundary hard breaks with HTML; interior breaks keep their usual
-- Markdown syntax. Empty inline nodes must not hide those boundaries.
renderParagraphText :: CommonMarkRenderOptions -> [Inline] -> Text
renderParagraphText opt contents =
  prefix <> renderInlineText opt middle <> suffix
  where
    nodes = normalizeInlines contents
    (prefix, afterStart) = leading nodes
    (suffix, reversedMiddle) = leading (reverse afterStart)
    middle = reverse reversedMiddle
    leading (SoftBreak : remaining) = leading remaining
    leading (HardBreak : remaining) =
      let (breaks, rest) = leading remaining in ("<br />" <> breaks, rest)
    leading remaining = ("", remaining)

-- True marks a syntactically empty line, which must not acquire indentation.
data PhysicalLine ann = PhysicalLine Bool (Doc ann)

-- | Physical lines retained until the final prettyprinter layout.
newtype RenderedLines ann = RenderedLines { renderedLines :: [PhysicalLine ann] }

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

-- | Join inline pieces, merging only their touching boundary lines.
appendRenderedLines :: RenderedLines ann -> RenderedLines ann -> RenderedLines ann
appendRenderedLines (RenderedLines leftLines) (RenderedLines rightLines) =
  RenderedLines $ case (unsnoc leftLines, rightLines) of
    (Nothing, _) -> rightLines
    (_, []) -> leftLines
    (Just (leftRest, leftLast), rightFirst : rightRest) ->
      leftRest <> (combinePhysicalLines leftLast rightFirst : rightRest)

combinePhysicalLines :: PhysicalLine ann -> PhysicalLine ann -> PhysicalLine ann
combinePhysicalLines (PhysicalLine leftBlank leftDoc) (PhysicalLine rightBlank rightDoc) =
  PhysicalLine (leftBlank && rightBlank) (leftDoc <> rightDoc)

-- | Add a marker before the first line and after the last line.
wrapRenderedLines :: Doc ann -> RenderedLines ann -> RenderedLines ann
wrapRenderedLines marker = addToLastLine marker . addToFirstLine marker

-- | Add a prefix to the first line, creating a line if needed.
addToFirstLine :: Doc ann -> RenderedLines ann -> RenderedLines ann
addToFirstLine prefix =
  appendRenderedLines (singletonLine prefix)

-- | Add a suffix to the last line, creating a line if needed.
addToLastLine :: Doc ann -> RenderedLines ann -> RenderedLines ann
addToLastLine suffix body =
  appendRenderedLines body (singletonLine suffix)

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
  RenderedLines . intercalate [blankPhysicalLine]
    . filter (not . null) . fmap renderedLines

-- | Join line sequences without inserting separators.
concatenateRenderedLines :: [RenderedLines ann] -> RenderedLines ann
concatenateRenderedLines = RenderedLines . concatMap renderedLines

-- | Join physical lines with hard breaks that cannot flatten into spaces.
renderedLinesToDoc :: RenderedLines ann -> Doc ann
renderedLinesToDoc (RenderedLines physicalLines) =
  concatWith (surround hardline) (physicalLineDoc <$> physicalLines)

physicalLineDoc :: PhysicalLine ann -> Doc ann
physicalLineDoc (PhysicalLine _ contents) = contents

-- | Render a sequence within a paragraph or heading, without adding separators.
-- Callers must supply valid inline contents: no blank lines, block HTML or
-- nested links. Breaks at block boundaries are handled by the block renderer.
renderInlines :: CommonMarkRenderOptions -> [Inline] -> RenderedLines ann
renderInlines opt = textToRenderedLines . renderInlineText opt

renderInlineText :: CommonMarkRenderOptions -> [Inline] -> Text
renderInlineText opt = renderInlineTextWithBreaks opt "\n" "\\\n"

renderInlineTextWithBreaks :: CommonMarkRenderOptions -> Text -> Text -> [Inline] -> Text
renderInlineTextWithBreaks opt softBreak hardBreak = T.concat . go Nothing False . normalizeInlines
  where
    go _ _ [] = []
    go previous startsLine (node : remaining) =
      let next = listToMaybe remaining
          rendered = renderNode previous node next
          contents = if startsLine then protectLeadingWhitespace rendered else rendered
          following = go (Just node) ("\n" `T.isSuffixOf` contents) remaining
          -- A newline can come from a break node, literal text or inline HTML.
          -- Trailing spaces must not turn any such newline into a hard break.
          protected = case following of
            first : _ | "\n" `T.isPrefixOf` first -> protectTrailingSpaces contents
            _ -> contents
      in protected : following
    renderNode _ (Plain t) _ = escapePlain (normalizeLineEndings t)
    renderNode before (Emphasis ch) after =
      emphasized "em" (emphasisDelimiter opt) 1 before ch after
    renderNode before (Strong ch) after =
      emphasized "strong" (strongDelimiter opt) 2 before ch after
    renderNode before (CodeSpan t) after
      | any adjacentCode [before, after] =
          "<code>" <> renderInlineHTML opt (HTML.RawText (normalizeCodeSpan t)) <> "</code>"
      | otherwise = renderCodeSpan t
    renderNode _ (Link url ch) _ = link "" url ch
    renderNode _ (Image source ch) _ = link "!" source ch
    renderNode _ SoftBreak _ = softBreak
    renderNode _ HardBreak _ = hardBreak
    renderNode _ (HTMLInline body) _ = renderInlineHTML opt body
    link prefix destination ch =
      prefix <> "[" <> renderInlineTextWithBreaks opt softBreak hardBreak ch <> "](<" <>
        escapeDestination destination <> ">)"
    -- Only use delimiter syntax where its boundaries are unambiguous.
    -- HTML handles whitespace, punctuation, nesting and touching emphasis.
    emphasized tag delimiter count before ch after
      | Just t <- plainContents ch,
        Just (first, _) <- T.uncons t, Just (_, lastChar) <- T.unsnoc t,
        isAlphaNum first && isAlphaNum lastChar,
        safeNeighbor delimiter T.unsnoc before,
        safeNeighbor delimiter (fmap swap . T.uncons) after =
          let marker = T.replicate count (delimiterText delimiter)
          in marker <> contents <> marker
      | otherwise = "<" <> tag <> ">" <> guardLine <> contents <> "</" <> tag <> ">"
      where
        contents = renderInlineTextWithBreaks opt softBreak hardBreak ch
        -- A tag-only opening line would start an HTML block in CommonMark.
        (opening, rest) = T.breakOn "\n" contents
        guardLine
          | not (T.null rest) && T.all (`elem` [' ', '\t']) opening = "<!-- -->"
          | otherwise = ""

-- Ignore absent text when inspecting neighbors, and join adjacent literal text
-- before normalizing line endings (a CRLF pair may span two Plain nodes).
normalizeInlines :: [Inline] -> [Inline]
normalizeInlines = mergePlain . filter visible
  where
    visible (Plain t) = not (T.null t)
    visible (HTMLInline (HTML.RawText t)) = not (T.null t)
    visible _ = True
    mergePlain [] = []
    mergePlain (Plain t : rest) =
      let (following, remaining) = span isPlain rest
      in Plain (T.concat (t : [text | Plain text <- following])) : mergePlain remaining
    mergePlain (node : rest) = node : mergePlain rest
    isPlain Plain{} = True
    isPlain _ = False

plainContents :: [Inline] -> Maybe Text
plainContents = fmap T.concat . traverse plain
  where
    plain (Plain t) = Just t
    plain _ = Nothing

safeNeighbor :: EmphasisDelimiter -> (Text -> Maybe (Text, Char)) -> Maybe Inline -> Bool
safeNeighbor _ _ Nothing = True
safeNeighbor delimiter boundary (Just (Plain t)) =
  case boundary t of
    Just (_, c) -> isSpace c || (delimiter == Asterisk && isAlphaNum c)
    Nothing -> True
safeNeighbor _ _ _ = False

adjacentCode :: Maybe Inline -> Bool
adjacentCode (Just (CodeSpan _)) = True
adjacentCode _ = False

delimiterText :: EmphasisDelimiter -> Text
delimiterText Asterisk = "*"
delimiterText Underscore = "_"

escapePlain :: Text -> Text
escapePlain = protectLineIndentation . T.replace " \n" "&#32;\n" . T.concatMap escapeChar
  where
    escapeChar c
      | isAscii c && (isPunctuation c || isSymbol c) = "\\" <> T.singleton c
      | otherwise = T.singleton c

escapeDestination :: Text -> Text
escapeDestination = T.concatMap escapeChar
  where
    escapeChar '&' = "&amp;"
    escapeChar c
      | ord c <= 32 || ord c == 127 || c `elem` ("<>\\\"" :: String) =
          let digits = T.toUpper (T.pack (showHex (ord c) ""))
          in "%" <> T.justifyRight 2 '0' digits
      | otherwise = T.singleton c

-- CommonMark trims space and tab at block edges and after source newlines.
-- Encoding the edge character protects the whole literal whitespace run.
protectLeadingWhitespace :: Text -> Text
protectLeadingWhitespace text = case T.uncons text of
  Just (' ', rest) -> "&#32;" <> rest
  Just ('\t', rest) -> "&#9;" <> rest
  _ -> text

protectTrailingWhitespace :: Text -> Text
protectTrailingWhitespace text = case T.unsnoc text of
  Just (rest, ' ') -> rest <> "&#32;"
  Just (rest, '\t') -> rest <> "&#9;"
  _ -> text

protectLineIndentation :: Text -> Text
protectLineIndentation text = case T.splitOn "\n" text of
  first : rest -> T.intercalate "\n" (first : fmap protectLeadingWhitespace rest)
  [] -> text

protectTrailingSpaces :: Text -> Text
protectTrailingSpaces contents =
  let prefix = T.dropWhileEnd (== ' ') contents
  in prefix <> T.replicate (T.length contents - T.length prefix) "&#32;"

-- Preserve HTML tags while protecting their text from Markdown interpretation.
-- Only the inline HTML constructors need recursive text handling here.
-- Protect indentation after HTML escaping, preserving the fragment's text
-- (including its configured formatting) without double-escaping entities.
renderInlineHTML :: CommonMarkRenderOptions -> HTML.HTMLBody -> Text
renderInlineHTML opt body = guardOpeningLine rendered
  where
    rendered = protectLineIndentation $ T.replace " \n" "&#32;\n" $ normalizeLineEndings $
      renderStrict (layoutPretty defaultLayoutOptions
        (HTML.renderHTMLFragment (htmlRenderOptions opt) [protectText body]))
    protectText (HTML.RawText t) = HTML.RawText (T.concatMap escapeChar t)
    protectText (HTML.TextFormat fmt attrs ch) = HTML.TextFormat fmt attrs (protectText <$> ch)
    protectText (HTML.Anchor url attrs ch) = HTML.Anchor url attrs (protectText <$> ch)
    protectText (HTML.Custom tag attrs ch) = HTML.Custom tag attrs (protectText <$> ch)
    protectText node = node
    -- These characters are already protected by HTML character references.
    escapeChar c
      | c `elem` ("<>&\"'" :: String) = T.singleton c
      | otherwise = escapePlain (T.singleton c)
    guardOpeningLine t =
      let (first, rest) = T.breakOn "\n" t
      in if ">" `T.isSuffixOf` T.stripEnd first && not ("<!--" `T.isPrefixOf` first) && not (T.null rest)
         then first <> "<!-- -->" <> rest
         else t

normalizeCodeSpan :: Text -> Text
normalizeCodeSpan = T.replace "\n" " " . normalizeLineEndings

-- | Delimit literal inline code, preserving spaces and embedded backticks.
-- Line endings become spaces as in CommonMark; an empty span uses HTML.
renderCodeSpan :: Text -> Text
renderCodeSpan input
  | T.null contents = "<code></code>"
  | otherwise = marker <> padding <> contents <> padding <> marker
  where
    contents = normalizeCodeSpan input
    marker = T.replicate (longestRun '`' contents + 1) "`"
    padding = if needsCodeSpanPadding contents then " " else ""

-- When both ends contain an ASCII space, CommonMark strips one from each end
-- unless the content is all spaces. Padding preserves those spaces and separates
-- boundary backticks from the delimiters.
-- See <https://spec.commonmark.org/0.31.2/#code-spans>.
needsCodeSpanPadding :: Text -> Bool
needsCodeSpanPadding contents =
  "`" `T.isPrefixOf` contents || "`" `T.isSuffixOf` contents ||
  (" " `T.isPrefixOf` contents && " " `T.isSuffixOf` contents &&
    not (T.all (== ' ') contents))

-- | Render a fenced code fragment.
-- Normalize line endings, retain trailing blank lines and add a final content
-- newline when needed. Collapse info-string whitespace to keep it on one line.
-- Use tildes if the info string contains a backtick.
-- See <https://spec.commonmark.org/0.31.2/#fenced-code-blocks>.
renderCodeFence :: CodeFenceStyle -> Maybe Text -> Text -> RenderedLines ann
renderCodeFence style info input = textToRenderedLines
  (fence <> infoPrefix <> escapeHTMLText (T.replace "\\" "\\\\" infoText) <> "\n" <> contents <> finalBreak <> fence)
  where
    contents = normalizeLineEndings input
    infoText = foldMap (T.unwords . T.words) info
    fenceChar = case style of
      TildeFence -> '~'
      BacktickFence
        | T.elem '`' infoText -> '~'
        | otherwise -> '`'
    infoPrefix = if T.singleton fenceChar `T.isPrefixOf` infoText then " " else ""
    fence = T.replicate (max 2 (longestRun fenceChar contents) + 1) (T.singleton fenceChar)
    finalBreak = if T.null contents || "\n" `T.isSuffixOf` contents then "" else "\n"

-- Choosing a delimiter longer than every run in the content prevents a
-- collision; fenced code additionally requires at least three markers.
longestRun :: Char -> Text -> Int
longestRun marker = maximum . (0 :) . fmap T.length . T.split (/= marker)
