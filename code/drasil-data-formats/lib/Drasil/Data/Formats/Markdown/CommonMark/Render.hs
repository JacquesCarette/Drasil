{-# LANGUAGE OverloadedStrings #-}

-- | Render the supported CommonMark subset, including structured HTML fallbacks.
module Drasil.Data.Formats.Markdown.CommonMark.Render
  ( EmphasisDelimiter (..),
    CodeFenceStyle (..),
    CommonMarkRenderOptions (..),
    defaultCommonMarkRO,
    renderCommonMark,
  )
where

import Data.Char (isAlphaNum, isAscii, isPunctuation, isSpace, isSymbol, ord)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe)
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
import Text.Regex.TDFA (Regex, makeRegex, matchTest)

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
-- leading and trailing whitespace is stripped.
renderCommonMark :: CommonMarkRenderOptions -> CommonMark -> Doc ann
renderCommonMark opt (CommonMark blocks) = linesToDoc (renderBlocks opt blocks)

renderBlocks :: CommonMarkRenderOptions -> [Block] -> [Text]
renderBlocks opt = snd . renderBlocksWithFirst opt

-- Inspect the first visible source block without rendering it a second time.
renderBlocksWithFirst :: CommonMarkRenderOptions -> [Block] -> (Maybe Block, [Text])
renderBlocksWithFirst opt blocks =
  (fst <$> listToMaybe rendered, separateBlocks (separateLists rendered))
  where
    rendered = mapMaybe render blocks
    render block = fmap (block,) (renderBlock opt block)
    separateLists [] = []
    separateLists [(_, contents)] = [contents]
    separateLists ((block, contents) : remaining@((next, _) : _)) =
      contents : [["<!-- -->"] | sameList block next] <> separateLists remaining
    sameList (OrderedList _) (OrderedList _) = True
    sameList (UnorderedList _) (UnorderedList _) = True
    sameList _ _ = False

renderBlock :: CommonMarkRenderOptions -> Block -> Maybe [Text]
renderBlock opt (Heading level contents) =
  Just [marker <> if T.null text then mempty else " " <> text]
  where
    marker = T.replicate (fromEnum level + 1) "#"
    -- Inline HTML can introduce layout newlines even in a validated heading.
    text = T.strip $ T.replace "\n" " " $ renderInlinesWith opt id contents
renderBlock opt (Paragraph contents)
  | all paragraphWhitespace (normalizeInlines contents) || null rendered = Nothing
  | otherwise = Just rendered
  where
    rendered = protectStart <$> trimLines (T.splitOn "\n" (renderParagraph opt contents))
    trimLines = reverse . dropWhile T.null . reverse . dropWhile T.null
    protectStart original = case T.uncons unindented of
      -- A comment can interrupt a paragraph even when another tag follows it.
      Just ('<', _) | T.isPrefixOf "<!--" unindented -> indentation <> "<span></span>" <> unindented
      -- A standalone inline tag must not open a CommonMark HTML block.
      Just ('<', _) | not (T.null tagEnd) && T.null (T.strip (T.drop 1 tagEnd)) ->
        line <> "<!-- -->"
      _ -> line
      where
        line = protectCodeIndentation original
        (indentation, unindented) = T.span (== ' ') line
        (_, tagEnd) = T.breakOn ">" unindented
renderBlock opt (BlockQuote blocks) = Just (prefixBlockQuote (renderBlocks opt blocks))
renderBlock opt (CodeBlock info contents) = Just (renderCodeFence (codeFenceStyle opt) info contents)
renderBlock opt (OrderedList items)
  | null items = Nothing
  | otherwise = Just (renderList opt [T.pack (show n) <> "." | n <- [1 :: Int ..]] items)
renderBlock opt (UnorderedList items)
  | null items = Nothing
  | otherwise = Just (renderList opt (repeat "-") items)
renderBlock _ ThematicBreak = Just ["---"]
renderBlock opt (HTMLBlock bodies)
  | null nonemptyBodies = Nothing
  | otherwise = Just (protectHTMLBlankLines rendered)
  where
    nonemptyBodies = filter nonemptyHTML bodies
    blockBodies = case nonemptyBodies of
      first : _ | startsHTMLBlock first -> nonemptyBodies
      _ -> [HTML.Div [] nonemptyBodies]
    rendered = renderHTMLText opt blockBodies

nonemptyHTML :: HTML.HTMLBody -> Bool
nonemptyHTML (HTML.RawText text) = not (T.null text)
nonemptyHTML _ = True

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
protectHTMLBlankLines :: Text -> [Text]
protectHTMLBlankLines input
  | T.null input = [input]
  | otherwise = protect <$> T.splitOn "\n" (normalizeLineEndings input)
  where
    protect line | T.all (\c -> c == ' ' || c == '\t') line = line <> "<!-- -->"
                 | otherwise = line

renderList :: CommonMarkRenderOptions -> [Text] -> [ListItem] -> [Text]
renderList opt markers = separateBlocks . zipWith renderItem markers
  where
    -- A marker-only line avoids thematic-break priority for @- ---@ and
    -- for nested empty unordered lists such as @- - -@.
    startsWithDashes ThematicBreak = True
    startsWithDashes (UnorderedList _) = True
    startsWithDashes _ = False
    renderItem marker (ListItem blocks) = case firstBlock of
      Nothing -> [marker]
      Just first -> case contents of
        [] -> [marker]
        ls@(opening : rest)
          | startsWithDashes first -> marker : fmap indentLine ls
          | otherwise -> (marker <> " " <> opening) : fmap indentLine rest
      where
        (firstBlock, contents) = renderBlocksWithFirst opt blocks
        padding = T.replicate (T.length marker + 1) " "
        indentLine line | T.null line = ""
                        | otherwise = padding <> line

-- A final backslash-newline is not a CommonMark hard break at a block boundary.
-- Render only boundary hard breaks with HTML; interior breaks keep their usual
-- Markdown syntax. Empty inline nodes must not hide those boundaries.
renderParagraph :: CommonMarkRenderOptions -> [Inline] -> Text
renderParagraph opt contents =
  mconcat prefix <> renderInlines opt middle <> mconcat suffix
  where
    nodes = normalizeInlines contents
    (prefix, afterStart) = leading nodes
    (suffix, reversedMiddle) = leading (reverse afterStart)
    middle = reverse reversedMiddle
    leading (SoftBreak : remaining) = leading remaining
    leading (HardBreak : remaining) =
      let (breaks, rest) = leading remaining
      in ("<br />" : breaks, if all paragraphWhitespace rest then [] else rest)
    leading (node@Plain{} : remaining)
      | paragraphWhitespace node =
          let (breaks, rest) = leading remaining
          in if null breaks then ([], node : rest) else (breaks, rest)
    leading remaining = ([], remaining)

paragraphWhitespace :: Inline -> Bool
paragraphWhitespace (Plain text) = T.all isSpace text
paragraphWhitespace SoftBreak = True
paragraphWhitespace _ = False

normalizeLineEndings :: Text -> Text
normalizeLineEndings = T.replace "\r" "\n" . T.replace "\r\n" "\n"

-- Fixed physical lines must survive an enclosing prettyprinter 'group'.
linesToDoc :: [Text] -> Doc ann
linesToDoc = concatWith (surround hardline) . fmap pretty

-- Only the external HTML renderer introduces layout decisions before the final
-- CommonMark document. Materialize that output once at this format boundary.
renderHTMLText :: CommonMarkRenderOptions -> [HTML.HTMLBody] -> Text
renderHTMLText opt = renderStrict . layoutPretty defaultLayoutOptions .
  HTML.renderHTMLFragment (htmlRenderOptions opt)

-- A quote needs a marker on every line, including blank and embedded HTML lines.
prefixBlockQuote :: [Text] -> [Text]
prefixBlockQuote [] = [">"]
prefixBlockQuote ls = fmap prefixLine ls
  where
    prefixLine line | T.null line = ">"
                    | otherwise = "> " <> line

separateBlocks :: [[Text]] -> [Text]
separateBlocks = intercalate [""]

renderInlines :: CommonMarkRenderOptions -> [Inline] -> Text
renderInlines opt = renderInlinesWith opt
  (protectLeadingWhitespace . protectTrailingWhitespace)

-- Compose inline nodes without block separators. Inspect neighbours to avoid
-- accidental delimiter runs or hard breaks. The supplied function controls
-- whitespace protection at embedded HTML edges; headings use id and trim
-- their final contents at block level.
renderInlinesWith :: CommonMarkRenderOptions -> (Text -> Text) -> [Inline] -> Text
renderInlinesWith opt protectHTMLEdges = mconcat . go Nothing False . normalizeInlines
  where
    go _ _ [] = []
    go previous startsLine (node : remaining) =
      renderNode previous startsLine node (listToMaybe remaining) :
        go (Just node) (endsWithNewline node) remaining
    prepareText startsLine next =
      protectNextBreak next . if startsLine then protectCodeIndentation else id
    protectNextBreak next
      | any startsWithNewline next = protectTrailingSpaces
      | otherwise = id
    renderNode _ startsLine (Plain text) next =
      prepareText startsLine next (escapePlain (normalizeLineEndings text))
    renderNode before _ (Emphasis ch) after =
      emphasized "em" (emphasisDelimiter opt) 1 before ch after
    renderNode before _ (Strong ch) after =
      emphasized "strong" (strongDelimiter opt) 2 before ch after
    renderNode before _ (CodeSpan text) after
      | any adjacentCode [before, after] =
          "<code>" <> protectLeadingWhitespace (protectTrailingWhitespace
            (renderInlineHTML opt (HTML.RawText (normalizeCodeSpan text)))) <> "</code>"
      | otherwise = renderCodeSpan text
    renderNode _ _ (Link url ch) _ = link "" url ch
    renderNode _ _ (Image source ch) _ = link "!" source ch
    renderNode _ _ SoftBreak _ = "\n"
    renderNode _ _ HardBreak _ = "\\\n"
    renderNode _ startsLine (HTMLInline body) next =
      prepareText startsLine next (protectHTMLEdges (renderInlineHTML opt body))
    link prefix destination ch =
      prefix <> "[" <> renderInlinesWith opt protectHTMLEdges ch <> "](<" <>
        escapeDestination destination <> ">)"
    -- Use Markdown delimiters only for literal contents whose edges and
    -- neighbours pass these conservative checks. Otherwise HTML keeps formatting
    -- boundaries explicit, including nested and adjacent emphasis. For example,
    -- adjacent Emphasis nodes for "a" and "b" emit <em>a</em><em>b</em>.
    emphasized tag delimiter count before ch after
      | Just text <- plainContents ch,
        Just (first, _) <- T.uncons text, Just (_, lastChar) <- T.unsnoc text,
        isAlphaNum first && isAlphaNum lastChar,
        safeNeighbor delimiter T.unsnoc before,
        safeNeighbor delimiter (fmap swap . T.uncons) after =
          let marker = T.replicate count (delimiterText delimiter)
          in marker <> contents <> marker
      | otherwise = "<" <> tag <> ">" <> guardLine <> contents <> "</" <> tag <> ">"
      where
        contents = renderInlinesWith opt protectHTMLEdges ch
        -- Keep a tag-only opening line from starting an HTML block.
        guardLine | openingNeedsGuard (normalizeInlines ch) = "<!-- -->"
                  | otherwise = mempty
    -- Only literal text and a soft break can leave the opening tag alone on
    -- its line. Inspect those leaves without laying out the composed inlines.
    openingNeedsGuard [] = False
    openingNeedsGuard (SoftBreak : _) = True
    openingNeedsGuard (Plain text : rest) =
      checkOpening (protectNextBreak (listToMaybe rest) (escapePlain (normalizeLineEndings text))) rest
    openingNeedsGuard (HTMLInline body@HTML.RawText{} : rest) =
      checkOpening (protectNextBreak (listToMaybe rest) (protectHTMLEdges (renderInlineHTML opt body))) rest
    openingNeedsGuard _ = False
    checkOpening text remaining =
      let (opening, rest) = T.breakOn "\n" text
      in T.all (\c -> c == ' ' || c == '\t') opening &&
        (not (T.null rest) || openingNeedsGuard remaining)

-- Formatting and link delimiters enclose their contents, so only these leaves
-- can put an adjacent node at the beginning or end of a physical source line.
startsWithNewline :: Inline -> Bool
startsWithNewline SoftBreak = True
startsWithNewline (Plain text) = any (isLineEnding . fst) (T.uncons text)
startsWithNewline (HTMLInline (HTML.RawText text)) =
  any (isLineEnding . fst) (T.uncons text)
startsWithNewline _ = False

endsWithNewline :: Inline -> Bool
endsWithNewline SoftBreak = True
endsWithNewline HardBreak = True
endsWithNewline (Plain text) = any (isLineEnding . snd) (T.unsnoc text)
endsWithNewline (HTMLInline (HTML.RawText text)) =
  any (isLineEnding . snd) (T.unsnoc text)
endsWithNewline _ = False

isLineEnding :: Char -> Bool
isLineEnding c = c == '\r' || c == '\n'

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
escapePlain = protectLineIndentation protectCodeIndentation . T.replace " \n" "&#32;\n" . T.concatMap escapeChar
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

-- Four columns of indentation can start an indented code block. Encode the
-- first leading space or tab as a character reference so paragraph text is
-- not parsed as code: "    x" becomes "&#32;   x". One to three leading
-- spaces need no such guard.
-- See <https://spec.commonmark.org/0.31.2/#indented-code-blocks>.
protectCodeIndentation :: Text -> Text
protectCodeIndentation text
  | T.length indentation >= 4 || T.any (== '\t') indentation = protectLeadingWhitespace text
  | otherwise = text
  where indentation = T.takeWhile (`elem` [' ', '\t']) text

-- Preserve literal HTML whitespace at fragment edges and after newlines.
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

protectLineIndentation :: (Text -> Text) -> Text -> Text
protectLineIndentation protect text = case T.splitOn "\n" text of
  first : rest -> T.intercalate "\n" (first : fmap protect rest)
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
    rendered = protectLineIndentation protectLeadingWhitespace $ T.replace " \n" "&#32;\n" $ normalizeLineEndings $
      renderHTMLText opt [protectText body]
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
    padding = if needsCodeSpanPadding contents then " " else mempty

-- When both ends contain an ASCII space, CommonMark strips one from each end
-- unless the content is all spaces. Padding preserves those spaces and separates
-- boundary backticks from the delimiters.
-- See <https://spec.commonmark.org/0.31.2/#code-spans>.
needsCodeSpanPadding :: Text -> Bool
needsCodeSpanPadding = matchTest codeSpanPaddingRegex

-- The final alternative matches spaces at both ends with a non-space inside.
-- For " x ", add one space at each end because CommonMark removes one from
-- each side; all-space contents are exempt from that rule.
codeSpanPaddingRegex :: Regex
codeSpanPaddingRegex = makeRegex ("^`|`$|^ .*[^ ].* $" :: Text)

-- | Render a fenced code fragment.
-- Normalize line endings, retain trailing blank lines and add a final content
-- newline when needed. Collapse info-string whitespace to keep it on one line.
-- Use tildes if the info string contains a backtick.
-- See <https://spec.commonmark.org/0.31.2/#fenced-code-blocks>.
renderCodeFence :: CodeFenceStyle -> Maybe Text -> Text -> [Text]
renderCodeFence style info input = T.splitOn "\n"
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
    fence = T.replicate (longestRun fenceChar contents + 1) (T.singleton fenceChar)
    finalBreak = if T.null contents || "\n" `T.isSuffixOf` contents then "" else "\n"

-- Choosing a delimiter longer than every run in the content prevents a
-- collision; fenced code additionally requires at least three markers.
longestRun :: Char -> Text -> Int
longestRun marker = maximum . (2 :) . fmap T.length . T.split (/= marker)
