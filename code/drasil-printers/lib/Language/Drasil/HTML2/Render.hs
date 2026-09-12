{-# LANGUAGE OverloadedStrings #-}
-- | Defines all functions needed to print HTML files. For more information on each of the helper functions, please view the [source files](https://jacquescarette.github.io/Drasil/docs/full/drasil-printers-0.1.10.0/src/Language.Drasil.HTML.Print.html).
module Language.Drasil.HTML2.Render(
  genHTML, HTMLGenOptions(..), defaultHTMLGO,
  renderHTML
) where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Drasil.Printing.AST (ItemType(Flat, Nested),
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Spec(EmptyS))
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import Language.Drasil.Printing.Helpers (sqbrac)

import qualified Language.Drasil.TeX.Print as TeX (spec)

import Language.Drasil.HTML2.Citation (printBib)
import Language.Drasil.HTML2.Spec (printSpec, specToHTML, articleTitle, author)
import Language.Drasil.Markdown.Print (printMath)

import Drasil.Data.Formats.JSON (renderJSON, jsonRenderOpts,
  JSONStyle(..), JSON(..))
import Drasil.Data.Formats.HTML hiding (Title, Row, Bold, ListType, Ordered,
  Unordered, span, Paragraph, Table, List, Figure)
import qualified Drasil.Data.Formats.HTML as HTML

-- | Options for converting layout objects ('LayoutObj's) into HTML AST
newtype HTMLGenOptions = HTMLGO {mathJaxSrc :: String}

-- | Default 'HTMLGenOptions' using the standard MathJax CDN URL.
defaultHTMLGO :: HTMLGenOptions
defaultHTMLGO = HTMLGO "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"

-- | Generate an HTML document from a Drasil 'Document'.
--   Arguments: Rendering options, Bib rendering options, CSS file name, `Document` to be rendered
genHTML :: HTMLGenOptions -> String -> Document -> HTML
genHTML rOpts fn (Document t a c) = HTML heads bodies
  where
    heads =
      [ stylesheet (T.pack fn),
        HTML.Title (printSpec t),
        Meta [attr "charset" "utf-8"],
        inlineScript mathJaxScript,
        externalScript
          (T.pack $ mathJaxSrc rOpts)
          [ id_ "MathJax-script",
            attr "async" ""
          ]
      ]
    bodies =
      [ articleTitle (specToHTML t),
        author (specToHTML a)
      ]
        ++ concatMap (loToHTML rOpts) c

-- | Variable to include MathJax in our HTML files so we can render equations in LaTeX.
mathJaxScript :: Text
mathJaxScript = "MathJax = " <> configJSON <> ";"
  where
    configJSON = T.pack $ show $ renderJSON
      (jsonRenderOpts (Pretty 2))
      ( JObject [
        ("loader",
        JObject [("load", JArray ["[tex]/textmacros", "output/chtml"])]),
        ("tex", JObject [("packages", JObject [("[+]", JArray ["textmacros"])])]),
        ("svg", JObject [("fontCache", "global")])]
      )

-- | Transforms layout objects ('LayoutObj's) into HTML.
loToHTML :: HTMLGenOptions -> LayoutObj -> [HTMLBody]
-- Creates delimeters to be used for mathjax displayed equations
-- Latex print sets up a \begin{displaymath} environment instead of this
loToHTML _ (EqnBlock contents) =
  [RawText ( T.pack ("\\" <> sqbrac ( show (printMath $ TeX.spec contents) <> "\\")))]
-- Non-mathjax
loToHTML rOpts (HDiv ts layoutObs l) =
  let idAttr = case l of
                 EmptyS -> []
                 _      -> [id_ (printSpec l)]
      classAttr =  ([class_ (T.unwords $ map T.pack ts) | not (null ts)])
      attrs = idAttr ++ classAttr
  in [Section attrs (concatMap (loToHTML rOpts) layoutObs)]
loToHTML _ (Paragraph contents) = [HTML.Paragraph [class_ "paragraph"] (specToHTML contents)]
loToHTML _ (Table ts rows r b t) = makeTableHTML ts rows r b t
loToHTML rOpts (Definition ssPs l) = makeDefnHTML rOpts ssPs l
loToHTML _ (Header n contents _) =
  case specToHTML contents of
    [] -> []
    ch -> [Heading (toHLevel n) [] ch]
  where
    toHLevel 0 = H1
    toHLevel 1 = H2
    toHLevel 2 = H3
    toHLevel 3 = H4
    toHLevel 4 = H5
    toHLevel _ = H6
loToHTML _ (List t) = [buildListHtml t]
loToHTML _ (Figure r c f wp) =
  [HTML.Div [id_ (printSpec r)] [figureImage [] attrs (T.pack f) captionText ("Figure: " <> captionText)]]
  where
    attrs = [attr "width" (T.pack $ show wp ++ "%") | wp /= 100]
    captionText = maybe mempty printSpec c
loToHTML _ (Bib bib) = [printBib bib]
loToHTML _ Graph {} = []
loToHTML _ Cell {} = []
loToHTML _ CodeBlock {} = []

-- | Generates an HTML table, called by 'printLO'.
makeTableHTML :: [String] -> [[Spec]] -> Spec -> Bool -> Spec -> [HTMLBody]
makeTableHTML _ [] _ _ _ = error "No table to print (see Language.Drasil.HTML2.Render)"
makeTableHTML ts (l : lls) r b t =
  if b
    then [HTML.Div wrapperAttrs [tableNode, captionNode]]
    else [HTML.Div wrapperAttrs [tableNode]]
  where
    attrs = [class_ (T.unwords $ map T.pack ts)]
    headerRow = HTML.Row [] (map (THeader [] . specToHTML) l)
    dataRows = map (HTML.Row [] . map (TData [] . specToHTML)) lls
    tableNode = HTML.Table attrs (headerRow : dataRows)
    captionNode = HTML.Paragraph [class_ "caption"] (specToHTML t)
    wrapperAttrs = [id_ (printSpec r)]

-- | Generates definition tables.
makeDefnHTML :: HTMLGenOptions -> [(String, [LayoutObj])] -> Spec -> [HTMLBody]
makeDefnHTML _ [] _ = error "Empty definition"
makeDefnHTML rOpts ps l =
  let attrs = [id_ (printSpec l), class_ "defn-table"]
      refRow = HTML.Row [] [THeader [] [rawText "Refname"], TData []
        [TextFormat HTML.Bold [] (specToHTML l)]]
      dataRows = map ( \(f, d) -> HTML.Row [] [THeader [] [rawText' f],
        TData [] (concatMap (loToHTML rOpts) d)]) ps
   in [HTML.Table attrs (refRow : dataRows)]

-- | Generates lists in HTML.
buildListHtml :: ListType -> HTMLBody
buildListHtml (Simple items) = HTML.Div [class_ "list"] $
  map (\(b, e, l) -> HTML.Paragraph (mbIdAttr l)
  (specToHTML b ++ [rawText ": "] ++ itemToHTML e)) items
buildListHtml (Desc items) = HTML.Div [class_ "list"] $
  map (\(b, e, l) -> HTML.Paragraph (mbIdAttr l)
  ([TextFormat HTML.Bold [] (specToHTML b), rawText ": "] ++ itemToHTML e)) items
buildListHtml (Ordered items) = HTML.List HTML.Ordered [class_ "list"] $
  map (\(i, l) -> LItem (mbIdAttr l) (itemToHTML i)) items
buildListHtml (Unordered items) = HTML.List HTML.Unordered [class_ "list"] $
  map (\(i, l) -> LItem (mbIdAttr l) (itemToHTML i)) items
buildListHtml (Definitions items) = HTML.List HTML.Unordered [class_ "hide-list-style-no-indent"] $
  map (\(b, e, l) -> LItem (mbIdAttr l) (specToHTML b ++ [rawText " is the "] ++ itemToHTML e)) items

-- | Convert @Maybe Spec@s into ID `Attr`s if the `Spec` exists.
mbIdAttr :: Maybe Spec -> [Attr]
mbIdAttr = maybe [] (\x -> [id_ $ printSpec x])

-- | Generates list items.
itemToHTML :: ItemType -> [HTMLBody]
itemToHTML (Flat s)     = specToHTML s
itemToHTML (Nested s l) = specToHTML s ++ [buildListHtml l]
