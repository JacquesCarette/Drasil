{-# LANGUAGE OverloadedStrings #-}

-- | Defines all functions needed to print HTML files. For more information on each of the helper functions, please view the [source files](https://jacquescarette.github.io/Drasil/docs/full/drasil-printers-0.1.10.0/src/Language.Drasil.HTML.Print.html).
module Language.Drasil.HTML2.Render(
  genHTML, BibFormatter(..), htmlBibFormatter, HTMLGenOptions(..),
  renderHTML
) where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Drasil.Printing.AST (ItemType(Flat, Nested),
  ListType(Ordered, Unordered, Definitions, Desc, Simple), Spec(EmptyS))
import Language.Drasil.Printing.LayoutObj (Document(Document), LayoutObj(..))
import Language.Drasil.Printing.Helpers (sqbrac)

import qualified Language.Drasil.TeX.Print as TeX (spec)

import Language.Drasil.HTML2.Citation (BibFormatter(..), printBib, htmlBibFormatter)
import Language.Drasil.HTML2.Spec (printSpec, specToHTML)
import Language.Drasil.Markdown.Print (printMath)

import Drasil.Data.Formats.JSON (JSON(..), renderJSON, jsonRenderOpts, JSONStyle(Pretty))
import Drasil.Data.Formats.HTML hiding (Title, Row, Bold, ListType, Ordered,
  Unordered, span, Paragraph, Table, List, Figure)
import qualified Drasil.Data.Formats.HTML as HTML

-- | Options for converting layout objects ('LayoutObj's) into HTML AST
data HTMLGenOptions = HTMLGO
  { -- | Formatting rules for Bib
    bibFmt :: BibFormatter,
    -- | MathJax source URL
    mathJaxSrc :: String
  }

-- | Generate an HTML document from a Drasil 'Document'.
--   Arguments: Rendering options, Bib rendering options, CSS file name, `Document` to be rendered
genHTML :: HTMLGenOptions -> String -> Document -> HTML
genHTML rOpts fn (Document t a c) = HTML heads bodies
  where
    heads =
      [ stylesheet (T.pack fn),
        HTML.Title (printSpec t),
        Meta [Attr "charset" "utf-8"],
        inlineScript mathJaxScript,
        externalScript
          (T.pack $ mathJaxSrc rOpts)
          [ Attr "id" "MathJax-script",
            Attr "async" ""
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
                 _      -> [Attr "id" (printSpec l)]
      classAttr =  ([Attr "class" (T.unwords $ map T.pack ts) | not (null ts)])
      attrs = idAttr ++ classAttr
  in [Section attrs (concatMap (loToHTML rOpts) layoutObs)]
loToHTML _ (Paragraph contents) = [HTML.Paragraph [Attr "class" "paragraph"] (specToHTML contents)]
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
  [HTML.Div [Attr "id" (printSpec r)] [figureImage [] attrs (T.pack f) captionText ("Figure: " <> captionText)]]
  where
    attrs = [Attr "width" (T.pack $ show wp ++ "%") | wp /= 100]
    captionText = maybe "" printSpec c
loToHTML rOpts (Bib bib) = [printBib (bibFmt rOpts) bib]
loToHTML _ Graph {} = []
loToHTML _ Cell {} = []
loToHTML _ CodeBlock {} = []

-- | Generates an HTML table, called by 'printLO'.
makeTableHTML :: [String] -> [[Spec]] -> Spec -> Bool -> Spec -> [HTMLBody]
makeTableHTML _ [] _ _ _ = error "No table to print (see PrintHTML)"
makeTableHTML ts (l : lls) r b t =
  if b
    then [HTML.Div wrapperAttrs [tableNode, captionNode]]
    else [HTML.Div wrapperAttrs [tableNode]]
  where
    attrs = [Attr "class" (T.unwords $ map T.pack ts)]
    headerRow = HTML.Row [] (map (THeader [] . specToHTML) l)
    dataRows = map (HTML.Row [] . map (TData [] . specToHTML)) lls
    tableNode = HTML.Table attrs (headerRow : dataRows)
    captionNode = HTML.Paragraph [Attr "class" "caption"] (specToHTML t)
    wrapperAttrs = [Attr "id" (printSpec r)]

-----------------------------------------------------------------
------------------BEGIN DEFINITION PRINTING----------------------
-----------------------------------------------------------------

-- | Generates definition tables.
makeDefnHTML :: HTMLGenOptions -> [(String, [LayoutObj])] -> Spec -> [HTMLBody]
makeDefnHTML _ [] _ = error "Empty definition"
makeDefnHTML rOpts ps l =
  let attrs = [Attr "id" (printSpec l), Attr "class" "defn-table"]
      refRow = HTML.Row [] [THeader [] [RawText "Refname"], TData []
        [TextFormat HTML.Bold [] (specToHTML l)]]
      dataRows = map ( \(f, d) -> HTML.Row [] [THeader [] [RawText (T.pack f)],
        TData [] (concatMap (loToHTML rOpts) d)]) ps
   in [HTML.Table attrs (refRow : dataRows)]

-----------------------------------------------------------------
------------------BEGIN LIST PRINTING----------------------------
-----------------------------------------------------------------

-- | Generates lists in HTML.
buildListHtml :: ListType -> HTMLBody -- FIXME: ref id's should be folded into the li
buildListHtml (Simple items) = HTML.Div [Attr "class" "list"] $
  map (\(b, e, l) -> HTML.Paragraph (mbIdAttr l)
  (specToHTML b ++ [RawText ": "] ++ itemToHTML e)) items
buildListHtml (Desc items) = HTML.Div [Attr "class" "list"] $
  map (\(b, e, l) -> HTML.Paragraph (mbIdAttr l)
  ([TextFormat HTML.Bold [] (specToHTML b), RawText ": "] ++ itemToHTML e)) items
buildListHtml (Ordered items) = HTML.List HTML.Ordered [Attr "class" "list"] $
  map (\(i, l) -> LItem (mbIdAttr l) (itemToHTML i)) items
buildListHtml (Unordered items) = HTML.List HTML.Unordered [Attr "class" "list"] $
  map (\(i, l) -> LItem (mbIdAttr l) (itemToHTML i)) items
buildListHtml (Definitions items) = HTML.List HTML.Unordered [Attr "class" "hide-list-style-no-indent"] $
  map (\(b, e, l) -> LItem (mbIdAttr l) (specToHTML b ++ [RawText " is the "] ++ itemToHTML e)) items

-- | Convert @Maybe Spec@s int ID `Attr`s if the `Spec` exists.
mbIdAttr :: Maybe Spec -> [Attr]
mbIdAttr = maybe [] (\x -> [Attr "id" $ printSpec x])

-- | Generates list items.
itemToHTML :: ItemType -> [HTMLBody]
itemToHTML (Flat s)     = specToHTML s
itemToHTML (Nested s l) = specToHTML s ++ [buildListHtml l]
