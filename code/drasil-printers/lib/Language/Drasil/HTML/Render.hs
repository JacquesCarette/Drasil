{-# LANGUAGE OverloadedStrings #-}
module Language.Drasil.HTML.Render(
  genHTML, HTMLGenOptions(..), defaultHTMLGO,
  renderHTML
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Extras (num2Text)

import Drasil.Data.Formats.HTML

import Language.Drasil.HTML.Citation (printBib)
import Language.Drasil.HTML.MathJax (mathJax3Url, mathJaxScript, blockEqn)
import Language.Drasil.HTML.Spec (printSpec, specToHTML)
import qualified Language.Drasil.Printing.AST as AST
import qualified Language.Drasil.Printing.LayoutObj as AST
import qualified Language.Drasil.TeX.Print as TeX (spec, printMath)

-- | Options for converting layout objects ('LayoutObj's) into HTML AST
newtype HTMLGenOptions = HTMLGO {mathJaxSrc :: Text}

-- | Default 'HTMLGenOptions' using the standard MathJax CDN URL.
defaultHTMLGO :: HTMLGenOptions
defaultHTMLGO = HTMLGO mathJax3Url

-- | Generate an HTML document from a Drasil 'Document'.
--   Arguments: Rendering options, CSS file name, `Document` to be rendered
genHTML :: HTMLGenOptions -> String -> AST.Document -> HTML
genHTML rOpts fn (AST.Document t a c) = HTML heads bodies
  where
    heads =
      [ stylesheet (T.pack fn),
        Title (printSpec t),
        Meta [attr "charset" "utf-8"],
        inlineScript mathJaxScript,
        externalScript
          (mathJaxSrc rOpts)
          [ id_ "MathJax-script",
            attr "async" ""
          ]
      ]
    bodies =
      [ articleTitle (specToHTML t),
        author (specToHTML a)
      ]
        ++ concatMap (loToHTML rOpts) c

-- | Internal: Creates the title block for the HTML document.
articleTitle :: [HTMLBody] -> HTMLBody
articleTitle t = Div [class_ ["title"]] [Heading H1 [] t]

-- | Internal: Creates the author block for the HTML document.
author :: [HTMLBody] -> HTMLBody
author a = Div [class_ ["author"]] [Heading H2 [] a]

-- | Internal: Transforms layout objects ('LayoutObj's) into HTML.
loToHTML :: HTMLGenOptions -> AST.LayoutObj -> [HTMLBody]
loToHTML _ (AST.EqnBlock contents) =
  [RawText $ blockEqn $ T.pack $ show $ TeX.printMath $ TeX.spec contents]
loToHTML rOpts (AST.HDiv ts layoutObs l) =
  let classAttr = [class_ (map T.pack ts) | not (null ts)]
      attrs = specToIdAttr l ++ classAttr
  in [Section attrs (concatMap (loToHTML rOpts) layoutObs)]
loToHTML _ (AST.Paragraph contents) = [Paragraph [class_ ["paragraph"]] (specToHTML contents)]
loToHTML _ (AST.Table ts rows r b t) = makeTableHTML ts rows r b t
loToHTML rOpts (AST.Definition ssPs l) = makeDefnHTML rOpts ssPs l
loToHTML _ (AST.Header n contents _) =
  case specToHTML contents of
    [] -> []
    ch -> [Heading (toHLevel n) [] ch]
loToHTML _ (AST.List t) = [buildListHtml t]
loToHTML _ (AST.Figure r c f wp) =
  [Div [id_ (printSpec r)] [figureImage [] attrs (T.pack f) captionText ("Figure: " <> captionText)]]
  where
    attrs = [attr "width" (num2Text wp <> "%") | wp /= 100]
    captionText = maybe mempty printSpec c
loToHTML _ (AST.Bib bib) = [printBib bib]
loToHTML _ AST.Graph {} = []
loToHTML _ AST.Cell {} = []
loToHTML _ AST.CodeBlock {} = []

-- | Internal: Generates an HTML table, called by 'loToHTML'.
makeTableHTML :: [String] -> [[AST.Spec]] -> AST.Spec -> Bool -> AST.Spec -> [HTMLBody]
makeTableHTML _ [] _ _ _ = error "No table to print (see Language.Drasil.HTML.Render)"
makeTableHTML ts (l : lls) r b t = [Div wrapperAttrs $ tableNode : [captionNode | b]]
  where
    attrs = [class_ (map T.pack ts)]
    headerRow = Row [] (map (THeader [] . specToHTML) l)
    dataRows = map (Row [] . map (TData [] . specToHTML)) lls
    tableNode = Table attrs (headerRow : dataRows)
    captionNode = Paragraph [class_ ["caption"]] (specToHTML t)
    wrapperAttrs = specToIdAttr r

-- | Internal: Generates definition tables.
makeDefnHTML :: HTMLGenOptions -> [(String, [AST.LayoutObj])] -> AST.Spec -> [HTMLBody]
makeDefnHTML _ [] _ = error "Empty definition"
makeDefnHTML rOpts ps l =
  let attrs = specToIdAttr l ++ [class_ ["defn-table"]]
      refRow = Row [] [THeader [] ["Refname"], TData []
        [bold_ (specToHTML l)]]
      dataRows = map ( \(f, d) -> Row [] [THeader [] [rawText' f],
        TData [] (concatMap (loToHTML rOpts) d)]) ps
   in [Table attrs (refRow : dataRows)]

-- | Internal: Generates lists in HTML.
buildListHtml :: AST.ListType -> HTMLBody
buildListHtml (AST.Simple items) = Div [class_ ["list"]] $
  map (\(b, e, l) -> Paragraph (mbIdAttr l)
  (specToHTML b ++ [": "] ++ itemToHTML e)) items
buildListHtml (AST.Desc items) = Div [class_ ["list"]] $
  map (\(b, e, l) -> Paragraph (mbIdAttr l)
  ([bold_ (specToHTML b), ": "] ++ itemToHTML e)) items
buildListHtml (AST.Ordered items) = List Ordered [class_ ["list"]] $ map mkLItem items
buildListHtml (AST.Unordered items) = List Unordered [class_ ["list"]] $ map mkLItem items
buildListHtml (AST.Definitions items) = List Unordered [class_ ["hide-list-style-no-indent"]] $
  map (\(b, e, l) -> LItem (mbIdAttr l) (specToHTML b ++ [" is the "] ++ itemToHTML e)) items

-- | Internal: Helper to create list items.
mkLItem :: (AST.ItemType, Maybe AST.Spec) -> LItem
mkLItem (i, l) = LItem (mbIdAttr l) (itemToHTML i)

-- | Internal: Converts a 'Spec' label into an ID 'Attr' list, omitting empty labels.
specToIdAttr :: AST.Spec -> [Attr]
specToIdAttr AST.EmptyS = []
specToIdAttr s          = [id_ (printSpec s)]

-- | Internal: Convert @Maybe Spec@s into ID `Attr`s if the `Spec` exists.
mbIdAttr :: Maybe AST.Spec -> [Attr]
mbIdAttr = maybe [] specToIdAttr

-- | Internal: Generates list items.
itemToHTML :: AST.ItemType -> [HTMLBody]
itemToHTML (AST.Flat s)     = specToHTML s
itemToHTML (AST.Nested s l) = specToHTML s ++ [buildListHtml l]
