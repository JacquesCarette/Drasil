-- | Defines functions to create accompanying .css files for HTML generators.
module Language.Drasil.HTML.CSS where

import Text.PrettyPrint (Doc, text, vcat)

import Language.Drasil hiding (Expr)

-- | Generates the CSS selectors necessary for a document.
makeCSS :: Document -> Doc  
makeCSS _ = vcat [
-- TODO: Autogenerate necessary css selectors only, make CSS configurable
  text "body {min-width: 400px; max-width: 1400px;}",
  text ".title {text-align: center;}",
  text ".author {text-align: center;}",
  text ".paragraph {text-align: justify;}",
  vcat [
    text ".cases {",
    text "  display: inline-block;",
    text "  vertical-align: middle;}"],
  vcat [
    text ".case {",
    text "  float: right;",
    text "  padding-left: 1em;}"],
  vcat [
    text ".cases > span {",
    text "  display: block;",
    text "  padding-top: 0.1em;",
    text "  padding-left: 0em;}"],
  vcat [
    text ".casebr {",
    text "  display: inline-block;",
    text "  vertical-align: middle;",
    text "  margin: 0 0.2em 0.4ex;",
    text "  text-align: center;",
    text "  font-size: 500%;}"],
  vcat [
    text ".caption {",
    text "  text-align: center;",
    text "  font-weight: bold;",
    text "  padding-bottom: 1%;",
    text "  line-height: 0;}"
    ],
  vcat [
    text ".fraction {",
    text "  display: inline-block;",
    text "  vertical-align: middle;",
    text "  margin: 0 0.2em 0.4ex;",
    text "  text-align: center;}"
    ],
  vcat [
    text ".fraction > span {",
    text "  display: block;",
    text "  padding-top: 0.15em;}"
    ],
  text ".fdn {border-top: thin solid black;}",
  vcat [
    text ".table {",
    text "  text-align: left;",
    text "  padding-left: 1%;",
    text "  width: 90%;",
    text "  margin-bottom: 2%;",
    text "  margin-top: 2%}"],
  vcat [
    text "table, th, td {",
    text "  border-collapse: collapse;",
    text "  margin-left: auto;",
    text "  margin-right: auto;}"],
  text "th, td {border: 1px solid black; padding: 0.5em;}",
  text ".tdefn, .ddefn, .gdefn, .idefn {width: 75%; margin-top: 1%; margin-bottom: 1%;}",
  text ".tdefn th, .ddefn th, .gdefn th, .idefn th {width: 15%;}",
  text ".section {width: 80%; margin: 0 auto; text-align: left;}",
  vcat [
    text ".code {",
    text "  display: inline-block;",
    text "  text-align: left;",
    text ("  font-family: Monaco, Consolas, \"Andale Mono\"," ++
      "\"DejaVu Sans Mono\", monospace;"),
    text "  font-size: 95%;",
    text "  line-height: 140%;",
    text "  white-space: pre;",
    text "  white-space: pre-wrap;",
    text "  white-space: -moz-pre-wrap;",
    text "  white-space: -o-pre-wrap;",
    text "  background: #faf8f0;}"],
  text ".list {text-align: left;}",
  vcat [
    text "figure {",
    text "text-align: center;",
    text "font-weight: bold;",
    text "}"],
  vcat [
    text "figure > img {",
    text "max-width: 100%;",
    text "}"],
  vcat [
    text ".matrix {",
    text "  position: relative;",
    text "  display: inline-table;",
    text "  margin: 10px;",
    text "  vertical-align: middle;}",
    text ".matrix:before, .matrix:after {",
    text "  content: \"\";",
    text "  position: absolute;",
    text "  top: 0;",
    text "  border: 1px solid #000;",
    text "  width: 5px;",
    text "  height: 100%;}",
    text ".matrix:before {",
    text "  left: -5px;",
    text "  border-right: 0px;}",
    text ".matrix:after {",
    text "  right: -5px;",
    text "  border-left: 0px;}",
    text ".matrix td {",
    text "  padding: 5px;",
    text "  text-align: center;",
    text "  border: 0px;}"],
  vcat [
    text "ul.hide-list-style {",
    text "  list-style-type: none;}"
    ],
  vcat [
    text "ul.hide-list-style-no-indent {",
    text "  list-style-type: none;",
    text "padding: 0;}"
    ] 
  ]

-- | Create the link to the necessary CSS file.
linkCSS :: String -> Doc  
linkCSS fn = 
  text $ "<link rel=\"stylesheet\" type=\"text/css\" href=\""++fn++".css\">"
