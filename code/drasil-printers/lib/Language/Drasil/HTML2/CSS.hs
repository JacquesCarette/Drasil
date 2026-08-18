{-# LANGUAGE OverloadedStrings #-}
-- | Defines functions to create accompanying .css files for HTML generators.
module Language.Drasil.HTML2.CSS (
  genericCSS
) where

import Prettyprinter (Doc, vcat)

-- | Generic CSS used for stylizing the 'LayoutObj' language when outputted in
-- HTML format.
genericCSS :: Doc ann
genericCSS = vcat [
-- TODO: Autogenerate necessary css selectors only, make CSS configurable
  "body {min-width: 400px; max-width: 1400px;}",
  ".title {text-align: center;}",
  ".author {text-align: center;}",
  ".paragraph {text-align: justify;}",
  vcat [
     ".cases {",
    "  display: inline-block;",
    "  vertical-align: middle;}"],
  vcat [
    ".case {",
    "  float: right;",
    "  padding-left: 1em;}"],
  vcat [
    ".cases > span {",
    "  display: block;",
    "  padding-top: 0.1em;",
    "  padding-left: 0em;}"],
  vcat [
    ".casebr {",
    "  display: inline-block;",
    "  vertical-align: middle;",
    "  margin: 0 0.2em 0.4ex;",
    "  text-align: center;",
    "  font-size: 500%;}"],
  vcat [
    ".caption {",
    "  text-align: center;",
    "  font-weight: bold;",
    "  padding-bottom: 1%;",
    "  line-height: 0;}"
    ],
  vcat [
    ".fraction {",
    "  display: inline-block;",
    "  vertical-align: middle;",
    "  margin: 0 0.2em 0.4ex;",
    "  text-align: center;}"
    ],
  vcat [
    ".fraction > span {",
    "  display: block;",
    "  padding-top: 0.15em;}"
    ],
  ".fdn {border-top: thin solid black;}",
  vcat [
    ".table {",
    "  text-align: left;",
    "  padding-left: 1%;",
    "  width: 90%;",
    "  margin-bottom: 2%;",
    "  margin-top: 2%}"],
  vcat [
    "table, th, td {",
    "  border-collapse: collapse;",
    "  margin-left: auto;",
    "  margin-right: auto;}"],
  "th, td {border: 1px solid black; padding: 0.5em;}",
  ".defn-table {width: 75%; margin-top: 1%; margin-bottom: 1%;}",
  ".defn-table th {width: 15%;}",
  vcat [
  "section {width: 80%; margin: 0 auto; text-align: left;}",
  "section > section {width: 100%}",
  "section h1 { font-size: 2em; }",
  "section h2 { font-size: 1.5em; }",
  "section h3 { font-size: 1.17em; }"],
  vcat [
    ".code {",
    "  display: inline-block;",
    "  text-align: left;",
    "  font-family: Monaco, Consolas, \"Andale Mono\",\"DejaVu Sans Mono\", monospace;",
    "  font-size: 95%;",
    "  line-height: 140%;",
    "  white-space: pre;",
    "  white-space: pre-wrap;",
    "  white-space: -moz-pre-wrap;",
    "  white-space: -o-pre-wrap;",
    "  background: #faf8f0;}"],
  ".list {text-align: left;}",
  vcat [
    "figure {",
    "text-align: center;",
    "font-weight: bold;",
    "}"],
  vcat [
    "figure > img {",
    "max-width: 100%;",
    "}"],
  vcat [
    ".matrix {",
    "  position: relative;",
    "  display: inline-table;",
    "  margin: 10px;",
    "  vertical-align: middle;}",
    ".matrix:before, .matrix:after {",
    "  content: \"\";",
    "  position: absolute;",
    "  top: 0;",
    "  border: 1px solid #000;",
    "  width: 5px;",
    "  height: 100%;}",
    ".matrix:before {",
    "  left: -5px;",
    "  border-right: 0px;}",
    ".matrix:after {",
    "  right: -5px;",
    "  border-left: 0px;}",
    ".matrix td {",
    "  padding: 5px;",
    "  text-align: center;",
    "  border: 0px;}"],
  vcat [
    "ul.hide-list-style {",
    "  list-style-type: none;}"
    ],
  vcat [
    "ul.hide-list-style-no-indent {",
    "  list-style-type: none;",
    "padding: 0;}"
    ],
  vcat [
    "dl.reference-list {",
    "  display: grid;",
    "  grid-template-columns: auto 1fr;",
    "  gap: 20px;",
    "  align-items: start;}"
    ],
  vcat [
    "dd {",
    "  margin: 0;}"
    ]
  ]
