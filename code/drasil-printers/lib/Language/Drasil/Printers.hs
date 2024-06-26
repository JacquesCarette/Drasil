module Language.Drasil.Printers (
  -- * Formats
  DocType(SRS, Website, Jupyter), Format(TeX, HTML, JSON, Markdown)
  -- * DOT
  -- ** Types
  , GraphInfo(..), NodeFamily(..)
  -- ** Functions
  , outputDot
  -- * HTML
  -- ** Printer 
  , genHTML
  -- ** Helpers
  , makeCSS
  -- * Markdown
  -- ** Printer
  , makeMd
  -- ** Section Printers
  , introInfo, verInfo, unsupOS, regularSec, instDoc, extLibSec, endNote, whatInfo
  -- * Plain
  -- ** Types
  , SingleLine(..)
  -- ** Functions
  , sentenceDoc, exprDoc, codeExprDoc, symbolDoc, unitDoc, showSymb,
  showHasSymbImpl
  -- * TeX
  , genTeX
  -- * Jupyter
  , genJSON
  -- * Markdown
  , genMD, genMD', makeBook
  -- * Log
  , printAllDebugInfo
  -- * Printing Information and Options
  , PrintingInformation(..), piSys
  , HasPrintingOptions (..)
  , Notation(..)
  , defaultConfiguration
  )
  where

import Language.Drasil.Format (DocType(SRS, Website, Jupyter), Format(TeX, HTML,JSON, Markdown))
import Language.Drasil.HTML.CSS (makeCSS)
import Language.Drasil.HTML.Print (genHTML)
import Language.Drasil.JSON.Print (genJSON)
import Language.Drasil.Markdown.Print (genMD, genMD')
import Language.Drasil.Markdown.Book (makeBook)
import Language.Drasil.Markdown.CreateMd (makeMd, introInfo, verInfo, unsupOS,
  extLibSec, instDoc, regularSec, endNote, whatInfo)
import Language.Drasil.Plain.Print (SingleLine(..), sentenceDoc, exprDoc,
  codeExprDoc, symbolDoc, unitDoc, showSymb, showHasSymbImpl)
import Language.Drasil.TeX.Print (genTeX)
import Language.Drasil.Printing.PrintingInformation (PrintingInformation(..),
  HasPrintingOptions(..), Notation(..), defaultConfiguration, piSys)
import Language.Drasil.DOT.Print (outputDot, GraphInfo(..), NodeFamily(..))
import Language.Drasil.Debug.Print (printAllDebugInfo)
