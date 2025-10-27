{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Language.Drasil.Debug.Print where

import Prelude hiding ((<>))

import Control.Lens ((^.), view)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as Map

import Language.Drasil
import Database.Drasil
import Language.Drasil.Plain.Print
import Language.Drasil.Printing.PrintingInformation

import Theory.Drasil
import Data.Typeable (Proxy (Proxy))

-- * Main Function
-- | Gathers all printing functions and creates the debugging tables from them.
printAllDebugInfo :: PrintingInformation -> [Doc]
printAllDebugInfo pinfo = map
  (cdbSection . ($ pinfo))
  [ mkTableReferencedChunks
  , mkTableDepChunks
  , mkTableSymb
  , mkTableOfTerms
  , mkTableConcepts
  , mkTableUnitDefn
  , mkTableDataDef
  , mkTableGenDef
  , mkTableTMod
  , mkTableIMod
  , mkTableCI
  , mkTableLC
  , mkTableRef]

-- * Helpers
-- ** Separators
-- | Debugging table separator.
cdbSection :: Doc -> Doc
cdbSection dd = text (replicate 100 '#' ++ "\n") $$ dd $$ text "\n"

-- | Header for debugging tables.
header :: Doc -> Doc
header d = text (replicate 100 '-') $$ d $$ text (replicate 100 '-')

-- ** Table Generators

-- | General function to make the debugging tables. Takes in printing
-- information, a function that extracts a certain field from the printing
-- information, a title, three column headers, and three functions that sort the
-- data from the printing information field into the required display formats
-- (often 'UID's, terms, shortnames, definitions, etc.).
mkTableFromLenses
  :: IsChunk a => PrintingInformation
  -> Proxy a -- Data is unused, but necessary for type constraint resolution.
  -> String
  -> [PrintingInformation -> (String, a -> Doc)]
  -> Doc
mkTableFromLenses pin _ ttle hsNEs =
  text ttle <> colon
  $$ header hdr
  $$ vcat (map col chunks)
  where
    namedLenses = map ($ pin) hsNEs
    ins :: [Int]
    ins = [1..]

    hdr   = foldl' (\r l -> r $$ nest (nestNum * snd l) (text $ fst l)) (text "UID")       (zip (map fst namedLenses) ins)
    col a = foldl' (\r l -> r $$ nest (nestNum * snd l) (fst l a)     ) (text $ showUID a) (zip (map snd namedLenses) ins)

    chunks = findAll $ pin ^. ckdb

    nestNum = 30

openTerm :: NamedIdea a => PrintingInformation -> (String, a -> Doc)
openTerm pinfo = ("Term", sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) MultiLine . phraseNP . view term)

openSymbol :: HasSymbol a =>PrintingInformation -> (String, a -> Doc)
openSymbol pinfo = ("Symbol", symbolDoc . flip symbol (pinfo ^. stg))

openDefSymbol :: DefinesQuantity s => PrintingInformation -> (String, s -> Doc)
openDefSymbol pinfo = ("Symbol Defining", symbolDoc . flip symbol (pinfo ^. stg) . view defLhs)

openAbbreviation :: Idea a => PrintingInformation -> (String, a -> Doc)
openAbbreviation _ = ("Abbreviation", text . fromMaybe "" . getA)

openDefinition :: Definition a => PrintingInformation -> (String, a -> Doc)
openDefinition pinfo = ("Definition", sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) OneLine . view defn)

openUnitSymbol :: HasUnitSymbol a => PrintingInformation -> (String, a -> Doc)
openUnitSymbol pinfo = ("Unit Symbol", sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) OneLine . Sy . usymb)

openShortName :: HasShortName a => PrintingInformation -> (String, a -> Doc)
openShortName pinfo = ("Short Name", sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) OneLine . getSentSN . shortname)

openTitle :: PrintingInformation -> (String, Section -> Doc)
openTitle pinfo = ("Title", sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) MultiLine . tle)

cntntToStr :: RawContent -> String
cntntToStr Table {} = "Table"
cntntToStr Paragraph {} = "Paragraph"
cntntToStr EqnBlock {} = "Equation"
cntntToStr DerivBlock {} = "Derivation"
cntntToStr Enumeration {} = "Enumeration"
cntntToStr Defini {} = "Definition or Model"
cntntToStr Figure {} = "Figure"
cntntToStr Bib {} = "Bibliography"
cntntToStr Graph {} = "Graph"
cntntToStr CodeBlock {} = "Code"

openContentType :: HasContents s => p -> (String, s -> Doc)
openContentType _ = ("Content Type", text . cntntToStr . view accessContents)

openRef :: HasRefAddress a => p -> (String, a -> Doc)
openRef _ = ("Reference Address", text . getAdd . getRefAdd)

-- | Makes a table with all symbolic quantities in the SRS.
mkTableSymb :: PrintingInformation -> Doc
mkTableSymb pinfo = mkTableFromLenses
  pinfo
  (Proxy @DefinedQuantityDict)
  "Symbol Chunks"
  [openTerm, openSymbol]

-- | Makes a table with terms in the SRS.
mkTableOfTerms :: PrintingInformation -> Doc
mkTableOfTerms pinfo = mkTableFromLenses
  pinfo
  (Proxy @IdeaDict)
  "Term Chunks"
  [openTerm, openAbbreviation]

-- | Makes a table with all concepts in the SRS.
mkTableConcepts :: PrintingInformation -> Doc
mkTableConcepts pinfo = mkTableFromLenses
  pinfo
  (Proxy @ConceptChunk)
  "Concepts"
  [openTerm] -- FIXME: `openDefinition` ommited because some ConceptChunks
             -- contain references to non-existent `Reference`s (which are only
             -- created at SRS generation time).

-- | Makes a table with all units used in the SRS.
mkTableUnitDefn :: PrintingInformation -> Doc
mkTableUnitDefn pinfo = mkTableFromLenses
  pinfo
  (Proxy @UnitDefn)
  "Unit Definitions"
  [openTerm, openUnitSymbol]

-- | Makes a table with all data definitions in the SRS.
mkTableDataDef :: PrintingInformation -> Doc
mkTableDataDef pinfo = mkTableFromLenses
  pinfo
  (Proxy @DataDefinition)
  "Data Definitions"
  [openTerm, openDefSymbol]

-- | Makes a table with all general definitions in the SRS.
mkTableGenDef :: PrintingInformation -> Doc
mkTableGenDef pinfo = mkTableFromLenses
  pinfo
  (Proxy @GenDefn)
  "General Definitions"
  [openTerm, openDefinition]

-- | Makes a table with all theoretical models in the SRS.
mkTableTMod :: PrintingInformation -> Doc
mkTableTMod pinfo = mkTableFromLenses
  pinfo
  (Proxy @TheoryModel)
  "Theory Models"
  [openTerm, openDefinition]

-- | Makes a table with all instance models in the SRS.
mkTableIMod :: PrintingInformation -> Doc
mkTableIMod pinfo = mkTableFromLenses
  pinfo
  (Proxy @InstanceModel)
  "Instance Models"
  [openTerm, openDefinition]

-- | Makes a table with all concept instances in the SRS.
mkTableCI :: PrintingInformation -> Doc
mkTableCI pinfo = mkTableFromLenses
  pinfo
  (Proxy @ConceptInstance)
  "ConceptInstance"
  [openTerm, openShortName]

-- | Makes a table with all labelled content in the SRS.
mkTableLC :: PrintingInformation -> Doc
mkTableLC pinfo = mkTableFromLenses
  pinfo
  (Proxy @LabelledContent)
  "LabelledContent"
  [openShortName, openContentType]

-- | Makes a table with all references in the SRS.
mkTableRef :: PrintingInformation -> Doc
mkTableRef pinfo = mkTableFromLenses
  pinfo
  (Proxy @Reference)
  "Reference"
  [openRef, openShortName]

-- | Chunks that depend on other chunks. An empty list means the chunks do not depend on anything.
mkTableDepChunks :: PrintingInformation -> Doc
mkTableDepChunks pinfo = text
  "Dependent Chunks (the chunks on the left use the chunks on the right in some capacity)"
  <> colon
  $$ header (text "UID" $$ nest nestNum (text "Dependent UIDs"))
  $$ vcat (map testIndepLayout traceMapUIDs)
  where
    testIndepLayout :: (UID, [UID]) -> Doc
    testIndepLayout (x, ys) = text (show x) $$ nest nestNum (text $ show ys)

    traceMapUIDs :: [(UID, [UID])]
    traceMapUIDs = Map.assocs $ traceTable $ pinfo ^. ckdb

    nestNum = 30

-- | Chunks that are referenced and used by other chunks.
-- Those chunks build on top of the ones listed here.
mkTableReferencedChunks :: PrintingInformation -> Doc
mkTableReferencedChunks pinfo =
  text "Referenced Chunks (other chunks build from these)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "UIDs that use the left UID"))
  $$ vcat (map testIsolateLayout refbyUIDs)
  where
    testIsolateLayout :: (UID, [UID]) -> Doc
    testIsolateLayout (x, ys) = text (show x) $$ nest nestNum (text $ show ys)

    refbyUIDs :: [(UID, [UID])]
    refbyUIDs = Map.assocs $ refbyTable $ pinfo ^. ckdb

    nestNum = 30

-- ** 'UID' Manipulation
-- | Creates a table of all UIDs and their "highest" recorded level of information. See 'mkListShowUsedUIDs'
-- for more details.
renderUsedUIDs :: [(UID, String)] -> Doc
renderUsedUIDs chs = header (text "UIDs" $$ nest 40 (text "Associated Chunks"))
  $$ vcat (map renderUsedUID chs)
  where
    renderUsedUID (u, chks) = text (show u) $$ nest 40 (text chks)
