{-# LANGUAGE RankNTypes #-}
module Language.Drasil.Debug.Print where

import           Language.Drasil hiding (symbol)
import qualified Language.Drasil as L (symbol)
import           Database.Drasil
import           Utils.Drasil (stringList)
import qualified Data.Map as Map
import           Control.Lens ((^.), view)
import           Data.List (sort, sortBy)
import           Data.Foldable (foldl')
import           Data.Maybe (fromMaybe)
import           Data.Bifunctor (second)
import           Data.Function (on)
import           Text.PrettyPrint.HughesPJ
import           Language.Drasil.Plain.Print
import           Language.Drasil.Printing.PrintingInformation
import           Prelude hiding ((<>))

import Data.Containers.ListUtils (nubOrd)

-- * Main Function
-- | Gathers all printing functions and creates the debugging tables from them.
printAllDebugInfo :: PrintingInformation -> [Doc]
printAllDebugInfo pinfo = map
  (cdbSection . ($ pinfo))
  [ mkTableReferencedChunks
  , mkTableDepChunks
  , mkTableDepReffedChunks
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
  , mkTableRef
  , renderUsedUIDs . mkListShowUsedUIDs]

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
  :: HasUID a => PrintingInformation
  -> (ChunkDB -> UMap a)
  -> String
  -> [PrintingInformation -> (String, a -> Doc)]
  -> Doc
mkTableFromLenses pin@PI { _ckdb = db } tableLens ttle hsNEs =
  text ttle <> colon
  $$ header hdr
  $$ vcat (map col chunks)
  where
    namedLenses = map ($ pin) hsNEs
    ins :: [Int]
    ins = [1..]

    hdr   = foldl' (\r l -> r $$ nest (nestNum * snd l) (text $ fst l)) (text "UID")       (zip (map fst namedLenses) ins)
    col a = foldl' (\r l -> r $$ nest (nestNum * snd l) (fst l a)     ) (text $ showUID a) (zip (map snd namedLenses) ins)

    chunks = map (fst . snd) (Map.assocs $ tableLens db)

    nestNum = 30

openTerm :: NamedIdea a => PrintingInformation -> (String, a -> Doc)
openTerm pinfo = ("Term", sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) MultiLine . phraseNP . view term)

openSymbol :: HasSymbol a =>PrintingInformation -> (String, a -> Doc)
openSymbol pinfo = ("Symbol", symbolDoc . flip L.symbol (pinfo ^. stg))

openDefSymbol :: DefinesQuantity s => PrintingInformation -> (String, s -> Doc)
openDefSymbol pinfo = ("Symbol Defining", symbolDoc . flip L.symbol (pinfo ^. stg) . view defLhs)

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
  symbolTable
  "Symbol Chunks"
  [openTerm, openSymbol]

-- | Makes a table with terms in the SRS.
mkTableOfTerms :: PrintingInformation -> Doc
mkTableOfTerms pinfo = mkTableFromLenses
  pinfo
  termTable
  "Term Chunks"
  [openTerm, openAbbreviation]

-- | Makes a table with all concepts in the SRS.
mkTableConcepts :: PrintingInformation -> Doc
mkTableConcepts pinfo = mkTableFromLenses
  pinfo
  defTable
  "Concepts"
  [openTerm, openDefinition]

-- | Makes a table with all units used in the SRS.
mkTableUnitDefn :: PrintingInformation -> Doc
mkTableUnitDefn pinfo = mkTableFromLenses
  pinfo
  (view unitTable)
  "Unit Definitions"
  [openTerm, openUnitSymbol]

-- | Makes a table with all data definitions in the SRS.
mkTableDataDef :: PrintingInformation -> Doc
mkTableDataDef pinfo = mkTableFromLenses
  pinfo
  (view dataDefnTable)
  "Data Definitions"
  [openTerm, openDefSymbol]

-- | Makes a table with all general definitions in the SRS.
mkTableGenDef :: PrintingInformation -> Doc
mkTableGenDef pinfo = mkTableFromLenses
  pinfo
  (view gendefTable)
  "General Definitions"
  [openTerm, openDefinition]

-- | Makes a table with all theoretical models in the SRS.
mkTableTMod :: PrintingInformation -> Doc
mkTableTMod pinfo = mkTableFromLenses
  pinfo
  (view theoryModelTable)
  "Theory Models"
  [openTerm, openDefinition]

-- | Makes a table with all instance models in the SRS.
mkTableIMod :: PrintingInformation -> Doc
mkTableIMod pinfo = mkTableFromLenses
  pinfo
  (view insmodelTable)
  "Instance Models"
  [openTerm, openDefinition]

-- | Makes a table with all concept instances in the SRS.
mkTableCI :: PrintingInformation -> Doc
mkTableCI pinfo = mkTableFromLenses
  pinfo
  (view conceptinsTable)
  "ConceptInstance"
  [openTerm, openShortName]

-- | Makes a table with all labelled content in the SRS.
mkTableLC :: PrintingInformation -> Doc
mkTableLC pinfo = mkTableFromLenses
  pinfo
  (view labelledcontentTable)
  "LabelledContent"
  [openShortName, openContentType]

-- | Makes a table with all references in the SRS.
mkTableRef :: PrintingInformation -> Doc
mkTableRef pinfo = mkTableFromLenses
  pinfo
  (view refTable)
  "Reference"
  [openRef, openShortName]

-- | Chunks that depend on other chunks. An empty list means the chunks do not depend on anything.
mkTableDepChunks :: PrintingInformation -> Doc
mkTableDepChunks PI { _ckdb = db } = text
  "Dependent Chunks (the chunks on the left use the chunks on the right in some capacity)"
  <> colon
  $$ header (text "UID" $$ nest nestNum (text "Dependent UIDs"))
  $$ vcat (map testIndepLayout traceMapUIDs)
  where
    testIndepLayout :: (UID, [UID]) -> Doc
    testIndepLayout (x, ys) = text (show x) $$ nest nestNum (text $ show ys)

    traceMapUIDs :: [(UID, [UID])]
    traceMapUIDs = Map.assocs $ db ^. traceTable

    nestNum = 30

-- | Chunks that are referenced and used by other chunks.
-- Those chunks build on top of the ones listed here.
mkTableReferencedChunks :: PrintingInformation -> Doc
mkTableReferencedChunks PI { _ckdb = db } =
  text "Referenced Chunks (other chunks build from these)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "UIDs that use the left UID"))
  $$ vcat (map testIsolateLayout refbyUIDs)
  where
    testIsolateLayout :: (UID, [UID]) -> Doc
    testIsolateLayout (x, ys) = text (show x) $$ nest nestNum (text $ show ys)

    refbyUIDs :: [(UID, [UID])]
    refbyUIDs = Map.assocs $ db ^. refbyTable

    nestNum = 30

-- | Chunks that use and are used by other chunks.
mkTableDepReffedChunks :: PrintingInformation -> Doc
mkTableDepReffedChunks PI { _ckdb = db } = text
  "Dependent and Referenced Chunks (chunks dependent on middle UIDs and used in the chunks on the right)"
  <> colon
  $$ header
    (text "UID"
     $$ nest nestNum (text "Dependent Chunk")
     $$ nest (nestNum * 3) (text "Used-in Chunk"))
  $$ vcat (map traceRefLayout $ Map.assocs combinedMaps)
  where
    traceRefLayout :: (UID, ([UID], [UID])) -> Doc
    traceRefLayout x = text (show $ fst x)
      $$ nest nestNum (text $ show $ fst $ snd x)
      $$ nest (nestNum * 3) (text $ show $ snd $ snd x)

    combinedMaps =
      Map.unionWith (\x y -> (fst x, snd y)) traceMapUIDs refByUIDs

    traceMapUIDs = Map.fromList
      $ map (\(x, y) -> (x, (y, [])))
      $ Map.assocs
      $ db ^. traceTable

    refByUIDs = Map.fromList
      $ map (\(x, y) -> (x, ([], y)))
      $ Map.assocs
      $ db ^. refbyTable

    nestNum = 30

-- ** 'UID' Manipulation
-- | Creates a table of all UIDs and their "highest" recorded level of information. See 'mkListShowUsedUIDs'
-- for more details.
renderUsedUIDs :: [(UID, String)] -> Doc
renderUsedUIDs chs = header (text "UIDs" $$ nest 40 (text "Associated Chunks"))
  $$ vcat (map renderUsedUID chs)
  where
    renderUsedUID (u, chks) = text (show u) $$ nest 40 (text chks)

-- | For the last section of the log output. Shows which chunk UID is being used at which stage.
-- Note that chunks used at a "higher stage" (like 'Concept's and 'QuantityDict's) will still be built off of the
-- more basic types (like 'IdeaDict's), they are just not explicitly used in that manner.
-- Also, some chunks may have been "downgraded" when put into the database (for example, mapping a
-- 'QuantityDict' wrapper onto things like Constrained and Unital chunks happens often).
mkListShowUsedUIDs :: PrintingInformation -> [(UID, String)]
mkListShowUsedUIDs PI { _ckdb = db } = sortBy (compare `on` fst)
  $ map (second stringList)
  $ Map.toList
  $ Map.fromListWith (++)
  $ map (\x -> (fst x, ["QuantityDict"])) (Map.assocs $ symbolTable db)
  ++ map (\x -> (fst x, ["IdeaDict"])) (Map.assocs $ termTable db)
  ++ map (\x -> (fst x, ["ConceptChunk"])) (Map.assocs $ defTable db)
  ++ map (\x -> (fst x, ["UnitDefn"])) (Map.assocs $ db ^. unitTable)
  ++ map (\x -> (fst x, ["DataDefinition"])) (Map.assocs $ db ^. dataDefnTable)
  ++ map (\x -> (fst x, ["InstanceModel"])) (Map.assocs $ db ^. insmodelTable)
  ++ map
    (\x -> (fst x, ["GeneralDefinition"]))
    (Map.assocs $ db ^. gendefTable)
  ++ map (\x -> (fst x, ["TheoryModel"])) (Map.assocs $ db ^. theoryModelTable)
  ++ map
    (\x -> (fst x, ["ConceptInstance"]))
    (Map.assocs $ db ^. conceptinsTable)
  ++ map
    (\x -> (fst x, ["LabelledContent"]))
    (Map.assocs $ db ^. labelledcontentTable)
  ++ map (\x -> (fst x, ["Reference"])) (Map.assocs $ db ^. refTable)

-- Currently Unused
-- | Get all 'UID's from a database ('ChunkDB').
mkListAll :: ChunkDB -> [UID]
mkListAll db = nubOrd
  $ sort
  $ map fst (Map.assocs $ symbolTable db)
  ++ map fst (Map.assocs $ termTable db)
  ++ map fst (Map.assocs $ defTable db)
  ++ map fst (Map.assocs $ db ^. unitTable)
  ++ map fst (Map.assocs $ db ^. traceTable)
  ++ map fst (Map.assocs $ db ^. refbyTable)
  ++ map fst (Map.assocs $ db ^. dataDefnTable)
  ++ map fst (Map.assocs $ db ^. insmodelTable)
  ++ map fst (Map.assocs $ db ^. gendefTable)
  ++ map fst (Map.assocs $ db ^. theoryModelTable)
  ++ map fst (Map.assocs $ db ^. conceptinsTable)
  ++ map fst (Map.assocs $ db ^. labelledcontentTable)
  ++ map fst (Map.assocs $ db ^. refTable)
