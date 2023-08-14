-- | Defines functions to help debug examples. Generated files appear in ./code/debug.
module Language.Drasil.Log.Print where

import Language.Drasil hiding (symbol)
import Language.Drasil.Development (showUID)
import qualified Language.Drasil as L (symbol)
import Database.Drasil
import Utils.Drasil (stringList)

import qualified Data.Map as Map
import Control.Lens ((^.), view)
import Data.List (nub, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)
import Data.Function (on)

import Text.PrettyPrint.HughesPJ
import Language.Drasil.Plain.Print
import Language.Drasil.Printing.PrintingInformation
import Prelude hiding ((<>))

-- * Main Function

-- | Gathers all printing functions and creates the debugging tables from them.
printAllDebugInfo :: PrintingInformation -> [Doc]
printAllDebugInfo pinfo = map (cdbSection . ($ pinfo)) [mkTableReferencedChunks, mkTableDepChunks, mkTableDepReffedChunks,
  mkTableSymb, mkTableOfTerms, mkTableConcepts, mkTableUnitDefn,
  mkTableDataDef, mkTableGenDef, mkTableTMod, mkTableIMod, mkTableCI,
  mkTableSec, mkTableLC, mkTableRef, renderUsedUIDs . mkListShowUsedUIDs]

-- * Helpers
-- ** Separators

-- | Debugging table separator.
cdbSection :: Doc -> Doc
cdbSection dd = text (replicate 100 '#' ++ "\n") $$ dd $$ text "\n"

-- | Header for debugging tables.
header :: Doc -> Doc
header d = text (replicate 100 '-') $$ d $$ text (replicate 100 '-')

-- ** Table Generators
-- | General function to make the debugging tables. Takes in printing information, a function
-- that extracts a certain field from the printing information, a title, three column headers,
-- and three functions that sort the data from the printing information field into the 
-- required display formats (often 'UID's, terms, shortnames, definitions, etc.).
mkTableFromLenses :: PrintingInformation -> (ChunkDB -> UMap a)
  -> String -> String -> String -> String -> (a -> Doc) -> (a -> Doc) -> (a -> Doc) -> Doc
mkTableFromLenses PI{_ckdb = db} tableLens ttle h1 h2 h3 l1 l2 l3 =
  text ttle <> colon
  $$ header (text h1 $$ nest nestNum (text h2) $$ nest (nestNum*3) (text h3))
  $$ vcat (map chunkLayout chunks)
  where
    chunkLayout x = l1 x $$ nest nestNum (l2 x)
      $$ nest (nestNum*3) (l3 x)
    chunks = map (fst.snd) (Map.assocs $ tableLens db)
    nestNum = 30

-- | Makes a table with all symbolic quantities in the SRS.
mkTableSymb :: PrintingInformation -> Doc
mkTableSymb pinfo = mkTableFromLenses pinfo symbolTable
  "Symbol Chunks" "UID" "Term" "Symbol"
      (text . showUID)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
          (symbolDoc . flip L.symbol (pinfo ^. stg))

-- | Makes a table with terms in the SRS.
mkTableOfTerms :: PrintingInformation -> Doc
mkTableOfTerms pinfo = mkTableFromLenses pinfo termTable
  "Term Chunks" "UID" "Term" "Abbreviation"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (text . fromMaybe "" . getA)

-- | Makes a table with all concepts in the SRS.
mkTableConcepts :: PrintingInformation -> Doc
mkTableConcepts pinfo = mkTableFromLenses pinfo defTable
  "Concepts" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all units used in the SRS.
mkTableUnitDefn :: PrintingInformation -> Doc
mkTableUnitDefn pinfo = mkTableFromLenses pinfo (view unitTable)
  "Unit Definitions" "UID" "Term" "Unit Symbol"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . Sy . usymb)

-- | Makes a table with all data definitions in the SRS.
mkTableDataDef :: PrintingInformation -> Doc
mkTableDataDef pinfo = mkTableFromLenses pinfo (view dataDefnTable)
  "Data Definitions" "UID" "Term" "Symbol"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (symbolDoc . flip L.symbol (pinfo ^. stg) . view defLhs)

-- | Makes a table with all general definitions in the SRS.
mkTableGenDef :: PrintingInformation -> Doc
mkTableGenDef pinfo = mkTableFromLenses pinfo (view gendefTable)
  "General Definitions" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all theoretical models in the SRS.
mkTableTMod :: PrintingInformation -> Doc
mkTableTMod pinfo = mkTableFromLenses pinfo (view theoryModelTable)
  "Theory Models" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all instance models in the SRS.
mkTableIMod :: PrintingInformation -> Doc
mkTableIMod pinfo = mkTableFromLenses pinfo (view insmodelTable)
  "Instance Models" "UID" "Term" "Definition"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . view defn)

-- | Makes a table with all concept instances in the SRS.
mkTableCI :: PrintingInformation -> Doc
mkTableCI pinfo = mkTableFromLenses pinfo (view conceptinsTable)
  "ConceptInstance" "UID" "Term" "ShortName"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . phraseNP . view term)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)

-- | Makes a table with all sections in the SRS.
mkTableSec :: PrintingInformation -> Doc
mkTableSec pinfo = mkTableFromLenses pinfo (view sectionTable)
  "Sections" "UID" "Title" "ShortName"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Nonlinear . tle)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)

-- | Makes a table with all labelled content in the SRS.
mkTableLC :: PrintingInformation -> Doc
mkTableLC pinfo = mkTableFromLenses pinfo (view labelledcontentTable)
  "LabelledContent" "UID" "ShortName" "Type of Content"
    (text . showUID)
      (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)
        (text . getContConst . view accessContents)
  where
    getContConst :: RawContent -> String
    getContConst Table{} = "Table"
    getContConst Paragraph{} = "Paragraph"
    getContConst EqnBlock{} = "Equation"
    getContConst DerivBlock{} = "Derivation"
    getContConst Enumeration{} = "Enumeration"
    getContConst Defini{} = "Definition or Model"
    getContConst Figure{} = "Figure"
    getContConst Bib{} = "Bibliography"
    getContConst Graph{} = "Graph"
    getContConst CodeBlock{} = "Code"

-- | Makes a table with all references in the SRS.
mkTableRef :: PrintingInformation -> Doc
mkTableRef pinfo = mkTableFromLenses pinfo (view refTable)
  "Reference" "UID" "Reference Address" "ShortName"
    (text . showUID)
      (text . getAdd . getRefAdd)
        (sentenceDoc (pinfo ^. ckdb) (pinfo ^. stg) Linear . getSentSN . shortname)

-- | Chunks that depend on other chunks. An empty list means the chunks do not depend on anything.
mkTableDepChunks :: PrintingInformation -> Doc
mkTableDepChunks PI{_ckdb = db} = text "Dependent Chunks (the chunks on the left use the chunks on the right in some capacity)" <> colon
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
mkTableReferencedChunks PI{_ckdb = db} = text "Referenced Chunks (other chunks build from these)" <> colon
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
mkTableDepReffedChunks PI{_ckdb = db} = text "Dependent and Referenced Chunks (chunks dependent on middle UIDs and used in the chunks on the right)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Dependent Chunk") $$ nest (nestNum*3) (text "Used-in Chunk"))
  $$ vcat (map traceRefLayout $ Map.assocs combinedMaps)
  where
    traceRefLayout :: (UID, ([UID], [UID])) -> Doc
    traceRefLayout x = text (show $ fst x) $$ nest nestNum (text $ show $ fst $ snd x)
      $$ nest (nestNum*3) (text $ show $ snd $ snd x)
    combinedMaps = Map.unionWith (\x y -> (fst x, snd y)) traceMapUIDs refByUIDs
    traceMapUIDs = Map.fromList $ map (\(x, y) -> (x, (y, []))) $ Map.assocs $ db ^. traceTable
    refByUIDs = Map.fromList $ map (\(x, y) -> (x, ([], y))) $ Map.assocs $ db ^. refbyTable
    nestNum = 30

-- ** 'UID' Manipulation
-- | Creates a table of all UIDs and their "highest" recorded level of information. See 'mkListShowUsedUIDs'
-- for more details.
renderUsedUIDs :: [(UID, String)] -> Doc
renderUsedUIDs chs = header (text "UIDs" $$ nest 40 (text "Associated Chunks")) $$ vcat (map renderUsedUID chs)
  where
    renderUsedUID (u, chks) = text (show u) $$ nest 40 (text chks)


-- | For the last section of the log output. Shows which chunk UID is being used at which stage.
-- Note that chunks used at a "higher stage" (like 'Concept's and 'QuantityDict's) will still be built off of the
-- more basic types (like 'IdeaDict's), they are just not explicitly used in that manner.
-- Also, some chunks may have been "downgraded" when put into the database (for example, mapping a
-- 'QuantityDict' wrapper onto things like Constrained and Unital chunks happens often).
mkListShowUsedUIDs :: PrintingInformation -> [(UID, String)]
mkListShowUsedUIDs PI{_ckdb = db} = sortBy (compare `on` fst) $ map (second stringList) $ Map.toList $ Map.fromListWith (++) $
  map (\x -> (fst x, ["QuantityDict"])) (Map.assocs $ symbolTable db) ++
  map (\x -> (fst x, ["IdeaDict"])) (Map.assocs $ termTable db) ++
  map (\x -> (fst x, ["ConceptChunk"])) (Map.assocs $ defTable db) ++
  map (\x -> (fst x, ["UnitDefn"])) (Map.assocs $ db ^. unitTable) ++
  map (\x -> (fst x, ["DataDefinition"])) (Map.assocs $ db ^. dataDefnTable) ++
  map (\x -> (fst x, ["InstanceModel"])) (Map.assocs $ db ^. insmodelTable) ++
  map (\x -> (fst x, ["GeneralDefinition"])) (Map.assocs $ db ^. gendefTable) ++
  map (\x -> (fst x, ["TheoryModel"])) (Map.assocs $ db ^. theoryModelTable) ++
  map (\x -> (fst x, ["ConceptInstance"])) (Map.assocs $ db ^. conceptinsTable) ++
  map (\x -> (fst x, ["Section"])) (Map.assocs $ db ^. sectionTable) ++
  map (\x -> (fst x, ["LabelledContent"])) (Map.assocs $ db ^. labelledcontentTable) ++
  map (\x -> (fst x, ["Reference"])) (Map.assocs $ db ^. refTable)

-- Currently Unused
-- | Get all 'UID's from a database ('ChunkDB').
mkListAll :: ChunkDB -> [UID]
mkListAll db = nub $ sort $
  map fst (Map.assocs $ symbolTable db) ++
  map fst (Map.assocs $ termTable db) ++
  map fst (Map.assocs $ defTable db) ++
  map fst (Map.assocs $ db ^. unitTable) ++
  map fst (Map.assocs $ db ^. traceTable) ++
  map fst (Map.assocs $ db ^. refbyTable) ++
  map fst (Map.assocs $ db ^. dataDefnTable) ++
  map fst (Map.assocs $ db ^. insmodelTable) ++
  map fst (Map.assocs $ db ^. gendefTable) ++
  map fst (Map.assocs $ db ^. theoryModelTable) ++
  map fst (Map.assocs $ db ^. conceptinsTable) ++
  map fst (Map.assocs $ db ^. sectionTable) ++
  map fst (Map.assocs $ db ^. labelledcontentTable) ++
  map fst (Map.assocs $ db ^. refTable)
