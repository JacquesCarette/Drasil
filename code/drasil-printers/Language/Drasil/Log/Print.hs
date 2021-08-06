module Language.Drasil.Log.Print where

import Language.Drasil hiding (symbol)
import qualified Language.Drasil as L (symbol)
import Database.Drasil
import System.IO
import System.Directory
import qualified Data.Map as Map
import Control.Lens ((^.))
import Data.List (nub, sort, sortBy)
import Text.PrettyPrint.HughesPJ
import Language.Drasil.Plain.Print
--import Language.Drasil.Printing.Import
import Language.Drasil.Printing.PrintingInformation
import Prelude hiding ((<>))
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM_)
import Utils.Drasil (stringList)
import Data.Bifunctor (second)
import Data.Function (on)
import Theory.Drasil

{-
Want to go from SystemInformation or PrintingInformation to Log files.
We can start by just listing all of the UIDs in a chunk database.

-}
printAllChunkUIDs :: SystemInformation -> PrintingInformation -> IO ()
printAllChunkUIDs SI{_sys = sys} pinfo = do
  createDirectoryIfMissing False "SRSlogs"
  setCurrentDirectory "SRSlogs"
  handle <- openFile (abrv sys ++ "_SRS.log") WriteMode
  --hPutStrLn handle $ "List of all Chunk UIDs in" ++ abrv sys
  --let listUIDs = mkListAll db
  --mapM_ (hPutStrLn handle) listUIDs
  --let listChunks = mkTableSymb db
  --mapM_ (hPutStrLn handle) listChunks
  --hPutStrLn handle "\n"
  --hPutStrLn handle $ renderStyle (Style{mode=PageMode, lineLength=200, ribbonsPerLine=1.5}) $ mkTableSymb pinfo
  cdbSection handle mkTableIsolatedChunks pinfo
  cdbSection handle mkTableIndepChunks pinfo
  cdbSection handle mkTableIsoIndepChunks pinfo

  cdbSection handle mkTableSymb pinfo
  cdbSection handle mkTableOfTerms pinfo
  cdbSection handle mkTableConcepts pinfo
  cdbSection handle mkTableUnitDefn pinfo
  cdbSection handle mkTableDataDef pinfo
  cdbSection handle mkTableTMod pinfo
  cdbSection handle mkTableIMod pinfo
  cdbSection handle mkTableCI pinfo
  cdbSection handle mkTableSec pinfo
  cdbSection handle mkTableLC pinfo
  cdbSection handle mkTableRef pinfo

  cdbSection handle (renderUsedUIDs . mkListShowUsedUIDs) pinfo
  hClose handle

cdbSection :: Handle -> (PrintingInformation -> Doc) -> PrintingInformation -> IO ()
cdbSection handle prntFunc pinfo = do
  replicateM_ 100 $ hPutStr handle "#"
  hPutStrLn handle "\n"
  hPutStrLn handle $ render $ prntFunc pinfo
  hPutStrLn handle "\n"

header :: Doc -> Doc
header d = text (replicate 100 '-') $$ d $$ text (replicate 100 '-')

renderUsedUIDs :: [(UID, String)] -> Doc
renderUsedUIDs chs = header (text "UIDs" $$ nest 40 (text "Associated Chunks")) $$ vcat (map renderUsedUID chs)
  where
    renderUsedUID (u, chks) = text u $$ nest 40 (text chks)

mkListShowUsedUIDs :: PrintingInformation -> [(UID, String)]
mkListShowUsedUIDs PI{_ckdb = db} = sortBy (compare `on` fst) $ map (second stringList) $ Map.toList $ Map.fromListWith (++) $
  map (\x -> (fst x, ["QuantityDict"])) (Map.assocs $ symbolTable db) ++
  map (\x -> (fst x, ["IdeaDict"])) (Map.assocs $ termTable db) ++
  map (\x -> (fst x, ["ConceptChunk"])) (Map.assocs $ defTable db) ++
  map (\x -> (fst x, ["UnitDefn"])) (Map.assocs $ db ^. unitTable) ++
  --map (\x -> (fst x, [""]) (Map.assocs $ db ^. traceTable) ++
  --map (\x -> (fst x, [""]) (Map.assocs $ db ^. refbyTable) ++
  map (\x -> (fst x, ["DataDefinition"])) (Map.assocs $ db ^. dataDefnTable) ++
  map (\x -> (fst x, ["InstanceModel"])) (Map.assocs $ db ^. insmodelTable) ++
  map (\x -> (fst x, ["GeneralDefinition"])) (Map.assocs $ db ^. gendefTable) ++
  map (\x -> (fst x, ["TheoryModel"])) (Map.assocs $ db ^. theoryModelTable) ++
  map (\x -> (fst x, ["ConceptInstance"])) (Map.assocs $ db ^. conceptinsTable) ++
  map (\x -> (fst x, ["Section"])) (Map.assocs $ db ^. sectionTable) ++
  map (\x -> (fst x, ["LabelledContent"])) (Map.assocs $ db ^. labelledcontentTable) ++
  map (\x -> (fst x, ["Reference"])) (Map.assocs $ db ^. refTable)

mkTableSymb :: PrintingInformation -> Doc
mkTableSymb PI{_ckdb = db, _stg = s} = text "Symbol Chunks" <> colon 
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Symbol"))
  $$ vcat (map symbLayout symbs)
  where
    symbLayout :: QuantityDict -> Doc
    symbLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (symbolDoc $ L.symbol x s)
    symbs :: [QuantityDict]
    symbs = map (fst.snd) (Map.assocs $ symbolTable db)
    nestNum = 30

mkTableOfTerms :: PrintingInformation -> Doc
mkTableOfTerms PI{_ckdb = db, _stg = s} = text "Term Chunks" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Abbreviation"))
  $$ vcat (map trmLayout trms)
  where
    trmLayout :: IdeaDict -> Doc
    trmLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ fromMaybe empty (mayDoc $ getA x)
    mayDoc :: Maybe String -> Maybe Doc
    mayDoc (Just strng) = Just $ nest (nestNum*3) $ text strng
    mayDoc Nothing = Nothing
    trms :: [IdeaDict]
    trms = map (fst.snd) (Map.assocs $ termTable db)
    nestNum = 30

mkTableConcepts :: PrintingInformation -> Doc
mkTableConcepts PI{_ckdb = db, _stg = s} = text "Concepts" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Definition"))
  $$ vcat (map conceptLayout concepts)
  where
    conceptLayout :: ConceptChunk -> Doc
    conceptLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ x ^. defn)
    concepts :: [ConceptChunk]
    concepts = map (fst.snd) (Map.assocs $ defTable db)
    nestNum = 30

mkTableUnitDefn :: PrintingInformation -> Doc
mkTableUnitDefn PI{_ckdb = db, _stg = s} = text "Unit Definitions" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Unit Symbol"))
  $$ vcat (map unitDefnLayout unitDefns)
  where
    unitDefnLayout :: UnitDefn -> Doc
    unitDefnLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ Sy $ usymb x)
    unitDefns :: [UnitDefn]
    unitDefns = map (fst.snd) (Map.assocs $ db ^. unitTable)
    nestNum = 30

mkTableDataDef :: PrintingInformation -> Doc
mkTableDataDef PI{_ckdb = db, _stg = s} = text "Data Definitions" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Symbol"))
  $$ vcat (map dataDefLayout dataDefs)
  where
    dataDefLayout :: DataDefinition -> Doc
    dataDefLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (symbolDoc $ L.symbol x s)
    dataDefs :: [DataDefinition]
    dataDefs = map (fst.snd) (Map.assocs $ db ^. dataDefnTable)
    nestNum = 30

mkTableGenDef :: PrintingInformation -> Doc
mkTableGenDef PI{_ckdb = db, _stg = s} = text "General Definitions" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Definition"))
  $$ vcat (map genDefLayout genDefs)
  where
    genDefLayout :: GenDefn -> Doc
    genDefLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ x ^. defn)
    genDefs :: [GenDefn]
    genDefs = map (fst.snd) (Map.assocs $ db ^. gendefTable)
    nestNum = 30

mkTableTMod :: PrintingInformation -> Doc
mkTableTMod PI{_ckdb = db, _stg = s} = text "Theory Models" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Definition"))
  $$ vcat (map tModLayout tMods)
  where
    tModLayout :: TheoryModel -> Doc
    tModLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ x ^. defn)
    tMods :: [TheoryModel]
    tMods = map (fst.snd) (Map.assocs $ db ^. theoryModelTable)
    nestNum = 30
    
mkTableIMod :: PrintingInformation -> Doc
mkTableIMod PI{_ckdb = db, _stg = s} = text "Instance Models" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "Definition"))
  $$ vcat (map iModLayout iMods)
  where
    iModLayout :: InstanceModel -> Doc
    iModLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ x ^. defn)
    iMods :: [InstanceModel]
    iMods = map (fst.snd) (Map.assocs $ db ^. insmodelTable)
    nestNum = 30

mkTableCI :: PrintingInformation -> Doc
mkTableCI PI{_ckdb = db, _stg = s} = text "ConceptInstance" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Term") $$ nest (nestNum*3) (text "ShortName"))
  $$ vcat (map concInstLayout concInsts)
  where
    concInstLayout :: ConceptInstance -> Doc
    concInstLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ getSentSN $ shortname x)
    concInsts :: [ConceptInstance]
    concInsts = map (fst.snd) (Map.assocs $ db ^. conceptinsTable)
    nestNum = 30

mkTableSec :: PrintingInformation -> Doc
mkTableSec PI{_ckdb = db, _stg = s} = text "Section" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Title") $$ nest (nestNum*3) (text "ShortName"))
  $$ vcat (map sectnLayout sectns)
  where
    sectnLayout :: Section -> Doc
    sectnLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ tle x)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ getSentSN $ shortname x)
    sectns :: [Section]
    sectns = map (fst.snd) (Map.assocs $ db ^. sectionTable)
    nestNum = 30

mkTableLC :: PrintingInformation -> Doc
mkTableLC PI{_ckdb = db, _stg = s} = text "LabelledContent" <> colon
  $$ header (text "UID" $$ nest nestNum (text "ShortName") $$ nest (nestNum*3) (text "Type of Content"))
  $$ vcat (map lContentLayout lContents)
  where
    lContentLayout :: LabelledContent -> Doc
    lContentLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Linear $ getSentSN $ shortname x)
      $$ nest (nestNum*3) (text $ getContConst $ x ^. accessContents)
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
    -- getContConst _ = "Unknown Content Type. Add in drasl-printer/Language/Drasil/Log/Print.hs"
    lContents :: [LabelledContent]
    lContents = map (fst.snd) (Map.assocs $ db ^. labelledcontentTable)
    nestNum = 30

mkTableRef :: PrintingInformation -> Doc
mkTableRef PI{_ckdb = db, _stg = s} = text "Reference" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Reference Address") $$ nest (nestNum*3) (text "Short Name"))
  $$ vcat (map refLayout refs)
  where
    refLayout :: Reference -> Doc
    refLayout x = text (x ^. uid) $$ nest nestNum (text $ getAdd $ getRefAdd x)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ getSentSN $ shortname x)
    refs :: [Reference]
    refs = map (fst.snd) (Map.assocs $ db ^. refTable)
    nestNum = 30

-- | Chunks that dont depend on anything.
mkTableIndepChunks :: PrintingInformation -> Doc
mkTableIndepChunks PI{_ckdb = db, _stg = s} = text "Independent Chunks (these do not use other chunks)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Dependent UIDs"))
  $$ vcat (map testIndepLayout traceMapUIDs)
  where
    uidList = mkListAll db
    testIndepLayout :: (UID, [UID]) -> Doc
    testIndepLayout (x, ys) = text x $$ nest nestNum (text $ show ys)
    --refLayout :: Reference -> Doc
    --refLayout x = text (x ^. uid) $$ nest nestNum (text $ getAdd $ getRefAdd x)
     -- $$ nest (nestNum*3) (sentenceDoc db s Linear $ getSentSN $ shortname x)
    traceMapUIDs :: [(UID, [UID])]
    traceMapUIDs = Map.assocs $ db ^. traceTable
    nestNum = 30

-- compare against refby map here
-- | Chunks that are not used by anything.
mkTableIsolatedChunks :: PrintingInformation -> Doc
mkTableIsolatedChunks PI{_ckdb = db, _stg = s} = text "Isolated Chunks (no other chunks depend on these)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "UIDs that use the left UID"))
  $$ vcat (map testIsolateLayout refbyUIDs)
  where
    uidList = mkListAll db
    testIsolateLayout :: (UID, [UID]) -> Doc
    testIsolateLayout (x, ys) = text x $$ nest nestNum (text $ show ys)
    refbyUIDs :: [(UID, [UID])]
    refbyUIDs = Map.assocs $ db ^. refbyTable
    nestNum = 30
    
-- | Chunks that are not used by anything and dont depend on anything.
mkTableIsoIndepChunks :: PrintingInformation -> Doc
mkTableIsoIndepChunks PI{_ckdb = db, _stg = s} = text "Isolated Chunks (no other chunks depend on these)" <> colon
  $$ header (text "UID" $$ nest nestNum (text "Reference Address") $$ nest (nestNum*3) (text "Short Name"))
  $$ vcat (map refLayout refs)
  where
    refLayout :: Reference -> Doc
    refLayout x = text (x ^. uid) $$ nest nestNum (text $ getAdd $ getRefAdd x)
      $$ nest (nestNum*3) (sentenceDoc db s Linear $ getSentSN $ shortname x)
    refs :: [Reference]
    refs = map (fst.snd) (Map.assocs $ db ^. refTable)
    nestNum = 30

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

{-mkTableTerms :: PrintingInformation -> Doc
mkTableSymb PI{_ckdb = db, _stg = s} = text "Term Chunks" <> colon $$ header $$
  vcat (map symbLayout symbs)
  where
    header :: Doc
    header = text (replicate 100 '-') $$ text "UID" $$ nest nestNum (text "Term")  
      $$ nest (nestNum*3) (text "Symbol") $$ text (replicate 100 '-')
    symbLayout :: QuantityDict -> Doc
    symbLayout x = text (x ^. uid) $$ nest nestNum (sentenceDoc db s Nonlinear $ phraseNP $ x ^. term)
      $$ nest (nestNum*3) (sentenceDoc db s Nonlinear $ P $ L.symbol x s)
    symbs :: [QuantityDict]
    symbs = map (fst.snd) (Map.assocs $ symbolTable db) --QuantityDict
    nestNum = 30
  -}
{-nub $ sort $
  "Symbol UIDs:"
  map (fst.snd) (Map.assocs $ symbolTable db) ++
  map (fst.snd) (Map.assocs $ termTable db) ++
  map (fst.snd) (Map.assocs $ defTable db) ++
  map (fst.snd) (Map.assocs $ db ^. unitTable) ++
  map fst (Map.assocs $ db ^. traceTable) ++ -- TODO
  map fst (Map.assocs $ db ^. refbyTable) ++
  map (fst.snd) (Map.assocs $ db ^. dataDefnTable) ++
  map (fst.snd) (Map.assocs $ db ^. insmodelTable) ++
  map (fst.snd) (Map.assocs $ db ^. gendefTable) ++
  map (fst.snd) (Map.assocs $ db ^. theoryModelTable) ++
  map (fst.snd) (Map.assocs $ db ^. conceptinsTable) ++
  map (fst.snd) (Map.assocs $ db ^. sectionTable) ++
  map (fst.snd) (Map.assocs $ db ^. labelledcontentTable) ++
  map (fst.snd) (Map.assocs $ db ^. refTable)-}




