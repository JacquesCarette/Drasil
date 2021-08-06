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

{-
Want to go from SystemInformation or PrintingInformation to Log files.
We can start by just listing all of the UIDs in a chunk database.

-}
printAllChunkUIDs :: SystemInformation -> PrintingInformation -> IO ()
printAllChunkUIDs SI{_sys = sys, _sysinfodb = db} pinfo = do
  createDirectoryIfMissing False "SRSlogs"
  setCurrentDirectory "SRSlogs"
  handle <- openFile (abrv sys ++ "_SRS.log") WriteMode
  --hPutStrLn handle $ "List of all Chunk UIDs in" ++ abrv sys
  --let listUIDs = mkListAll db
  --mapM_ (hPutStrLn handle) listUIDs
  --let listChunks = mkTableSymb db
  --mapM_ (hPutStrLn handle) listChunks
  --hPutStrLn handle "\n"
  hPutStrLn handle $ render $ renderUsedUIDs $ mkListShowUsedUIDs db
  hPutStrLn handle "\n"
  replicateM_ 100 $ hPutStr handle "#"
  hPutStrLn handle "\n"
  --hPutStrLn handle $ renderStyle (Style{mode=PageMode, lineLength=200, ribbonsPerLine=1.5}) $ mkTableSymb pinfo
  hPutStrLn handle $ render $ mkTableSymb pinfo
  hPutStrLn handle "\n"
  replicateM_ 100 $ hPutStr handle "#"
  hPutStrLn handle "\n"
  hPutStrLn handle $ render $ mkTableOfTerms pinfo
  hPutStrLn handle "\n"
  replicateM_ 100 $ hPutStr handle "#"
  hPutStrLn handle "\n"
  hPutStrLn handle $ render $ mkTableConcepts pinfo
  hPutStrLn handle "\n"
  replicateM_ 100 $ hPutStr handle "#"
  hPutStrLn handle "\n"
  hPutStrLn handle $ render $ mkTableUnitDefn pinfo
  hClose handle

{-
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
  map fst (Map.assocs $ db ^. refTable)-}

  -- concatMap (map ((^. uid) . fst . snd) . Map.assocs . ($ db)) 
  --[symbolTable, termTable, defTable, unitTable, traceTable, refbyTable,
     -- dataDefnTable, insmodelTable, gendefTable, theoryModelTable, conceptinsTable,
     -- sectionTable, labelledcontentTable, refTable]

{-data UIDAndChunks = UAC {
  uID :: UID,
  chunkTypes :: [String]
-}
{-concatChunks :: [UIDAndChunks] -> [UIDAndChunks]
concatChunks [] = []
concatChunks [x] = [x]
concatChunks (x1:x2:xs)
  | uID x1 == uID x2 = concatChunks $ UAC (uID x1) (nub $ chunkTypes x1 ++ chunkTypes x2) : xs
  | otherwise = x1 -}
header :: Doc -> Doc
header d = text (replicate 100 '-') $$ d $$ text (replicate 100 '-')

renderUsedUIDs :: [(UID, String)] -> Doc
renderUsedUIDs chs = header (text "UIDs" $$ nest 40 (text "Associated Chunks")) $$ vcat (map renderUsedUID chs)
  where
    renderUsedUID (u, chks) = text u $$ nest 40 (text chks)

mkListShowUsedUIDs :: ChunkDB -> [(UID, String)]
mkListShowUsedUIDs db = sortBy (compare `on` fst) $ map (second stringList) $ Map.toList $ Map.fromListWith (++) $
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




