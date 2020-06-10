module Language.Drasil.Code.Imperative.Comments (
  getComment
) where

import Language.Drasil
import Database.Drasil (defTable)
import Language.Drasil.Chunk.Code (CodeIdea(..))
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.CodeSpec (CodeSpec(..))
import Language.Drasil.Printers (Linearity(Linear), sentenceDoc, unitDoc)

import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe)
import Control.Monad.State (get)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), colon, empty, parens, render)

getTermDoc :: (CodeIdea c) => c -> GenState Doc
getTermDoc c = do
  g <- get
  let db = sysinfodb $ codeSpec g
  return $ sentenceDoc db Implementation Linear $ phraseNP $ codeChunk c ^. term

getDefnDoc :: (CodeIdea c) => c -> GenState Doc
getDefnDoc c = do
  g <- get
  let db = sysinfodb $ codeSpec g
  return $ maybe empty ((<+>) colon . sentenceDoc db Implementation Linear . 
    (^. defn) . fst) (Map.lookup (codeChunk c ^. uid) $ defTable db)

getUnitsDoc :: (CodeIdea c) => c -> Doc
getUnitsDoc c = maybe empty (parens . unitDoc Linear . usymb) 
  (getUnit $ codeChunk c)

getComment :: (CodeIdea c) => c -> GenState String
getComment l = do
  t <- getTermDoc l
  d <- getDefnDoc l
  let u = getUnitsDoc l
  return $ render $ (t <> d) <+> u
