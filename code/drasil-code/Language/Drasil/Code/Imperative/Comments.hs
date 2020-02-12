module Language.Drasil.Code.Imperative.Comments (
  paramComment, returnComment
) where

import Language.Drasil
import Database.Drasil (defTable)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..))
import Language.Drasil.Printers (Linearity(Linear), sentenceDoc, unitDoc)

import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe)
import Control.Monad.Reader (Reader, ask)
import Control.Lens (view)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), colon, empty, parens, render, 
  text)

getTermDoc :: (NamedIdea c) => UID -> Map UID c -> Reader DrasilState Doc
getTermDoc cname m = do
  g <- ask
  let db = sysinfodb $ csi $ codeSpec g
  return $ (maybe (text "No description given") (sentenceDoc db 
    Implementation Linear . phraseNP . view term) . Map.lookup cname) m

getDefnDoc :: UID -> Reader DrasilState Doc
getDefnDoc cname = do
  g <- ask
  let db = sysinfodb $ csi $ codeSpec g
  return $ maybe empty ((<+>) colon . sentenceDoc db Implementation Linear . 
    view defn . fst) (Map.lookup cname $ defTable db)

getUnitsDoc :: (MayHaveUnit c) => UID -> Map UID c -> Doc
getUnitsDoc cname m = maybe empty (parens . unitDoc Linear . usymb) 
  (Map.lookup cname m >>= getUnit)

getComment :: (NamedIdea c, MayHaveUnit c) => UID -> Map UID c -> 
  Reader DrasilState String
getComment l m = do
  t <- getTermDoc l m
  d <- getDefnDoc l
  let u = getUnitsDoc l m
  return $ render $ (t <> d) <+> u

paramComment :: UID -> Reader DrasilState String
paramComment l = do
  g <- ask
  let m = vMap $ codeSpec g
  getComment l m

returnComment :: UID -> Reader DrasilState String
returnComment l = do
  g <- ask
  let m = fMap $ codeSpec g
  getComment l m