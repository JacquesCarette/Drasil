-- | Contains functions for generating code comments that describe a chunk.
module Language.Drasil.Code.Imperative.Comments (
  getComment
) where

import Language.Drasil
import Database.Drasil (defTable)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.CodeSpec (CodeSpec(..))
import Language.Drasil.Printers (Linearity(Linear), sentenceDoc, unitDoc)

import qualified Data.Map as Map (lookup)
import Control.Monad.State (get)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), colon, empty, parens, render)

-- | Gets a plain renderering of the term for a chunk.
getTermDoc :: (CodeIdea c) => c -> GenState Doc
getTermDoc c = do
  g <- get
  let db = sysinfodb $ codeSpec g
  return $ sentenceDoc db Implementation Linear $ phraseNP $ codeChunk c ^. term

-- | Gets a plain rendering of the definition of a chunk, preceded by a colon 
-- as it is intended to follow the term for the chunk. Returns empty if the 
-- chunk has no definition.
getDefnDoc :: (CodeIdea c) => c -> GenState Doc
getDefnDoc c = do
  g <- get
  let db = sysinfodb $ codeSpec g
  return $ maybe empty ((<+>) colon . sentenceDoc db Implementation Linear . 
    (^. defn) . fst) (Map.lookup (codeChunk c ^. uid) $ defTable db)

-- | Gets a plain rendering of the unit of a chunk in parentheses, 
-- or empty if it has no unit.
getUnitsDoc :: (CodeIdea c) => c -> Doc
getUnitsDoc c = maybe empty (parens . unitDoc Linear . usymb) 
  (getUnit $ codeChunk c)

-- | Generates a comment string for a chunk, including the term, 
-- definition (if applicable), and unit (if applicable).
getComment :: (CodeIdea c) => c -> GenState String
getComment l = do
  t <- getTermDoc l
  d <- getDefnDoc l
  let u = getUnitsDoc l
  return $ render $ (t <> d) <+> u
