-- | Contains functions for generating code comments that describe a chunk.
module Language.Drasil.Code.Imperative.Comments (
  getCommentBrief
) where

import Control.Monad.State (get)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), empty, parens, render)

import Drasil.Code.CodeVar (CodeIdea(..))
import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.Printers (oneLineSentenceDoc, oneLineUnitDoc)

-- | Gets a plain renderering of the term for a chunk.
getTermDoc :: (CodeIdea c) => c -> GenState Doc
getTermDoc c = do
  g <- get
  return $ oneLineSentenceDoc (printfo g) $ phrase $ codeChunk c

-- | Gets a plain rendering of the unit of a chunk in parentheses,
-- or empty if it has no unit.
getUnitsDoc :: (CodeIdea c) => c -> Doc
getUnitsDoc c = maybe empty (parens . oneLineUnitDoc . usymb)
  (getUnit $ codeChunk c)

getCommentBrief :: (CodeIdea c) => c -> GenState String
getCommentBrief l = do
  t <- getTermDoc l
  let u = getUnitsDoc l
  return $ render $ t <+> u
