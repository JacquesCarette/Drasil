-- | Contains functions for generating code comments that describe a chunk.
module Language.Drasil.Code.Imperative.Comments (
  getCommentBrief
) where

import Control.Monad.State (get)
import Text.PrettyPrint.HughesPJ ((<+>), parens, render)

import Drasil.Code.CodeVar (CodeIdea(..))
import Language.Drasil (phrase, MayHaveUnit(..), HasUnitSymbol(..))
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.Printers (oneLineSentenceDoc, oneLineUnitDoc)

-- | For a named quantity, render its name and associated unit (when it exists)
-- in plaintext in the form: <term> (<unit>)
getCommentBrief :: (CodeIdea c) => c -> GenState String
getCommentBrief l = do
  g <- get
  let quant = codeChunk l
      tm = oneLineSentenceDoc (printfo g) $ phrase quant
      unit = parens . oneLineUnitDoc . usymb <$> getUnit quant
  pure $ render $ maybe tm (tm <+>) unit
