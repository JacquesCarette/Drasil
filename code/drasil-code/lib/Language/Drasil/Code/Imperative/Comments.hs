-- | Contains functions for generating code comments that describe a chunk.
module Language.Drasil.Code.Imperative.Comments (
  getComment
) where

import Language.Drasil
import Database.Drasil (conceptChunkTable)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.CodeSpec (HasOldCodeSpec(..))
import Language.Drasil.Printers (SingleLine(OneLine), sentenceDoc, unitDoc, PrintingInformation)

import qualified Data.Map as Map (lookup)
import Control.Monad.State (get)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), colon, empty, parens, render)

-- | Gets a plain renderering of the term for a chunk.
getTermDoc :: (CodeIdea c) => PrintingInformation -> c -> GenState Doc
getTermDoc sm c = do
  g <- get
  let db = codeSpec g ^. systemdbO
  return $ sentenceDoc sm db Implementation OneLine $ phraseNP $ codeChunk c ^. term

-- | Gets a plain rendering of the definition of a chunk, preceded by a colon
-- as it is intended to follow the term for the chunk. Returns empty if the
-- chunk has no definition.
getDefnDoc :: (CodeIdea c) => PrintingInformation -> c -> GenState Doc
getDefnDoc sm c = do
  g <- get
  let db = codeSpec g ^. systemdbO
  return $ maybe empty ((<+>) colon . sentenceDoc sm db Implementation OneLine .
    (^. defn) . fst) (Map.lookup (codeChunk c ^. uid) $ conceptChunkTable db)

-- | Gets a plain rendering of the unit of a chunk in parentheses,
-- or empty if it has no unit.
getUnitsDoc :: (CodeIdea c) => c -> Doc
getUnitsDoc c = maybe empty (parens . unitDoc OneLine . usymb)
  (getUnit $ codeChunk c)

-- | Generates a comment string for a chunk, including the term,
-- definition (if applicable), and unit (if applicable).
getComment :: (CodeIdea c) => PrintingInformation -> c -> GenState String
getComment sm l = do
  t <- getTermDoc sm l
  d <- getDefnDoc sm l
  let u = getUnitsDoc l
  return $ render $ (t <> d) <+> u
