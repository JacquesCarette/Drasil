-- | Contains functions for generating code comments that describe a chunk.
module Language.Drasil.Code.Imperative.Comments (
  getComment, getCommentBrief
) where

import Control.Monad.State (get)
import Control.Lens ((^.))
import Text.PrettyPrint.HughesPJ (Doc, (<+>), colon, empty, parens, render)

import Drasil.Database (HasUID(..))
import Drasil.Database.SearchTools (DomDefn (definition), defResolve')
import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.CodeSpec (HasOldCodeSpec(..))
import Language.Drasil.Printers (SingleLine(OneLine), sentenceDoc, unitDoc)

-- | Gets a plain renderering of the term for a chunk.
getTermDoc :: (CodeIdea c) => c -> GenState Doc
getTermDoc c = do
  g <- get
  return $ sentenceDoc (printfo g) OneLine $ phrase $ codeChunk c

-- | Gets a plain rendering of the definition of a chunk, preceded by a colon
-- as it is intended to follow the term for the chunk. Returns empty if the
-- chunk has no definition.
getDefnDoc :: (CodeIdea c) => c -> GenState Doc
getDefnDoc c = do
  g <- get
  let db = codeSpec g ^. systemdbO
  return $ ((<+>) colon . sentenceDoc (printfo g) OneLine)
    (definition $ defResolve' db (codeChunk c ^. uid))

-- | Gets a plain rendering of the unit of a chunk in parentheses,
-- or empty if it has no unit.
getUnitsDoc :: (CodeIdea c) => c -> Doc
getUnitsDoc c = maybe empty (parens . unitDoc OneLine . usymb)
  (getUnit $ codeChunk c)

-- | Generates a comment string for a chunk, including the term,
-- definition (if applicable), and unit (if applicable).
getComment :: (CodeIdea c) => c -> GenState String
getComment l = do
  t <- getTermDoc l
  d <- getDefnDoc l
  let u = getUnitsDoc l
  return $ render $ (t <> d) <+> u

getCommentBrief :: (CodeIdea c) => c -> GenState String
getCommentBrief l = do
  t <- getTermDoc l
  let u = getUnitsDoc l
  return $ render $ t <+> u
