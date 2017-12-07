{-# LANGUAGE GADTs,Rank2Types #-}

module Language.Drasil.Chunk.Method(MethodChunk(..), MethodType(..)
  , ExcType(..), IOType(..), fromEC, makeStdInputMethod, makeFileInputMethod
  , makeFileOutputMethod, makeMainMethod) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Expr
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..))
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Wrapper (nw, NWrapper)
import Language.Drasil.Chunk.Quantity (Quantity(..), qw, QWrapper)
import Language.Drasil.ChunkDB
import Language.Drasil.Expr.Extract

import qualified Language.Drasil.Code.Imperative.AST as A

-- BEGIN METHODCHUNK --
data MethodChunk = MeC
  { methcc :: NWrapper   -- Name
  , mType :: MethodType  -- Type
  , input :: [QWrapper]  -- inputs
  , output :: [QWrapper] -- outputs
  , exc :: [ExcType]     -- exceptions
  }

data MethodType where 
  MCalc :: QDefinition -> MethodType
  MInput :: (Quantity q) => IOType -> q -> MethodType
  MOutput :: (Quantity q) => IOType -> [q] -> MethodType
  -- temporary for generating control module:
  MCustom :: A.Body -> MethodType

data IOType = IOStd
            | IOFile String

data ExcType = DivByZero

instance Show ExcType where
  show DivByZero = "Divide By Zero"

instance Chunk MethodChunk where
  id = cl . id

instance NamedIdea MethodChunk where
  term = cl . term
  getA mc = getA (methcc mc)

-- END METHODCHUNK --

-- don't export this
cl :: Simple Lens MethodChunk NWrapper
cl f (MeC a b c d e) = fmap (\x -> MeC x b c d e) (f a)

--FIXME? Added a hack (pattern match) to make the modified EqChunk work
fromEC :: HasSymbolTable ctx => QDefinition -> ctx -> MethodChunk
fromEC qd@(EC a b _) m =
  let exc' = if (checkDiv $ b) then [DivByZero] else []
  in  MeC (nw a) (MCalc $ qd) (vars b m)
      [qw a] exc'

makeStdInputMethod :: (Quantity q) => q -> MethodChunk
makeStdInputMethod q' = 
  let n = nw q
      q = qw q'
  in  MeC n (MInput IOStd q) [] [q] []

makeFileInputMethod :: (Quantity q, NamedIdea n) => n -> q -> String -> MethodChunk
makeFileInputMethod n' q' f = 
  let n = nw n'
      q = qw q'
  in  MeC n (MInput (IOFile f) q) [q] [] []

makeFileOutputMethod :: (Quantity q, NamedIdea n) => n -> [q] -> String -> MethodChunk
makeFileOutputMethod n' qs' f = 
  let n  = nw n'
      qs = map qw qs'
  in  MeC n (MOutput (IOFile f) qs) qs [] []

makeMainMethod :: (NamedIdea n) => n -> A.Body -> MethodChunk
makeMainMethod n b = MeC (nw n) (MCustom b) [] [] []

-- don't export
checkDiv :: Expr -> Bool
checkDiv (b :/ e) = if   (not $ null $ dep e)
                    then True
                    else checkDiv b || checkDiv e
checkDiv (b :^ e) = checkDiv b || checkDiv e
checkDiv (b :* e) = checkDiv b || checkDiv e
checkDiv (b :+ e) = checkDiv b || checkDiv e
checkDiv (b :- e) = checkDiv b || checkDiv e
checkDiv _        = False
