module Language.Drasil.Chunk.Method(MethodChunk(..), MethodType(..)
  , ExcType(..), IOType(..), fromEC, makeStdInputMethod, makeFileInputMethod
  , makeFileOutputMethod, makeMainMethod) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Expr
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), NamedChunk)
import Language.Drasil.Chunk.VarChunk (VarChunk)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Wrapper (nw, NWrapper)
import Language.Drasil.ChunkDB
import Language.Drasil.Expr.Extract

import qualified Language.Drasil.Code.Imperative.AST as A

-- BEGIN METHODCHUNK --
data MethodChunk = MeC
  { methcc :: NWrapper   -- Name
  , mType :: MethodType  -- Type
  , input :: [VarChunk]  -- inputs
  , output :: [VarChunk] -- outputs
  , exc :: [ExcType]     -- exceptions
  }

data MethodType = MCalc QDefinition
                | MInput IOType VarChunk
                | MOutput IOType [VarChunk]
                -- temporary for generating control module
                | MCustom A.Body

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
fromEC ec@(EC a b) m =
  let exc' = if (checkDiv $ b) then [DivByZero] else []
  in  MeC (nw a) (MCalc $ ec) (vars b m)
      [(toVC a m)] exc'

makeStdInputMethod :: VarChunk -> MethodChunk
makeStdInputMethod vc = MeC (nw vc) (MInput IOStd vc) [] [vc] []

makeFileInputMethod :: NamedChunk -> VarChunk -> String -> MethodChunk
makeFileInputMethod cc' vc f = MeC (nw cc') (MInput (IOFile f) vc) [vc] [] []

makeFileOutputMethod :: NamedChunk -> [VarChunk] -> String -> MethodChunk
makeFileOutputMethod cc' vcs f = MeC (nw cc') (MOutput (IOFile f) vcs) vcs [] []

makeMainMethod :: NamedChunk -> A.Body -> MethodChunk
makeMainMethod cc' b = MeC (nw cc') (MCustom b) [] [] []

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
