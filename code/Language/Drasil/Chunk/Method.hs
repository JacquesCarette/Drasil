module Language.Drasil.Chunk.Method(MethodChunk(..), MethodType(..),
  ExcType(..), IOType(..), fromEC, makeStdInputMethod) where

import Control.Lens (Simple, Lens, (^.))
import Prelude hiding (id)
import Language.Drasil.Expr
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Eq
import Language.Drasil.Expr.Extract

-- BEGIN METHODCHUNK --
data MethodChunk = MeC { methcc :: NamedChunk, mType :: MethodType,
                         input :: [VarChunk], output :: [VarChunk],
                         exc :: [ExcType] }

data MethodType = MCalc QDefinition
                | MInput IOType VarChunk
                | MOutput IOType VarChunk

data IOType = IOStd
            | IOFile

data ExcType = DivByZero

instance Show ExcType where
  show DivByZero = "Divide By Zero"

instance Chunk MethodChunk where
  id = cl . id

instance NamedIdea MethodChunk where
  term = cl . term

-- END METHODCHUNK --

-- don't export this
cl :: Simple Lens MethodChunk NamedChunk
cl f (MeC a b c d e) = fmap (\x -> MeC x b c d e) (f a)

--FIXME? Added a hack (pattern match) to make the modified EqChunk work
fromEC :: QDefinition -> MethodChunk
fromEC ec@(EC a b) =
  let exc' = if (checkDiv $ b) then [DivByZero] else []
  in  MeC (toCC $ a) (MCalc $ ec) (vars $ b)
      [(toVC $ a)] exc'

makeStdInputMethod :: VarChunk -> MethodChunk
makeStdInputMethod vc = MeC (toCC $ vc) (MInput IOStd vc) [] [vc] []

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


toCC :: NamedIdea c => c -> NamedChunk
toCC c = CC (c ^. id) (c ^. term)