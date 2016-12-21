module Language.Drasil.Chunk.Method(MethodChunk(..), MethodType(..),
  ExcType(..), IOType(..), fromEC, makeStdInputMethod) where

import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Expr
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Eq
import Language.Drasil.Expr.Extract

-- BEGIN METHODCHUNK --
data MethodChunk = MeC { methcc :: ConceptChunk, mType :: MethodType,
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
  name = cl . name

instance Concept MethodChunk where
  descr = cl . descr

-- END METHODCHUNK --

-- don't export this
cl :: Simple Lens MethodChunk ConceptChunk
cl f (MeC a b c d e) = fmap (\x -> MeC x b c d e) (f a)


fromEC :: QDefinition -> MethodChunk
fromEC ec = let exc' = if (checkDiv $ equat ec) then [DivByZero] else []
            in  MeC (toCC $ uc ec) (MCalc $ ec) (vars $ equat ec)
                  [(toVC $ uc ec)] exc'

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


toCC :: Concept c => c -> ConceptChunk
toCC c = CC (c ^. name) (c ^. descr)