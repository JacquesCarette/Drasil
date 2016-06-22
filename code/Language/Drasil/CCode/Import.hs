module Language.Drasil.CCode.Import where

import Control.Lens

import Language.Drasil.CCode.AST
import Language.Drasil.CCode.Helpers
import qualified Language.Drasil.Expr as E
import Language.Drasil.Expr.Extract (dep)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk (name)
import Data.List (nub)

-- for testing
--toCode :: Lang -> CodeType -> EqChunk -> Code
--toCode CLang Calc ec = C [StdLibHeader, StdIOHeader] [VarDecl IntType "gVar", VarDecl DblType "gVar2"] [(MethodDecl DblType "test" [VarDecl DblType "a", VarDecl DblType "b"], [Declare DblType "f" (Just $ Dbl 20), Assign "a" (Int $ -1), If (Leq (Var "a") (Var "b")) [(Return (Int 1))] (Just [(Return (Int 0))]), Return (Var "a")])]

toCode :: Lang -> CodeType -> [EqChunk] -> String -> Code
-- hard-coded standard header file includes for now
toCode CLang Calc ecs moduleName    = let methods = map (\x -> makeMethod x moduleName) ecs
                                          testMethods = map makeTest methods
                                          testMain = makeTestMain testMethods
                                      in C (stdHeaders ++ testHeaders) [errorDecl] (methods ++ testMethods ++ [errorMethod moduleName] ++ [testMain])
-- toCode _ _ _ = error "Unimplemented code translation in ToCode.hs"

makeMethod :: EqChunk -> String -> Method
makeMethod ec moduleName = ((MethodDecl DblType (moduleName ++ "_" ++ (ec ^. name)) (makeArgs $ dep (equat ec))),
                            (makeDivZeroGuards $ getDenoms $ equat ec) ++ [Assign errorCode (Int 0), Return (makeCode $ equat ec)])


makeTest :: Method -> Method
makeTest m = ((MethodDecl VoidType ("test_"++(getMethodName m)) []), [])

makeTestMain :: [Method] -> Method
makeTestMain tests = ((MethodDecl IntType "main" []), [MethodCall testBegin []] ++ (map (\x -> MethodCall runTest [Var $ getMethodName x]) tests) ++ [MethodCall testEnd []])

makeArgs :: [String] -> [VarDecl]
makeArgs = map (VarDecl DblType)


--makeArgs :: [String] -> [ArgsDecl]
--makeArgs = map (ArgsDecl DblType)

-- Currently assuming type of double for arguments as all examples involve
  -- calculations using known (or calculated) values.
-- TODO: Add either a toggle or a field in chunks to declare their precision
  
makeCode :: E.Expr -> CodeExpr
makeCode (E.V v)    = Var v
makeCode (E.Dbl d)  = Dbl d
makeCode (E.Int i)  = Int i
makeCode (E.C c)    = Var (c ^. name)
makeCode (b E.:^ e) = Pow (makeCode b) (makeCode e)
makeCode (b E.:* e) = Mult (makeCode b) (makeCode e)
makeCode (b E.:/ e) = Div (makeCode b) (makeCode e)
makeCode (b E.:+ e) = Add (makeCode b) (makeCode e)
makeCode (b E.:- e) = Sub (makeCode b) (makeCode e)

makeDivZeroGuards :: [E.Expr] -> [Statement]
makeDivZeroGuards []   = []
makeDivZeroGuards (e:es) = (If (Eq (makeCode e) (Int 0)) [Assign errorCode (Int 1), Return (Int 0)] Nothing):makeDivZeroGuards es

-- list of expressions that occur in denominators for guarding against div by zero (duplicates removed)
-- the list is reversed so that "deeper" denominators are guarded first (so that a denominator test itself
-- does not contain a div by zero error!)
getDenoms :: E.Expr -> [E.Expr]
getDenoms = reverse . nub . getDenoms'
            where   getDenoms' (b E.:/ e) = e : (getDenoms b ++ getDenoms e)
                    getDenoms' (b E.:^ e) = getDenoms b ++ getDenoms e
                    getDenoms' (b E.:* e) = getDenoms b ++ getDenoms e
                    getDenoms' (b E.:+ e) = getDenoms b ++ getDenoms e
                    getDenoms' (b E.:- e) = getDenoms b ++ getDenoms e
                    getDenoms' _ = []
