module Language.Drasil.Code.Imperative.Import where

import Language.Drasil.Code.Code as C
import Language.Drasil.Code.Imperative.AST as I
import Language.Drasil.Code.Imperative.LanguageRenderer (Options(..))
import Language.Drasil.Code.Imperative.Parsers.ConfigParser (pythonLabel)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code
import Language.Drasil.Expr as E
import Language.Drasil.Expr.Extract
import Language.Drasil.CodeSpec

import Prelude hiding (log, exp, return)

generateCode :: CodeSpec -> IO () 
generateCode spec = let modules = genModules spec 
   in createCodeFiles $ makeCode 
        pythonLabel
        (Options Nothing Nothing Nothing (Just "Code")) 
        (map moduleName modules) 
        (toAbsCode (codeName $ program spec) modules)

genModules :: CodeSpec -> [Module]
genModules (CodeSpec _ _ _ d) = [genCalcMod d]

genCalcMod :: [CodeDefinition] -> Module
genCalcMod defs = buildModule "Calculations" [] [] (genCalcFuncs defs) []     
        
genCalcFuncs :: [CodeDefinition] -> [Method]
genCalcFuncs = map 
  ( \x -> pubMethod 
            (methodType $ convType (codeType x)) 
            ("calc_" ++ codeName x) 
            (map 
              (\y -> param (codeName y) (convType $ codeType y)) 
              (codevars $ codeEquat x)) 
            (genCalcBlock x)
  )

genCalcBlock :: CodeDefinition -> Body
genCalcBlock def 
  | isCase (codeEquat def) = genCaseBlock $ getCases (codeEquat def)
  | otherwise              = oneLiner $ return $ convExpr (codeEquat def)
  where isCase (Case _) = True
        isCase _        = False
        getCases (Case cs) = cs
        getCases _         = error "impossible to get here"


genCaseBlock :: [(Expr,Relation)] -> Body
genCaseBlock cs = oneLiner $ ifCond (genIf cs) noElse
  where genIf :: [(Expr,Relation)] -> [(Value,Body)]
        genIf = map 
          (\(e,r) -> (convExpr r, oneLiner $ return (convExpr e)))
        
convType :: C.CodeType -> I.StateType
convType C.Boolean = bool
convType C.Integer = int
convType C.Float = float
convType C.Char = char
convType C.String = string
convType (C.List t) = listT $ convType t
convType (C.Object n) = obj n
convType _ = error "No type conversion"

convExpr :: Expr -> Value
convExpr (Dbl d)      = litFloat d
convExpr (Int i)      = litInt i
convExpr (Bln b)      = litBool b
convExpr (a :/ b)     = (convExpr a) #/ (convExpr b)
convExpr (a :* b)     = (convExpr a) #* (convExpr b)
convExpr (a :+ b)     = (convExpr a) #+ (convExpr b)
convExpr (a :^ b)     = (convExpr a) #^ (convExpr b)
convExpr (a :- b)     = (convExpr a) #- (convExpr b)
convExpr (a :. b)     = (convExpr a) #* (convExpr b)
convExpr (a :&& b)    = (convExpr a) ?&& (convExpr b)
convExpr (a :|| b)    = (convExpr a) ?|| (convExpr b)
convExpr (Deriv _ _ _) = error "not implemented"
convExpr (E.Not e)      = (?!) (convExpr e)
convExpr (Neg e)      = (#~) (convExpr e)
convExpr (C c)        = var (codeName (SFCN c))
convExpr (FCall (C c) x)  = funcApp' (codeName (SFCN c)) (map convExpr x)
convExpr (FCall _ _)  = error "not implemented"
convExpr (a := b)     = (convExpr a) ?== (convExpr b)
convExpr (a :!= b)    = (convExpr a) ?!= (convExpr b)
convExpr (a :> b)     = (convExpr a) ?> (convExpr b)
convExpr (a :< b)     = (convExpr a) ?< (convExpr b)
convExpr (a :<= b)    = (convExpr a) ?<= (convExpr b)
convExpr (a :>= b)    = (convExpr a) ?>= (convExpr b)
convExpr (UnaryOp u)  = unop u
convExpr (Grouping e) = convExpr e
convExpr (BinaryOp _) = error "not implemented"
convExpr (Case _)     = error "Case should be dealt with separately"
convExpr _            = error "not implemented"

unop :: UFunc -> Value
unop (E.Log e)          = I.log (convExpr e)
unop (E.Abs e)          = (#|) (convExpr e)
unop (E.Exp e)          = I.exp (convExpr e)
unop _                  = error "not implemented"
