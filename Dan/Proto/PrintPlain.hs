{-# OPTIONS -Wall #-} 
module PrintPlain where
import ASTPlain
import ToPlain
import qualified Data.Map.Strict as Map
import Data.Maybe
import ASTInternal (Chunk)
import qualified ASTInternal as AST
import Prelude hiding (print)

plaintext :: Chunk -> String
plaintext c = print $
  fromMaybe (fromMaybe (error "Chunk is missing requisite fields") 
  (Map.lookup AST.Symbol c)) (Map.lookup AST.VarName c)
-- Need to fully implement.

print :: AST.FDesc -> String
print (AST.E e)     = p_expr $ expr e
print (AST.S s)     = s
print (AST.U u)     = uni u
print (a AST.:-: b) = print a ++ "_" ++ print b
print (a AST.:^: b) = print a ++ "^" ++ print b
print (AST.M unit)  = writeUnit unit
print (AST.Empty)   = ""

p_expr :: Expr -> String
p_expr (C c) = print $ fromMaybe (AST.Empty) (Map.lookup AST.Equation c)
p_expr (Dbl d)  = show d
p_expr (Int i)  = show i
p_expr (Mul a b) = p_expr a ++ "*" ++ p_expr b
p_expr (Add a b) = "(" ++ p_expr a ++ "+" ++ p_expr b ++ ")"
p_expr (Div a b) = p_expr a ++ "/" ++ p_expr b
p_expr (Var v) = v
p_expr (Pow a b) = p_expr a ++ "^(" ++ p_expr b ++ ")"
p_expr (Sub a b) = "(" ++ p_expr a ++ "-" ++ p_expr b ++ ")"


-- --This function should be moved elsewhere, preferably somewhere accessible to
  -- --the recipes, should also be changed to use the internal AST instead of Doc
-- get :: AST.FName -> Chunk -> AST.Context -> Doc
-- get name chunk con = text $ getStr name chunk con

-- --This function can be collapsed into get. Functionality may need to be tweaked.
-- getStr :: AST.FName -> Chunk -> AST.Context -> String
-- getStr AST.Equation chunk con = 
  -- format con (fromMaybe (fromMaybe (error "wut") (Map.lookup AST.VarName chunk)) 
    -- (Map.lookup AST.Equation chunk))
-- getStr name chunk con = format con (fromMaybe (AST.Empty) (Map.lookup name chunk))

uni :: AST.Unicode -> String
uni (AST.Tau_L) = "tau"
uni (AST.Tau_U) = "Tau"
uni (AST.Alpha_L) = "alpha"
uni (AST.Alpha_U) = "Alpha"
uni (AST.Circle) = "(deg)"
--uni _ = error "Invalid unicode character selection"
  
-- writeDep :: [AST.FName] -> AST.Dependency -> String -> String -> AST.Context -> [Doc]
-- writeDep [] _ _ _ _ = [empty]
-- writeDep _ [] _ _ _ = [empty]
-- writeDep (x:[]) (c:[]) _ es con = 
  -- [get x c con <+> text es]
-- writeDep (x:[]) (c:_) _ es con= 
  -- [get x c con <+> text es]
-- writeDep (x:xs) (c:[]) is es con= 
  -- [get x c con <+> text is] ++ writeDep xs [c] is es con
-- writeDep (x:xs) (c:cs) is es con= 
  -- writeDep (x:xs) (c:[]) is es con ++ writeDep (x:xs) cs is es con

writeUnit :: AST.Unit -> String  
writeUnit (AST.Fundamental s) = s
writeUnit (AST.Derived s e) = s ++ " = " ++ pU_expr (expr e)

-- printSIU :: [Chunk] -> [AST.FName] -> Doc -> Doc -> [Doc]
-- printSIU [] _ _ _ = [empty]
-- printSIU _ [] _ _ = [empty]
-- printSIU (c:cs) (x:xs) bet aft = 
  -- [unitSymbol (fromMaybe (AST.Empty) (Map.lookup AST.SIU c)) <+> bet <+>
  -- get x c AST.Pg] ++ gets xs c AST.Pg ++ [aft] ++ printSIU cs (x:xs) bet aft

-- gets :: [AST.FName] -> Chunk -> AST.Context -> [Doc]
-- gets [] _ _ = [empty]
-- gets (x:xs) c con = [get x c con] ++ gets xs c con

-- unitSymbol :: AST.FDesc -> Doc
-- unitSymbol (AST.M (AST.Derived s _)) = text s
-- unitSymbol (AST.M (AST.Fundamental s)) = text s
-- unitSymbol _ = empty

pU_expr :: Expr -> String
pU_expr (C c) = print $ fromMaybe (AST.Empty) (Map.lookup AST.SIU c)
pU_expr (Dbl d)  = show d
pU_expr (Int i)  = show i
pU_expr (Mul a b) = pU_expr a ++ "*" ++ pU_expr b
pU_expr (Add a b) = "(" ++ pU_expr a ++ "+" ++ pU_expr b ++ ")"
pU_expr (Div a b) = pU_expr a ++ "/" ++ pU_expr b
pU_expr (Var v) = v
pU_expr (Pow a b) = pU_expr a ++ "^(" ++ pU_expr b ++ ")"
pU_expr (Sub a b) = "(" ++ pU_expr a ++ "-" ++ pU_expr b ++ ")"
