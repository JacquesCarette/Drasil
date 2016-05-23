import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

data Context = Equation | EqnBlock | Plain deriving (Show, Eq)

data Term = Eqn String
          | Text String
          | Block [Term]
  deriving (Show, Eq)
  
type Terms = [Term]
  

gcontext :: Reader Term Context
gcontext = do
  term <- ask
  return $ getCon term
  
getCon :: Term -> Context
getCon (Eqn _) = Equation
getCon (Text _) = Plain
getCon (Block (t:ts)) = getCon t

pcontext :: Reader Context (Term -> String)
pcontext = do
  c1 <- ask
  case c1 of 
    Plain -> return $ print'
    _ -> return $ dollar . print'
  
print' :: Term -> String
print' (Eqn s) = s
print' (Text s) = s

dollar = \x -> "$" ++ x ++ "$"

sample1 = Eqn "x = 5"
sample2 = Text "This is text"

getConAndPrint = \x -> runReader pcontext (runReader gcontext x) x

main = do
  mapM_ putStrLn $ map getConAndPrint [sample1,sample2]
  putStrLn $ runReader (foo $ Block $ (Text "Hello here is an equation: "):(Eqn "x = 5"):[]) Plain
  putStrLn $ runReader (foo $ Block $ (Eqn "x = 5*"):(Eqn "h_g"):(Eqn "+"):(Eqn "k_c"):[]) (EqnBlock)
  
foo :: Term -> Reader Context String
foo (Block []) = do
  c <- ask
  case c of
    Plain    -> return ""
    Equation -> return "$"
    EqnBlock -> return eEq
foo (Block (t:ts)) = do
  c <- ask
  let c2 = getCon t
  case c of 
    EqnBlock -> return $ bEq ++ (print' t) ++ runReader (foo $ Block ts) c2 ++ eEq
    _ -> if c == c2 then
          return $ (print' t) ++ runReader (foo $ Block ts) c2
         else
          case c2 of 
            Plain -> return $ (print' t) ++ (runReader (foo $ Block ts) c2) ++ "$"
            Equation -> return $ ("$" ++ print' t) ++ (runReader (foo $ Block ts) c2)
foo t = do
  c <- ask
  case c of
    EqnBlock -> return $ bEq ++ print' t ++ eEq
    Equation -> return $ dollar (print' t)
    Plain    -> return $ print' t
    
bEq = "\\begin{equation} " 
eEq = "\\end{equation}"
    
-- Plain -> return $ (print' t) ++ runReader (foo $ Block ts) c
-- Equation -> return $ dollar $ (print' t) ++ runReader (foo $ Block ts) c  