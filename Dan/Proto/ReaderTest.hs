import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

data Context = Equation | Plain deriving (Show, Eq)

data Term = Eqn String
          | Text String
  deriving (Show, Eq)
  
type Terms = [Term]
  

gcontext :: Reader Term Context
gcontext = do
  term <- ask
  return $ getCon term
  
getCon :: Term -> Context
getCon (Eqn _) = Equation
getCon (Text _) = Plain

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
  