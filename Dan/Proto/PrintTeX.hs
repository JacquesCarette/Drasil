{-# OPTIONS -Wall #-} 
module PrintTeX where

import Prelude hiding (print)
import Data.List (intersperse)
import Text.PrettyPrint hiding (render)

import Control.Monad.Reader

import ASTTeX
import ToTeX
import qualified ASTInternal as A
import qualified Spec as S
import Config (srsTeXParams,lpmTeXParams, --colAwidth,colBwidth,
  tableWidth) --, verboseDDDescription)
import Helpers
import Unicode
import Format (Format(TeX))
import Unit (USymb(..))
import Symbol (Symbol(..))

genTeX :: A.DocType -> S.Document -> Doc
genTeX typ doc = build typ $ makeDocument doc

build :: A.DocType -> Document -> Doc
build A.SRS doc = buildSRS srsTeXParams doc
build A.LPM doc = buildLPM lpmTeXParams doc
build A.Code _  = error "Unimplemented"

buildSRS :: [A.DocParams] -> Document -> Doc
buildSRS ((A.DocClass sb b) : (A.UsePackages ps) : []) (Document t a c) =
  docclass sb b $$ listpackages ps $$ title (pCon Plain t) $$ 
  author (p_spec a) $$ begin $$ print c $$ endL
buildSRS _ _ = error "Invalid syntax in Document Parameters"

buildLPM :: [A.DocParams] -> Document -> Doc
buildLPM  ((A.DocClass sb b) : (A.UsePackages ps) : xs) (Document t a c) =
  docclass sb b $$ listpackages ps $$ moreDocParams xs $$
  title (p_spec t) $$ author (p_spec a) $$ begin $$ print c $$ endL
buildLPM _ _ = error "Invalid syntax in Document Parameters"

moreDocParams :: [A.DocParams] -> Doc
moreDocParams [] = empty
moreDocParams ((A.ExDoc f n):xs) = exdoc f n $$ moreDocParams xs
moreDocParams _ = error "Unexpected document parameters"

listpackages :: [String] -> Doc
listpackages []     = empty
listpackages (p:[]) = usepackage p
listpackages (p:ps) = usepackage p $$ listpackages ps

printLO :: LayoutObj -> Doc
printLO (Section t contents)  = sec (pCon Plain t) $$ print contents
printLO (Paragraph contents)  = text (pCon Plain contents)
printLO (EqnBlock contents)   = text $ makeEquation contents
printLO (Table rows) = makeTable rows
-- printLO (Definition dtype chunk fields) = makeDefn dtype chunk fields

print :: [LayoutObj] -> Doc
print l = foldr ($$) empty $ map printLO l
-- print []                         = empty
-- print ((Section t contents):cs)  = sec (p_spec t) $$ print contents $$ print cs
-- print ((Paragraph contents):cs)  = text (p_spec contents) $$ print cs
-- print ((EqnBlock contents):cs)   = makeEquation contents $$ print cs
-- print ((Table chunks fields):cs) = makeTable chunks fields $$ print cs
-- print ((Definition dtype chunk fields):cs) = makeDefn dtype chunk fields $$ print cs

p_spec :: Spec -> String
-- p_spec (CS c)      = dollar (printSymbol c)
p_spec (E e)      = p_expr e
p_spec (a :+: b)  = p_spec a ++ p_spec b
p_spec (a :-: b)  = p_spec a ++ "_" ++ brace (p_spec b)
p_spec (a :^: b)  = p_spec a ++ "^" ++ brace (p_spec b)
p_spec (a :/: b)  = "\\frac" ++ brace (p_spec a) ++ brace (p_spec b)
-- p_spec (CS c)     = printSymbol c
p_spec (S s)        = s
p_spec (N s)        = symbol s
p_spec (Sy s)       = runReader (uSymbPrint s) Plain

symbol :: Symbol -> String
symbol (Atomic s) = s
symbol (Special s) = render TeX s
symbol (Catenate s1 s2) = (symbol s1) ++ (symbol s2)
--
-- handle the special cases first, then general case
symbol (Corners [] [] [x] [] s) = (symbol s) ++"^"++ (symbol x)
symbol (Corners [] [] [] [x] s) = (symbol s) ++"_"++ (symbol x)
symbol (Corners [_] [] [] [] _) = error "rendering of ul prescript"
symbol (Corners [] [_] [] [] _) = error "rendering of ll prescript"
symbol (Corners _ _ _ _ _) = error "rendering of Corners (general)"

-- unit :: USymb -> String
-- unit (UName n) = symbol n
-- unit (UProd l) = foldr1 (++) (map unit l)
-- unit (UPow n p) = (unit n) ++"^"++ brace (show p)

p_expr :: Expr -> String
p_expr (Var v)    = v
p_expr (Dbl d)    = show d
p_expr (Int i)    = show i
p_expr (Add a b)  = p_expr a ++ "+" ++ p_expr b
p_expr (Sub a b)  = p_expr a ++ "-" ++ p_expr b
p_expr (Mul a b)  = mul a b
p_expr (Frac a b) = fraction (p_expr a) (p_expr b) --Found in Helpers
p_expr (Div a b)  = p_expr a ++ "/" ++ p_expr b
p_expr (Pow a b)  = p_expr a ++ "^" ++ brace (p_expr b)
-- p_expr (C c)      = p_spec $ spec (find A.Equation c "No equation or symbol for chunk")

mul :: Expr -> Expr -> String
mul a b@(Dbl _) = p_expr a ++ "*" ++ p_expr b
mul a b@(Int _) = p_expr a ++ "*" ++ p_expr b
mul a b         = p_expr a ++ p_expr b

makeEquation :: Spec -> String
makeEquation contents = 
  ("\\begin{equation}" ++ p_spec contents ++ "\\end{equation}")
  --TODO: Add auto-generated labels -> Need to be able to ensure labeling based
  --  on chunk (i.e. "eq:h_g" for h_g = ...

makeTable :: [[Spec]] -> Doc
makeTable lls  = text ("~\\newline \\begin{longtable}" ++ brace (header lls)) 
  $$ makeRows lls $$ text "\\end{longtable}"
  where header l = concat (replicate ((length (head l))-1) "l ") ++ "p" ++ 
                        brace (show tableWidth ++ "cm")

makeRows :: [[Spec]] -> Doc
makeRows [] = empty
makeRows (c:cs) = text (makeColumns c) $$ dbs $$ makeRows cs

makeColumns :: [Spec] -> String
makeColumns ls = (concat $ intersperse " & " $ map (pCon Plain) ls) ++ "\\"

-- makeDefn :: A.DType -> A.Chunk TeX -> [A.Field] -> Doc
-- makeDefn _ _ []   = error "No fields provided for data definition"
-- makeDefn A.Data c f = beginDataDefn $$ makeDDTable c f $$ endDataDefn
-- makeDefn A.Literate _ _ = error "makeDefn: missing case"

beginDataDefn :: Doc
beginDataDefn = text "~" <>newline<+> text "\\noindent \\begin{minipage}{\\textwidth}"

endDataDefn :: Doc  
endDataDefn = text "\\end{minipage}" <> dbs


-- READER --

data Context = Equation | EqnB | Plain deriving (Show, Eq)

getCon :: Spec -> Context
getCon (a :+: _) = getCon a
getCon (S _) = Plain
--Not using a catchall for now.
getCon (E _) = Equation
getCon (_ :-: _) = Equation --Subscripts and superscripts must be in Equation ctxt.
getCon (_ :^: _) = Equation
getCon (_ :/: _) = Equation -- Fractions are always equations.
getCon (Sy _) = Plain
getCon (N _) = Equation


lPrint :: Spec -> Reader Context String
lPrint t@(a :+: b) = do
  c <- ask
  let ca = getCon a
  let cb = getCon b
  case c of
    EqnB -> return $ makeEquation t
    _    -> 
        case b of
          (_ :+: _) ->
      -- if c == ca then
            if ca == cb then
              return $ p_spec a ++ pCon cb b
            else
              return $ p_spec a ++ "$" ++ pCon cb b
          _ ->
            if ca == cb then
              case ca of
                Equation -> return $ p_spec a ++ p_spec b ++ "$"
                Plain    -> return $ p_spec a ++ p_spec b
                _        -> error "This can't happen"
            else
              case cb of
                Equation -> return $ p_spec a ++ pCon cb b
                Plain    -> return $ "$" ++ p_spec b
                _        -> error "This can't happen"
          
lPrint t = do
  c <- ask
  let ct = getCon t
  case c of
    EqnB -> return $ makeEquation t
    _ ->
      case ct of
        Equation -> return $ dollar (p_spec t)
        Plain    -> return $ p_spec t
        EqnB     -> return $ makeEquation t --This will never run right now, but maybe eventually.

bEq, eEq :: String    
bEq = "\\begin{equation} " 
eEq = "\\end{equation}"

pCon :: Context -> Spec -> String
pCon = \c t -> runReader (lPrint t) c

uSymbPrint :: USymb -> Reader Context String --To fix unit printing will need this.
uSymbPrint (UName n) = do
  c <- ask
  let cn = getSyCon n
  if c == cn then
    return $ symbol n
  else
    case cn of
      Equation -> return $ dollar $ symbol n 
        --Need a way to parse/print symbols using their context for units, once I figure out the getSyCon part below.
      _ -> return $ symbol n
uSymbPrint (UProd l) = do
  c <- ask
  return $ foldr1 (++) (map ((\ctxt t -> runReader t ctxt) c) (map uSymbPrint l))
uSymbPrint (UPow n p) = do
  c <- ask
  case c of
    Plain -> return $ runReader (uSymbPrint n) c ++ dollar ("^" ++ brace (show p))
    _ -> return $ runReader (uSymbPrint n) c ++ "^" ++ brace (show p)

getSyCon :: Symbol -> Context
getSyCon (Atomic _) = Plain
--getSyCon (Special Circle) = Equation --Need to figure this out.
getSyCon (Special _) = Plain
getSyCon (Catenate s1 _) = getSyCon s1
getSyCon (Corners _ _ _ _ s) = getSyCon s

--- END READER ---






-- makeDDTable :: A.Chunk TeX -> [A.Field] -> Doc
-- makeDDTable c f = vcat [
  -- text $ "\\begin{tabular}{p{"++show colAwidth++"\\textwidth} p{"++show colBwidth++"\\textwidth}}",
  -- text $ "\\toprule \\textbf{Refname} & \\textbf{DD:$"++ (printSymbol Pg c) ++"$}",
  -- text $ "\\label{DD:" ++ (printSymbol Code c) ++ "}",
  -- makeDDRows c f, dbs <+> text ("\\bottomrule \\end{tabular}")
  -- ]

-- makeDDRows :: A.Chunk TeX -> [A.Field] -> Doc
-- makeDDRows _ [] = error "No fields to create DD table"
-- makeDDRows c (f@(A.Symbol):[]) = ddBoilerplate f (text (p_spec (CS c)))
-- makeDDRows c (f@(A.Symbol):fs) = 
  -- ddBoilerplate f (text (p_spec (spec (A.CS c)))) $$ makeDDRows c fs
-- makeDDRows c (f@(A.Description):[]) = ddBoilerplate f $ writeDesc c $$
  -- (if (verboseDDDescription) then (newline $$ descDependencies c)
  -- else empty)
-- makeDDRows c (f@(A.Description):fs) = ddBoilerplate f $ writeDesc c $$
  -- (if (verboseDDDescription) then (newline $$ descDependencies c)
  -- else empty) $$ makeDDRows c fs
-- makeDDRows c (f:[]) = ddBoilerplate f $ ddWritetext f c
-- makeDDRows c (f:fs) = ddBoilerplate f (ddWritetext f c) $$ makeDDRows c fs

-- printSymbol :: A.Chunk TeX -> String
-- printSymbol chunk =
  -- p_spec $ spec (find A.Symbol chunk "Error: No symbol for chunk")

-- ddBoilerplate :: A.Field -> Doc -> Doc
-- ddBoilerplate = \f -> \t -> dbs <+> text "\\midrule" <+> dbs $$ text 
  -- (writeField f ++ " & ") <> t
  
-- ddWritetext :: A.Field -> A.Chunk TeX -> Doc
-- ddWritetext = \f -> \c -> text (p_spec (spec (find f c ("Error: missing field" ++ 
  -- writeField f ++ " in chunk " ++ printSymbol Code c))))

-- descDependencies :: A.Chunk TeX -> Doc
-- descDependencies c = writeDescs (unSpec (maybe (S "") spec deps))
 -- where
   -- deps = findOptional A.Dependencies c

-- unSpec :: Spec -> A.Chunks TeX
-- unSpec (D cs) = cs
-- unSpec (S _) = []
-- unSpec _     = error "oh my, what are we doing here?"

-- writeDescs :: A.Chunks TeX -> Doc
-- writeDescs [] = error "Nothing to write" --Might change this in case chunk has no dependencies
-- writeDescs (c:[]) = writeDesc c
-- writeDescs (c:cs) = writeDesc c <+> newline $$ writeDescs cs

-- writeDesc :: A.Chunk TeX -> Doc
-- writeDesc c  = text $ 
  -- p_spec (CS c) ++ " is the " ++ p_spec (spec (find A.Description c "Missing Description"))

