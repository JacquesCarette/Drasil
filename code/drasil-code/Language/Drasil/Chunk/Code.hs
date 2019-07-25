{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeChunk(..), codeType, codevar, codefunc,
    ConstraintMap, constraintMap, physLookup, sfwrLookup,
    programName, symbToCodeName, toCodeName, funcPrefix
  ) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil

import Language.Drasil.Code.Code (CodeType, spaceToCodeType)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType(ctyp), CodeQuantityDict, 
  cqw)

import Data.String.Utils (replace)
import qualified Data.Map as Map

-- not using lenses for now
class CodeIdea c where
  codeName      :: c -> String
  codeChunk     :: c -> CodeChunk

programName :: CommonIdea c => c -> String
programName = toCodeName . abrv

symbToCodeName :: Symbol -> String
symbToCodeName (Atomic s) = toCodeName s
symbToCodeName (Special sp) = specialToCodeName sp
--symbToCodeName (Greek g) = greekToCodeName g
symbToCodeName (Atop d s) = decorate (symbToCodeName s) d
symbToCodeName (Corners ul ll ur lr b) =
  cleft ul ++ cleft ll ++ symbToCodeName b
    ++ cright lr ++ cright ur
  where cleft :: [Symbol] -> String
        cleft [] = ""
        cleft (s:syms) = symbToCodeName s ++ "_" ++ cleft syms
        cright :: [Symbol] -> String
        cright [] = ""
        cright (s:syms) = "_" ++ symbToCodeName s ++ cright syms
symbToCodeName (Concat s) = concatMap symbToCodeName s
symbToCodeName Empty = ""

decorate :: String -> Decoration -> String
decorate s Hat = s ++ "_hat"
decorate s Vector = s ++ "_vect"
decorate s Prime = s ++ "'"

-- TODO: Double check that this is valid in all output languages
specialToCodeName :: Special -> String
specialToCodeName Circle        = "circ"
specialToCodeName Partial       = "partial"

toCodeName :: String -> String
toCodeName s =
    let illegalChars = [
            ",","~","`","-","=","!","@","#","$","%","^","&","*","(",")","+",
            "[","]","\\",";","'",".","/","|",":","\"","<",">","?"," "]
    in foldl varNameReplace s illegalChars
    where  varNameReplace :: String -> String -> String
           varNameReplace l old = replace old "_" l

funcPrefix :: String
funcPrefix = "func_"
 
data VarOrFunc = Var | Func
data CodeChunk = CodeC { _qc :: CodeQuantityDict
                       , kind :: VarOrFunc
                       }
makeLenses ''CodeChunk

instance HasUID      CodeChunk where uid = qc . uid
instance NamedIdea   CodeChunk where term = qc . term
instance Idea        CodeChunk where getA = getA . view qc
instance HasCodeType CodeChunk where ctyp = qc . ctyp
instance HasSymbol   CodeChunk where symbol c = symbol (c ^. qc)
instance CodeIdea    CodeChunk where
  codeName (CodeC c Var) = symbToCodeName (codeSymb c)
  codeName (CodeC c Func) = funcPrefix ++ symbToCodeName (codeSymb c)
  codeChunk = id
instance Eq          CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit CodeChunk where getUnit = getUnit . view qc

codeType :: HasSpace c => c -> CodeType
codeType c = spaceToCodeType $ c ^. typ

codevar :: (Quantity c, MayHaveUnit c) => c -> CodeChunk
codevar c = CodeC (cqw c) Var

codefunc :: (Quantity c, MayHaveUnit c) => c -> CodeChunk
codefunc c = CodeC (cqw c) Func

type ConstraintMap = Map.Map UID [Constraint]

constraintMap :: (HasUID c, Constrained c) => [c] -> ConstraintMap
constraintMap = Map.fromList . map (\x -> (x ^. uid, x ^. constraints))

physLookup :: (Quantity q) => ConstraintMap -> q -> (q,[Constraint])
physLookup m q = constraintLookup' q m (filter isPhysC)

sfwrLookup :: (Quantity q) => ConstraintMap -> q -> (q,[Constraint])
sfwrLookup m q = constraintLookup' q m (filter isSfwrC)

constraintLookup' :: (Quantity q) => q -> ConstraintMap
                      -> ([Constraint] -> [Constraint]) -> (q , [Constraint])
constraintLookup' q m filt = (q, maybe [] filt (Map.lookup (q^.uid) m))
