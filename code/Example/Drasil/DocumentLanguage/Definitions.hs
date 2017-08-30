{-# Language GADTs #-}

module Drasil.DocumentLanguage.Definitions 
  ( Fields
  , Field(..)
  , Verbosity(..)
  , tmodel
  , ddefn
  , gdefn
  , InclUnits(..)
  )where

import Language.Drasil
import Drasil.DocumentLanguage.Chunk.GenDefn

import Control.Lens ((^.))

import Prelude hiding (id)

-- | Synonym for a list of 'Field'
type Fields = [Field]

-- | Fields that should be displayed in definitions
data Field = Label 
           | Symbol
           | Units
           | DefiningEquation
           | Description Verbosity InclUnits VerbatimIntro
           -- --| Sources --TODO
           -- --| RefBy --TODO
           
data Verbosity = Verbose  -- Full Descriptions
               | Succinct -- Simple Description (do not redefine other symbols)

data InclUnits = IncludeUnits -- In description field (for other symbols)
               | IgnoreUnits
               
type VerbatimIntro = Sentence

-- | Create a theoretical model using a list of fields to be displayed, a SymbolMap,
-- and a RelationConcept (called automatically by 'SCSSub' program)
tmodel :: Fields -> SymbolMap -> TheoryModel -> Contents
tmodel fs m t = Defnt TM (foldr (mkTMField t m) [] fs)
  (S $ t ^. id) --FIXME: Generate reference names here

-- | Create a data definition using a list of fields, SymbolMap, and a 
-- QDefinition (called automatically by 'SCSSub' program)
ddefn :: Fields -> SymbolMap -> QDefinition -> Contents
ddefn fs m d = Defnt DD (foldr (mkQField d m) [] fs) (S "DD:" +:+ S (d ^. id))
--FIXME: Generate the reference names here

gdefn :: Fields -> SymbolMap -> GenDefn -> Contents
gdefn fs m g = Defnt General (foldr (mkGDField g m) [] fs)
  (S $ g ^. id) --FIXME: Generate reference names here

-- | Synonym for easy reading. Model rows are just 'String',['Contents'] pairs
type ModRow = [(String,[Contents])]

-- | Create the fields for a model from a relation concept (used by tmodel)
mkTMField :: TheoryModel -> SymbolMap -> Field -> ModRow -> ModRow
mkTMField t _ l@Label fs  = (show l, (Paragraph $ at_start t):[]) : fs
mkTMField t _ l@DefiningEquation fs = 
  (show l, (map EqnBlock (map tConToExpr (t ^. invariants)))) : fs
mkTMField t m l@(Description v u intro) fs = (show l, Paragraph intro : 
  foldr (\x -> buildTMDescription v u x m) [] (map tConToExpr (t ^. invariants))) : fs
mkTMField _ _ label _ = error $ "Label " ++ show label ++ " not supported " ++
  "for theory models"

tConToExpr :: Constraint -> Expr
tConToExpr (Phys x) = x 0 --FIXME: HACK
tConToExpr (Sfwr x) = x 0 --FIXME: HACK

-- TODO: buildDescription gets list of constraints to expr and ignores 't'.

-- | Create the fields for a definition from a QDefinition (used by ddefn)
mkQField :: QDefinition -> SymbolMap -> Field -> ModRow -> ModRow
mkQField d _ l@Label fs = (show l, (Paragraph $ at_start d):[]) : fs
mkQField d _ l@Symbol fs = (show l, (Paragraph $ (P $ symbol d)):[]) : fs
mkQField d _ l@Units fs = (show l, (Paragraph $ (unit'2Contents d)):[]) : fs
mkQField d _ l@DefiningEquation fs = (show l, (EqnBlock $ equat d):[]) : fs
mkQField d m l@(Description v u intro) fs = 
  (show l, Paragraph intro : buildDDescription v u d m) : fs

-- | Create the description field (if necessary) using the given verbosity and
-- including or ignoring units for a model / general definition
buildTMDescription :: Verbosity -> InclUnits -> Expr -> SymbolMap -> [Contents] -> 
  [Contents]
buildTMDescription Succinct _ _ _ _ = []
buildTMDescription Verbose u e m cs = 
  Enumeration (Definitions (descPairs u (vars e m))) : cs

-- | Create the description field (if necessary) using the given verbosity and
-- including or ignoring units for a data definition
buildDDescription :: Verbosity -> InclUnits -> QDefinition -> SymbolMap -> 
  [Contents]
buildDDescription Succinct u d _ = [Enumeration (Definitions $ (firstPair u d):[])]
buildDDescription Verbose u d m = [Enumeration (Definitions 
  (firstPair u d : descPairs u (vars (equat d) m)))]

mkGDField :: GenDefn -> SymbolMap -> Field -> ModRow -> ModRow
mkGDField g _ l@Label fs = (show l, (Paragraph $ at_start g):[]) : fs
mkGDField g _ l@Units fs = 
  let u = gdUnit g in
    case u of Nothing   -> fs
              Just udef -> (show l, (Paragraph $ Sy (udef ^. usymb)):[]) : fs
mkGDField g _ l@DefiningEquation fs = (show l, (EqnBlock (g ^. relat)):[]) : fs
mkGDField g m l@(Description v u intro) fs = (show l, Paragraph intro : 
  (buildGDDescription v u (g ^. relat) m)) : fs
mkGDField _ _ l _ = error $ "Label " ++ show l ++ " not supported for gen defs"

buildGDDescription :: Verbosity -> InclUnits -> Expr -> SymbolMap -> [Contents]
buildGDDescription Succinct _ _ _ = []
buildGDDescription Verbose u e m  = 
  Enumeration (Definitions (descPairs u (vars e m))) : []

-- | Used for definitions. The first pair is the symbol of the quantity we are
-- defining.
firstPair :: InclUnits -> QDefinition -> ListPair
firstPair (IgnoreUnits) d  = (P (symbol d), Flat (phrase d))
firstPair (IncludeUnits) d = (P (symbol d), Flat (phrase d +:+ sParen (unit'2Contents d)))

-- | Create the descriptions for each symbol in the relation/equation
descPairs :: InclUnits -> [VarChunk] -> [ListPair]
descPairs IgnoreUnits = map (\x -> (P (symbol x), Flat $ phrase x))
descPairs IncludeUnits = 
  map (\x -> ((P (symbol x)), Flat $ phrase x +:+ sParen (unit'2Contents x)))
  -- FIXME: Need a Units map for looking up units from variables

instance Show Field where
  show Label = "Label"
  show Symbol = "Symbol"
  show Units = "Units"
  show DefiningEquation = "Equation"
  show (Description _ _ _) = "Description"
  -- show Sources = "Sources"
  -- show RefBy = "RefBy"