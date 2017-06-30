{-# Language GADTs #-}

module Drasil.DocumentLanguage.Definitions 
  ( Fields
  , Field(..)
  , Verbosity(..)
  , tmodel
  , InclUnits(..)
  )where

import Language.Drasil
import Control.Lens ((^.))

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
tmodel :: Fields -> SymbolMap -> RelationConcept -> Contents
tmodel fs m t = TMod (foldr (mkRelField t m) [] fs) 
  (S "T:" +:+ EmptyS) t --FIXME: Generate reference names here

-- | Synonym for easy reading. Model rows are just 'String',['Contents'] pairs
type ModRow = [(String,[Contents])]

-- | Create the fields for a model from a relation concept (used by tmodel)
mkRelField :: RelationConcept -> SymbolMap -> Field -> ModRow -> ModRow
mkRelField t _ l@Label fs  = (show l, (Paragraph $ at_start t):[]) : fs
mkRelField _ _ Symbol _ = error $ 
  "Cannot assign symbol to a model - See Drasil.DocumentLanguage.Definitions"
mkRelField _ _ Units _  = error $ 
  "Models cannot have units - See Drasil.DocumentLanguage.Definitions"
--(show l, (Paragraph $ unit'2Contents t):[]) : fs
mkRelField t _ l@DefiningEquation fs = (show l, (EqnBlock $ relat t):[]) : fs
mkRelField t m l@(Description v u intro) fs =
  (show l, Paragraph intro : buildDescription v u t m) : fs

-- | Create the description field (if necessary) using the given verbosity and
-- including or ignoring units
buildDescription :: Verbosity -> InclUnits -> RelationConcept -> SymbolMap -> 
  [Contents]
buildDescription Succinct _ _ _ = []
buildDescription Verbose u t m = [Enumeration (Definitions (descPairs u (vars (relat t) m)))]

-- | Used for definitions. The first pair is the symbol of the quantity we are
-- defining.
firstPair :: InclUnits -> QDefinition -> ListPair
firstPair (IgnoreUnits) t  = (P (t ^. symbol), Flat (phrase t))
firstPair (IncludeUnits) t = (P (t ^. symbol), Flat (phrase t +:+ sParen (unit'2Contents t)))

-- | Create the descriptions for each symbol in the relation/equation
descPairs :: InclUnits -> [VarChunk] -> [ListPair]
descPairs IgnoreUnits = map (\x -> (P (x ^. symbol), Flat $ phrase x))
descPairs IncludeUnits = 
  map (\x -> ((P (x ^. symbol)), Flat $ phrase x +:+ sParen (unit'2Contents x)))
  -- FIXME: Need a Units map for looking up units from variables

instance Show Field where
  show Label = "Label"
  show Symbol = "Symbol"
  show Units = "Units"
  show DefiningEquation = "Equation"
  show (Description _ _ _) = "Description"
  -- show Sources = "Sources"
  -- show RefBy = "RefBy"