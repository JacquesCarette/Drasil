{-# Language GADTs #-}

module Drasil.DocumentLanguage.Definitions 
  ( Fields
  , Field(..)
  , Verbosity(..)
  , tmodel
  , ddefn
  , gdefn, derivation
  , instanceModel
  , InclUnits(..)
  )where

import Language.Drasil
import Data.Drasil.Utils (foldle)

import Control.Lens ((^.))

import Prelude hiding (id)

-- | Synonym for a list of 'Field'
type Fields = [Field]

-- | Fields that should be displayed in definitions
data Field = Label 
           | Symbol
           | Units
           | DefiningEquation
           | Description Verbosity InclUnits
           | Input
           | Output
           | InConstraints
           | OutConstraints
           | Source --  I think using attribute makes most sense, as sources can and
              -- will be modified across applications; the underlying knowledge won't.
           | RefBy --TODO: Fill in the field.
           
data Verbosity = Verbose  -- Full Descriptions
               | Succinct -- Simple Description (do not redefine other symbols)

data InclUnits = IncludeUnits -- In description field (for other symbols)
               | IgnoreUnits

-- FIXME: Will need to couple specialized intros with the chunks they belong to.
-- Sounds like an attribute for Definitions/Models.
type VerbatimIntro = Sentence

-- | Create a theoretical model using a list of fields to be displayed, a database of symbols,
-- and a RelationConcept (called automatically by 'SCSSub' program)
tmodel :: HasSymbolTable ctx => Fields -> ctx -> TheoryModel -> Contents
tmodel fs m t = Defnt TM (foldr (mkTMField t m) [] fs)
  (S "T:" :+: S (t ^. id)) --FIXME: Generate reference names here

-- | Create a data definition using a list of fields, a database of symbols, and a 
-- QDefinition (called automatically by 'SCSSub' program)
ddefn :: HasSymbolTable ctx => Fields -> ctx -> QDefinition -> Contents
ddefn fs m d = Defnt DD (foldr (mkQField d m) [] fs) (S "DD:" :+: S (d ^. id))
--FIXME: Generate the reference names here

-- | Create a general definition using a list of fields, database of symbols,
-- and a 'GenDefn' (general definition) chunk (called automatically by 'SCSSub'
-- program)
gdefn :: HasSymbolTable ctx => Fields -> ctx -> GenDefn -> Contents
gdefn fs m g = Defnt General (foldr (mkGDField g m) [] fs)
  (S "GD:" :+: S (g ^. id)) --FIXME: Generate reference names here

-- | Create an instance model using a list of fields, database of symbols,
-- and an 'InstanceModel' chunk (called automatically by 'SCSSub' program)
instanceModel :: HasSymbolTable ctx => Fields -> ctx -> InstanceModel -> Contents
instanceModel fs m i = Defnt Instance (foldr (mkIMField i m) [] fs) (S "IM:" :+: S (i ^. id))

-- | Create a derivation from a chunk's attributes. This follows the TM, DD, GD,
-- or IM definition automatically (called automatically by 'SCSSub' program)
derivation :: HasAttributes c => c -> [Contents]
derivation g = map makeDerivationContents (getDerivation g)

-- | Helper function for creating the layout objects 
-- (paragraphs and equation blocks) for a derivation.
makeDerivationContents :: Sentence -> Contents
makeDerivationContents (E e) = EqnBlock e
makeDerivationContents s     = Paragraph s

-- | Synonym for easy reading. Model rows are just 'String',['Contents'] pairs
type ModRow = [(String,[Contents])]

-- | Create the fields for a model from a relation concept (used by tmodel)
mkTMField :: HasSymbolTable ctx => TheoryModel -> ctx -> Field -> ModRow -> ModRow
mkTMField t _ l@Label fs  = (show l, (Paragraph $ at_start t):[]) : fs
mkTMField t _ l@DefiningEquation fs = 
  (show l, (map EqnBlock (map tConToExpr (t ^. invariants)))) : fs
mkTMField t m l@(Description v u) fs = (show l,  
  foldr (\x -> buildDescription v u x m) [] (map tConToExpr (t ^. invariants))) : fs
mkTMField _ _ l@(RefBy) fs = (show l, fixme) : fs --FIXME: fill this in
mkTMField _ _ l@(Source) fs = (show l, fixme) : fs --FIXME: fill this in
mkTMField _ _ label _ = error $ "Label " ++ show label ++ " not supported " ++
  "for theory models"

tConToExpr :: TheoryConstraint -> Expr
tConToExpr (TCon Invariant x) = x
tConToExpr (TCon AssumedCon x) = x

-- TODO: buildDescription gets list of constraints to expr and ignores 't'.

-- | Create the fields for a definition from a QDefinition (used by ddefn)
mkQField :: HasSymbolTable ctx => QDefinition -> ctx -> Field -> ModRow -> ModRow
mkQField d _ l@Label fs = (show l, (Paragraph $ at_start d):[]) : fs
mkQField d _ l@Symbol fs = (show l, (Paragraph $ (P $ eqSymb d)):[]) : fs
mkQField d _ l@Units fs = (show l, (Paragraph $ (unit'2Contents d)):[]) : fs
mkQField d _ l@DefiningEquation fs = (show l, (EqnBlock $ d^.equat):[]) : fs
mkQField d m l@(Description v u) fs = 
  (show l, buildDDescription v u d m) : fs
mkQField _ _ l@(RefBy) fs = (show l, fixme) : fs --FIXME: fill this in
mkQField d _ l@(Source) fs = (show l, [Paragraph $ getSource d]) : fs 
mkQField _ _ label _ = error $ "Label " ++ show label ++ " not supported " ++
  "for data definitions"

-- | Create the description field (if necessary) using the given verbosity and
-- including or ignoring units for a model / general definition
buildDescription :: HasSymbolTable ctx => Verbosity -> InclUnits -> Expr -> ctx -> [Contents] -> 
  [Contents]
buildDescription Succinct _ _ _ _ = []
buildDescription Verbose u e m cs = 
  Enumeration (Definitions (descPairs u (vars e m))) : cs

-- | Create the description field (if necessary) using the given verbosity and
-- including or ignoring units for a data definition
buildDDescription :: HasSymbolTable ctx => Verbosity -> InclUnits -> QDefinition -> ctx -> 
  [Contents]
buildDDescription Succinct u d _ = [Enumeration (Definitions $ (firstPair u d):[])]
buildDDescription Verbose u d m = [Enumeration (Definitions 
  (firstPair u d : descPairs u (vars (d^.equat) m)))]

-- | Create the fields for a general definition from a 'GenDefn' chunk.
mkGDField :: HasSymbolTable ctx => GenDefn -> ctx -> Field -> ModRow -> ModRow
mkGDField g _ l@Label fs = (show l, (Paragraph $ at_start g):[]) : fs
mkGDField g _ l@Units fs = 
  let u = gdUnit g in
    case u of Nothing   -> fs
              Just udef -> (show l, (Paragraph $ Sy (udef ^. usymb)):[]) : fs
mkGDField g _ l@DefiningEquation fs = (show l, (EqnBlock (g ^. relat)):[]) : fs
mkGDField g m l@(Description v u) fs = (show l, 
  (buildDescription v u (g ^. relat) m) []) : fs
mkGDField _ _ l@(RefBy) fs = (show l, fixme) : fs --FIXME: fill this in
mkGDField g _ l@(Source) fs = (show l, [Paragraph $ getSource g]) : fs 
mkGDField _ _ l _ = error $ "Label " ++ show l ++ " not supported for gen defs"

-- | Create the fields for an instance model from an 'InstanceModel' chunk
mkIMField :: HasSymbolTable ctx => InstanceModel -> ctx -> Field -> ModRow -> ModRow
mkIMField i _ l@Label fs  = (show l, (Paragraph $ at_start i):[]) : fs
mkIMField i _ l@DefiningEquation fs = 
  (show l, (EqnBlock (i ^. relat)):[]) : fs
mkIMField i m l@(Description v u) fs = (show l,  
  foldr (\x -> buildDescription v u x m) [] [i ^. relat]) : fs
mkIMField _ _ l@(RefBy) fs = (show l, fixme) : fs --FIXME: fill this in
mkIMField i _ l@(Source) fs = (show l, [Paragraph $ getSource i]) : fs --FIXME: fill this in
mkIMField i _ l@(Output) fs = (show l, [Paragraph $ foldle (sC) (+:+.) EmptyS (map (P . symbol Equational) (outputs i))]) : fs
mkIMField i _ l@(Input) fs = (show l, [Paragraph $ foldle (sC) (+:+.) EmptyS (map (P . symbol Equational) (inputs i))]) : fs
mkIMField i _ l@(InConstraints) fs  = (show l,  
  foldr ((:) . EqnBlock) [] (map tConToExpr (inCons i))) : fs
mkIMField i _ l@(OutConstraints) fs = (show l,  
  foldr ((:) . EqnBlock) [] (map tConToExpr (outCons i))) : fs
mkIMField _ _ label _ = error $ "Label " ++ show label ++ " not supported " ++
  "for instance models"

  
-- | Used for definitions. The first pair is the symbol of the quantity we are
-- defining.
firstPair :: InclUnits -> QDefinition -> ListPair
firstPair (IgnoreUnits) d  = (P (eqSymb d), Flat (phrase d))
firstPair (IncludeUnits) d = (P (eqSymb d), Flat (phrase d +:+ sParen (unit'2Contents d)))

-- | Create the descriptions for each symbol in the relation/equation
descPairs :: (Quantity q) => InclUnits -> [q] -> [ListPair]
descPairs IgnoreUnits = map (\x -> (P (eqSymb x), Flat $ phrase x))
descPairs IncludeUnits = 
  map (\x -> ((P (eqSymb x)), Flat $ phrase x +:+ sParen (unit'2Contents x)))
  -- FIXME: Need a Units map for looking up units from variables

instance Show Field where
  show Label             = "Label"
  show Symbol            = "Symbol"
  show Units             = "Units"
  show RefBy             = "RefBy"
  show Source            = "Source"
  show Input             = "Input"
  show Output            = "Output"
  show InConstraints     = "Input Constraints"
  show OutConstraints    = "Output Constraints"
  show DefiningEquation  = "Equation"
  show (Description _ _) = "Description"

fixme :: [Contents]
fixme = [Paragraph $ S "FIXME: This needs to be filled in"]
  
