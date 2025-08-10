{-# Language GADTs #-}
-- | Defines helper functions for creating subsections within the Solution Characteristics Specification.
-- Namely, for theory models, general definitions, data definitions, and instance models.
module Drasil.DocumentLanguage.Definitions (
  -- * Types
  Field(..), Fields, InclUnits(..), Verbosity(..),
  -- * Constructors
  ddefn, derivation, gdefn,
  instanceModel, tmodel,
  -- * Helpers
  helperRefs, helpToRefField) where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Control.Lens ((^.))

import Language.Drasil
import Database.Drasil

import Drasil.System
import Drasil.GetChunks (vars)

import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, Theory(..),
  TheoryModel, HasInputs(inputs), HasOutput(output, out_constraints), qdFromDD)

import Drasil.DocumentLanguage.Units (toSentenceUnitless)

-- | Synonym for a list of 'Field's.
type Fields = [Field]

-- | Fields that should be displayed in definitions.
data Field = Label
           | Symbol
           | Units
           | DefiningEquation
           | Description Verbosity InclUnits
           | Input
           | Output
           | InConstraints
           | OutConstraints
           | Notes
           | Source --  I think using attribute makes most sense, as sources can and
              -- will be modified across applications; the underlying knowledge won't.
           | RefBy --TODO: Fill in the field.

-- | Refers to the verbosity of statements.
data Verbosity = Verbose  -- ^ Full Descriptions.
               | Succinct -- ^ Simple Description (do not redefine other symbols).

-- | Determines whether to include or ignore units.
data InclUnits = IncludeUnits -- ^ In description field (for other symbols).
               | IgnoreUnits

-- | Create a theoretical model using a list of fields to be displayed, a database of symbols,
-- and a 'RelationConcept' (called automatically by 'SCSSub' program).
tmodel :: Fields -> System -> TheoryModel -> LabelledContent
tmodel fs m t = mkRawLC (Defini Theory (foldr (mkTMField t m) [] fs)) (ref t)

-- | Create a data definition using a list of fields, a database of symbols, and a
-- 'QDefinition' (called automatically by 'SCSSub' program).
ddefn :: Fields -> System -> DataDefinition -> LabelledContent
ddefn fs m d = mkRawLC (Defini Data (foldr (mkDDField d m) [] fs)) (ref d)

-- | Create a general definition using a list of fields, database of symbols,
-- and a 'GenDefn' (general definition) chunk (called automatically by 'SCSSub'
-- program).
gdefn :: Fields -> System -> GenDefn -> LabelledContent
gdefn fs m g = mkRawLC (Defini General (foldr (mkGDField g m) [] fs)) (ref g)

-- | Create an instance model using a list of fields, database of symbols,
-- and an 'InstanceModel' chunk (called automatically by 'SCSSub' program).
instanceModel :: Fields -> System -> InstanceModel -> LabelledContent
instanceModel fs m i = mkRawLC (Defini Instance (foldr (mkIMField i m) [] fs)) (ref i)

-- | Create a derivation from a chunk's attributes. This follows the TM, DD, GD,
-- or IM definition automatically (called automatically by 'SCSSub' program).
derivation :: (MayHaveDerivation c, HasShortName c, Referable c) => c -> Maybe Contents
derivation c = fmap
  (\(Derivation h d) -> LlC $ llcc (ref c) $ DerivBlock h $ map makeDerivCons d) $
  c ^. derivations

-- | Helper function for creating the layout objects
-- (paragraphs and equation blocks) for a derivation.
makeDerivCons :: Sentence -> RawContent
makeDerivCons (E e) = EqnBlock e
makeDerivCons s     = Paragraph s

-- | Synonym for easy reading. Model rows are just 'String',['Contents'] pairs.
type ModRow = [(String, [Contents])]

-- | Similar to 'maybe' but for lists.
nonEmpty :: b -> ([a] -> b) -> [a] -> b
nonEmpty def _ [] = def
nonEmpty _   f xs = f xs

tmDispExprs :: TheoryModel -> [ModelExpr]
tmDispExprs t = map express (t ^. defined_quant) ++ t ^. invariants

-- | Create the fields for a model from a relation concept (used by 'tmodel').
mkTMField :: TheoryModel -> System -> Field -> ModRow -> ModRow
mkTMField t _ l@Label fs  = (show l, [mkParagraph $ atStart t]) : fs
mkTMField t _ l@DefiningEquation fs = (show l, map unlbldExpr $ tmDispExprs t) : fs
mkTMField t m l@(Description v u) fs = (show l,
  foldr ((\x -> buildDescription v u x m) . express) [] $ tmDispExprs t) : fs
mkTMField t m l@RefBy fs = (show l, [mkParagraph $ helperRefs t m]) : fs --FIXME: fill this in
mkTMField t _ l@Source fs = (show l, helperSources $ t ^. getDecRefs) : fs
mkTMField t _ l@Notes fs =
  nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (t ^. getNotes)
mkTMField _ _ l _ = error $ "Label " ++ show l ++ " not supported " ++
  "for theory models"

-- | Helper function to make a list of 'Sentence's from the current system information and something that has a 'UID'.
helperRefs :: HasUID t => t -> System -> Sentence
helperRefs t s = foldlList Comma List $ map (`helpToRefField` (s ^. systemdb)) $ nub $
  refbyLookup (t ^. uid) (refbyTable $ s ^. systemdb)

-- | Creates a reference as a 'Sentence' by finding if the 'UID' is in one of
-- the possible data sets contained in the 'System' database.
helpToRefField :: UID -> ChunkDB -> Sentence
helpToRefField trg db
  | (Just c) <- (find trg db :: Maybe DataDefinition)  = refS c
  | (Just c) <- (find trg db :: Maybe InstanceModel)   = refS c
  | (Just c) <- (find trg db :: Maybe GenDefn)         = refS c
  | (Just c) <- (find trg db :: Maybe TheoryModel)     = refS c
  | (Just _) <- (find trg db :: Maybe ConceptInstance) = EmptyS
  | otherwise = error $ show trg ++ "Caught."

-- | Helper that makes a list of 'Reference's into a 'Sentence'. Then wraps into 'Contents'.
helperSources :: [DecRef] -> [Contents]
helperSources [] = [mkParagraph $ S "--"]
helperSources rs  = [mkParagraph $ foldlList Comma List $ map (\r -> Ref (r ^. uid) EmptyS $ refInfo r) rs]

-- | Creates the fields for a definition from a 'QDefinition' (used by 'ddefn').
mkDDField :: DataDefinition -> System -> Field -> ModRow -> ModRow
mkDDField d _ l@Label fs = (show l, [mkParagraph $ atStart d]) : fs
mkDDField d _ l@Symbol fs = (show l, [mkParagraph . P $ eqSymb $ d ^. defLhs]) : fs
mkDDField d _ l@Units fs = (show l, [mkParagraph $ toSentenceUnitless $ d ^. defLhs]) : fs
mkDDField d _ l@DefiningEquation fs = (show l, [unlbldExpr $ express d]) : fs
mkDDField d m l@(Description v u) fs = (show l, buildDDescription' v u d m) : fs
mkDDField t m l@RefBy fs = (show l, [mkParagraph $ helperRefs t m]) : fs --FIXME: fill this in
mkDDField d _ l@Source fs = (show l, helperSources $ d ^. getDecRefs) : fs
mkDDField d _ l@Notes fs = nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (d ^. getNotes)
mkDDField _ _ l _ = error $ "Label " ++ show l ++ " not supported " ++
  "for data definitions"

-- | Creates the description field for 'Contents' (if necessary) using the given verbosity and
-- including or ignoring units for a model/general definition.
buildDescription :: Verbosity -> InclUnits -> ModelExpr -> System -> [Contents] ->
  [Contents]
buildDescription Succinct _ _ _ _ = []
buildDescription Verbose u e m cs = (UlC . ulcc .
  Enumeration . Definitions . descPairs u $ vars e $ _systemdb m) : cs

-- | Similar to 'buildDescription' except it takes a 'DataDefinition' that is included as the 'firstPair'' in ['Contents'] (independent of verbosity).
-- The 'Verbose' case also includes more details about the 'DataDefinition' expressions.
buildDDescription' :: Verbosity -> InclUnits -> DataDefinition -> System ->
  [Contents]
buildDDescription' Succinct u d _ = [UlC . ulcc . Enumeration $ Definitions [firstPair' u d]]
buildDDescription' Verbose  u d m = [UlC . ulcc . Enumeration $ Definitions $
  firstPair' u d : descPairs u (flip vars (_systemdb m) $
  either (express . (^. defnExpr)) (^. defnExpr) (qdFromDD d))]

-- | Create the fields for a general definition from a 'GenDefn' chunk.
mkGDField :: GenDefn -> System -> Field -> ModRow -> ModRow
mkGDField g _ l@Label fs = (show l, [mkParagraph $ atStart g]) : fs
mkGDField g _ l@Units fs =
  maybe fs (\udef -> (show l, [mkParagraph . Sy $ usymb udef]) : fs) (getUnit g)
mkGDField g _ l@DefiningEquation fs = (show l, [unlbldExpr $ express g]) : fs
mkGDField g m l@(Description v u) fs = (show l,
  buildDescription v u (express g) m []) : fs
mkGDField g m l@RefBy fs = (show l, [mkParagraph $ helperRefs g m]) : fs --FIXME: fill this in
mkGDField g _ l@Source fs = (show l, helperSources $ g ^. getDecRefs) : fs
mkGDField g _ l@Notes fs = nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (g ^. getNotes)
mkGDField _ _ l _ = error $ "Label " ++ show l ++ " not supported for gen defs"

-- | Create the fields for an instance model from an 'InstanceModel' chunk.
mkIMField :: InstanceModel -> System -> Field -> ModRow -> ModRow
mkIMField i _ l@Label fs  = (show l, [mkParagraph $ atStart i]) : fs
mkIMField i _ l@DefiningEquation fs = (show l, [unlbldExpr $ express i]) : fs
mkIMField i m l@(Description v u) fs = (show l,
  foldr (\x -> buildDescription v u x m) [] [express i]) : fs
mkIMField i m l@RefBy fs = (show l, [mkParagraph $ helperRefs i m]) : fs --FIXME: fill this in
mkIMField i _ l@Source fs = (show l, helperSources $ i ^. getDecRefs) : fs
mkIMField i _ l@Output fs = (show l, [mkParagraph x]) : fs
  where x = P . eqSymb $ i ^. output
mkIMField i _ l@Input fs =
  case map fst (i ^. inputs) of
    [] -> (show l, [mkParagraph EmptyS]) : fs -- FIXME? Should an empty input list be allowed?
    (_:_) -> (show l, [mkParagraph $ foldl1 sC xs]) : fs
  where xs = map (P . eqSymb . fst) $ i ^. inputs
mkIMField i _ l@InConstraints fs  =
  let ll = mapMaybe (\(x,y) -> y >>= (\z -> Just (x, z))) (i ^. inputs) in
  (show l, foldr ((:) . UlC . ulcc . EqnBlock . express . uncurry realInterval) [] ll) : fs
mkIMField i _ l@OutConstraints fs =
  (show l, foldr ((:) . UlC . ulcc . EqnBlock . express . realInterval (i ^. output)) []
    (i ^. out_constraints)) : fs
mkIMField i _ l@Notes fs =
  nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (i ^. getNotes)
mkIMField _ _ l _ = error $ "Label " ++ show l ++ " not supported " ++
  "for instance models"

-- | Used for making definitions. The first pair is the symbol of the quantity we are
-- defining.
firstPair' :: InclUnits -> DataDefinition -> ListTuple
firstPair' IgnoreUnits d  = (P $ eqSymb $ d ^. defLhs, Flat $ phrase d, Nothing)
firstPair' IncludeUnits d =
  (P $ eqSymb $ d ^. defLhs, Flat $ phrase (d ^. defLhs) +:+ sParen (toSentenceUnitless $ d ^. defLhs), Nothing)

-- | Creates the descriptions for each symbol in the relation/equation.
descPairs :: (Quantity q, MayHaveUnit q) => InclUnits -> [q] -> [ListTuple]
descPairs IgnoreUnits = map (\x -> (P $ eqSymb x, Flat $ phrase x, Nothing))
descPairs IncludeUnits =
  map (\x -> (P $ eqSymb x, Flat $ phrase x +:+ sParen (toSentenceUnitless x), Nothing))
  -- FIXME: Need a Units map for looking up units from variables

-- | Defines 'Field's as 'String's.
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
  show Notes             = "Notes"
