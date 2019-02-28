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
  , helperRefs
  , helpToRefField
  )where
import Data.Map (keys)
import Data.List (elem)
import Language.Drasil
import Language.Drasil.Development (MayHaveUnit(getUnit))
import Data.Drasil.Utils (eqUnR')
import Data.Drasil.SentenceStructures (getSource', foldlSent)

import Drasil.DocumentLanguage.Units (toSentenceUnitless)

import Control.Lens ((^.))

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
           | Notes
           | Source --  I think using attribute makes most sense, as sources can and
              -- will be modified across applications; the underlying knowledge won't.
           | RefBy --TODO: Fill in the field.

data Verbosity = Verbose  -- Full Descriptions
               | Succinct -- Simple Description (do not redefine other symbols)

data InclUnits = IncludeUnits -- In description field (for other symbols)
               | IgnoreUnits

-- | Create a theoretical model using a list of fields to be displayed, a database of symbols,
-- and a RelationConcept (called automatically by 'SCSSub' program)
tmodel :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => Fields -> ctx  -> TheoryModel -> LabelledContent
tmodel fs m t = mkRawLC (Defini TM (foldr (mkTMField t m) [] fs)) (makeRef2 t)

-- | Create a data definition using a list of fields, a database of symbols, and a
-- QDefinition (called automatically by 'SCSSub' program)
ddefn :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => Fields -> ctx -> DataDefinition -> LabelledContent
ddefn fs m d = mkRawLC (Defini DD (foldr (mkDDField d m) [] fs)) (makeRef2 d)

-- | Create a general definition using a list of fields, database of symbols,
-- and a 'GenDefn' (general definition) chunk (called automatically by 'SCSSub'
-- program)
gdefn :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => Fields -> ctx -> GenDefn -> LabelledContent
gdefn fs m g = mkRawLC (Defini General (foldr (mkGDField g m) [] fs)) (makeRef2 g)

-- | Create an instance model using a list of fields, database of symbols,
-- and an 'InstanceModel' chunk (called automatically by 'SCSSub' program)
instanceModel :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => Fields -> ctx -> InstanceModel -> LabelledContent
instanceModel fs m i = mkRawLC (Defini Instance (foldr (mkIMField i m) [] fs)) (makeRef2 i)

-- | Create a derivation from a chunk's attributes. This follows the TM, DD, GD,
-- or IM definition automatically (called automatically by 'SCSSub' program)
derivation :: HasDerivation c => c -> [Contents]
derivation g = map makeDerivationContents (g ^. derivations)

-- | Helper function for creating the layout objects
-- (paragraphs and equation blocks) for a derivation.
makeDerivationContents :: Sentence -> Contents
makeDerivationContents (E e) = UlC $ ulcc $ EqnBlock e
makeDerivationContents s     = UlC $ ulcc $ Paragraph s

-- | Synonym for easy reading. Model rows are just 'String',['Contents'] pairs
type ModRow = [(String, [Contents])]

-- | nonEmpty is like |maybe| but for lists
nonEmpty :: b -> ([a] -> b) -> [a] -> b
nonEmpty def _ [] = def
nonEmpty _   f xs = f xs

-- | Create the fields for a model from a relation concept (used by tmodel)
mkTMField :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => TheoryModel -> ctx  -> Field -> ModRow -> ModRow
mkTMField t _ l@Label fs  = (show l, [mkParagraph $ at_start t]) : fs
mkTMField t _ l@DefiningEquation fs =
  (show l, map eqUnR' (t ^. invariants)) : fs 
mkTMField t m l@(Description v u) fs = (show l,
  foldr (\x -> buildDescription v u x m) [] (t ^. invariants)) : fs
mkTMField t m l@(RefBy) fs = (show l, [mkParagraph $ helperRefs t m]) : fs --FIXME: fill this in
mkTMField t _ l@(Source) fs = (show l, map (mkParagraph . Ref) $ t ^. getReferences) : fs
mkTMField t _ l@(Notes) fs = 
  nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (t ^. getNotes)
mkTMField _ _ label _ = error $ "Label " ++ show label ++ " not supported " ++
  "for theory models"

helperRefs :: (HasUID t, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => t -> ctx -> Sentence
helperRefs t s = foldlSent $ map (\x -> helpToRefField x s) $ refbyLookup (t ^. uid) (s ^. refbyTable)

helpToRefField :: (HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => UID -> ctx -> Sentence
helpToRefField t s = if t `elem` (keys $ s ^. dataDefnTable) 
  then makeRef2S $ datadefnLookup t (s ^. dataDefnTable)
  else if  t `elem` (keys $ s ^. insmodelTable)
    then makeRef2S $ insmodelLookup t (s ^. insmodelTable)
    else if t `elem` (keys $ s ^. gendefTable)
      then makeRef2S $ gendefLookup t (s ^. gendefTable)
      else if t `elem` (keys $ s ^. theoryModelTable)
        then makeRef2S $ theoryModelLookup t (s ^. theoryModelTable)
        else if t `elem` (keys $ s ^. assumpTable)
          then makeRef2S $ assumptionLookup t (s ^. assumpTable)
          else if t `elem` (keys $ s ^. conceptinsTable)
            then makeRef2S $ conceptinsLookup t (s ^. conceptinsTable)
            else if t `elem` (keys $ s ^. sectionTable)
              then makeRef2S $ sectionLookup t (s ^. sectionTable)
              else if t `elem` (keys $ s ^. labelledcontent)
                then makeRef2S $ labelledconLookup t (s ^. labelledcontent) 
                else error $ t ++ "Caught."

-- | Create the fields for a definition from a QDefinition (used by ddefn)
mkDDField :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => DataDefinition -> ctx -> Field -> ModRow -> ModRow
mkDDField d _ l@Label fs = (show l, [mkParagraph $ at_start d]) : fs
mkDDField d _ l@Symbol fs = (show l, [mkParagraph . P $ eqSymb d]) : fs
mkDDField d _ l@Units fs = (show l, [mkParagraph $ toSentenceUnitless d]) : fs
mkDDField d _ l@DefiningEquation fs = (show l, [eqUnR' $ sy d $= d ^. defnExpr]) : fs 
mkDDField d m l@(Description v u) fs =
  (show l, buildDDescription' v u d m) : fs
mkDDField t m l@(RefBy) fs = (show l, [mkParagraph $ helperRefs t m]) : fs --FIXME: fill this in
mkDDField d _ l@(Source) fs = (show l, [mkParagraph $ getSource' d]) : fs
mkDDField d _ l@(Notes) fs = nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (d ^. getNotes)
mkDDField _ _ label _ = error $ "Label " ++ show label ++ " not supported " ++
  "for data definitions"

-- | Create the description field (if necessary) using the given verbosity and
-- including or ignoring units for a model / general definition
buildDescription :: HasSymbolTable ctx => Verbosity -> InclUnits -> Expr -> ctx -> [Contents] ->
  [Contents]
buildDescription Succinct _ _ _ _ = []
buildDescription Verbose u e m cs = (UlC . ulcc .
  Enumeration . Definitions . descPairs u $ vars e m) : cs

-- | Create the description field (if necessary) using the given verbosity and
-- including or ignoring units for a data definition
buildDDescription' :: HasSymbolTable ctx => Verbosity -> InclUnits -> DataDefinition -> ctx ->
  [Contents]
buildDDescription' Succinct u d _ = [UlC . ulcc . Enumeration $ Definitions [firstPair' u d]]
buildDDescription' Verbose u d m = [UlC . ulcc . Enumeration $ Definitions
  (firstPair' u d : descPairs u (flip vars m $ d ^. defnExpr))]

-- | Create the fields for a general definition from a 'GenDefn' chunk.
mkGDField :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => GenDefn -> ctx -> Field -> ModRow -> ModRow
mkGDField g _ l@Label fs = (show l, [mkParagraph $ at_start g]) : fs
mkGDField g _ l@Units fs = 
  maybe fs (\udef -> (show l, [mkParagraph . Sy $ usymb udef]) : fs) (getUnit g)
mkGDField g _ l@DefiningEquation fs = (show l, [eqUnR' $ g ^. relat]) : fs
mkGDField g m l@(Description v u) fs = (show l,
  (buildDescription v u (g ^. relat) m) []) : fs
mkGDField g m l@(RefBy) fs = (show l, [mkParagraph $ helperRefs g m]) : fs --FIXME: fill this in
mkGDField g _ l@(Source) fs = (show l, [mkParagraph $ getSource' g]) : fs
mkGDField g _ l@(Notes) fs = nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (g ^. getNotes)
mkGDField _ _ l _ = error $ "Label " ++ show l ++ " not supported for gen defs"

-- | Create the fields for an instance model from an 'InstanceModel' chunk
mkIMField :: (HasSymbolTable ctx, HasDataDefnTable ctx, HasInsModelTable ctx, HasGendefTable ctx, HasTheoryModelTable ctx
  , HasTraceTable ctx, HasRefbyTable ctx, HasAssumpTable ctx, HasConceptInstance ctx,
  HasSectionTable ctx, HasLabelledContent ctx) => InstanceModel -> ctx -> Field -> ModRow -> ModRow
mkIMField i _ l@Label fs  = (show l, [mkParagraph $ at_start i]) : fs
mkIMField i _ l@DefiningEquation fs = (show l, [eqUnR' $ i ^. relat]) : fs
mkIMField i m l@(Description v u) fs = (show l,
  foldr (\x -> buildDescription v u x m) [] [i ^. relat]) : fs
mkIMField i m l@(RefBy) fs = (show l, [mkParagraph $ helperRefs i m]) : fs --FIXME: fill this in
mkIMField i _ l@(Source) fs = (show l, [mkParagraph $ getSource' i]) : fs
mkIMField i _ l@(Output) fs = (show l, [mkParagraph x]) : fs
  where x = P . eqSymb $ i ^. imOutput
mkIMField i _ l@(Input) fs = 
  case (i ^. imInputs) of
  [] -> (show l, [mkParagraph EmptyS]) : fs -- FIXME? Should an empty input list be allowed?
  (_:_) -> (show l, [mkParagraph $ foldl (sC) x xs]) : fs
  where (x:xs) = map (P . eqSymb) $ i ^. imInputs
mkIMField i _ l@(InConstraints) fs  = 
  (show l, foldr ((:) . UlC . ulcc . EqnBlock) [] (i ^. inCons)) : fs
mkIMField i _ l@(OutConstraints) fs = 
  (show l, foldr ((:) . UlC . ulcc . EqnBlock) [] (i ^. outCons)) : fs
mkIMField i _ l@(Notes) fs = 
  nonEmpty fs (\ss -> (show l, map mkParagraph ss) : fs) (i ^. getNotes)
mkIMField _ _ label _ = error $ "Label " ++ show label ++ " not supported " ++
  "for instance models"

-- | Used for definitions. The first pair is the symbol of the quantity we are
-- defining.
firstPair' :: InclUnits -> DataDefinition -> ListTuple
firstPair' (IgnoreUnits) d  = (P $ eqSymb d, Flat $ phrase d, Nothing)
firstPair' (IncludeUnits) d = (P $ eqSymb d, Flat $ phrase d +:+ (sParen $
  toSentenceUnitless d), Nothing)

-- | Create the descriptions for each symbol in the relation/equation
descPairs :: (Quantity q, MayHaveUnit q) => InclUnits -> [q] -> [ListTuple]
descPairs IgnoreUnits = map (\x -> (P $ eqSymb x, Flat $ phrase x, Nothing))
descPairs IncludeUnits =
  map (\x -> (P $ eqSymb x, Flat $ phrase x +:+ (sParen $ toSentenceUnitless x), Nothing))
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
  show Notes             = "Notes"
