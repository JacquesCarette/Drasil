{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.UnitDefn (
    UnitDefn(..)
  , fromUDefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), newUnit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', compUnitDefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit)
  , IsUnit(getUnits)
  ) where

import Control.Lens ((^.), makeLenses, view)
import Control.Arrow (second)

import Language.Drasil.Chunk.Concept (ConceptChunk, dcc, cc')
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), HasUnitSymbol(usymb), IsUnit(udefn, getUnits))
import Language.Drasil.NounPhrase (cn,cn',NP)
import Language.Drasil.Symbol (Symbol(Label))
import Language.Drasil.UnitLang (USymb(US), UDefn(UScale, USynonym, UShift), 
  compUSymb, fromUDefn, getUSymb, getDefn, UnitSymbol(BaseSI, DerivedSI, Defined))
import Language.Drasil.UID

-- | for defining units
-- It is a concept chunk (defined what kind of unit it is),
-- has a unit symbol, maybe another (when it is a synonym),
-- perhaps a definition, and the list of UID of the units that make up
-- the definition.
data UnitDefn = UD { _vc :: ConceptChunk 
                   , _cas :: UnitSymbol
                   , _cu :: [UID] }
makeLenses ''UnitDefn

instance HasUID        UnitDefn where uid = vc . uid
instance NamedIdea     UnitDefn where term   = vc . term
instance Idea          UnitDefn where getA c = getA (c ^. vc)
instance Definition    UnitDefn where defn = vc . defn
instance Eq            UnitDefn where a == b = usymb a == usymb b
instance ConceptDomain UnitDefn where cdom = cdom . view vc
instance HasUnitSymbol UnitDefn where usymb = getUSymb . view cas
instance IsUnit        UnitDefn where udefn = getDefn . view cas
                                      getUnits = view cu
class MayHaveUnit u where
   getUnit :: u -> Maybe UnitDefn

data UnitEquation = UE {_contributingUnit :: [UID]
                       , _us :: USymb}
makeLenses ''UnitEquation
instance HasUnitSymbol UnitEquation where usymb u = u ^. us

getCu :: UnitEquation -> [UID]
getCu = view contributingUnit

-- | Create a derived unit chunk from a concept and a unit equation
makeDerU :: ConceptChunk -> UnitEquation -> UnitDefn
makeDerU concept eqn = UD concept (Defined (usymb eqn) (USynonym $ usymb eqn)) (getCu eqn)

-- | Create a SI_Unit with two symbol representations
derCUC, derCUC' :: String -> String -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC a b c s ue = UD (dcc a (cn b) c) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) [a]
derCUC' a b c s ue = UD (dcc a (cn' b) c) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) [a]
-- | 
-- | Create a derived unit chunk from an id, term (as 'String'), definition,
-- symbol, and unit equation
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> UnitDefn
-- | Uses self-plural term
derUC  a b c s u = UD (dcc a (cn b) c) (DerivedSI (US [(s,1)]) (fromUDefn u) u) []
-- | Uses term that pluralizes by adding *s* to the end
derUC' a b c s u = UD (dcc a (cn' b) c) (DerivedSI (US [(s,1)]) (fromUDefn u) u) []

derCUC'' :: String -> NP -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC'' a b c s ue = UD (dcc a b c) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) (getCu ue)
-- | Create a derived unit chunk from an id, term (as noun phrase), definition, 
-- symbol, and unit equation
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> UnitDefn
derUC'' a b c s u = UD (dcc a b c) (DerivedSI (US [(s,1)]) (fromUDefn u) u) []

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same string
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = dcc s (cn' s) s
---------------------------------------------------------

-- | For allowing lists to mix the two, thus forgetting
-- the definition part
unitWrapper :: (IsUnit u)  => u -> UnitDefn
unitWrapper u = UD (cc' u (u ^. defn)) (Defined (usymb u) (USynonym $ usymb u)) (getUnits u)

getSecondSymb :: UnitDefn -> Maybe USymb
getSecondSymb c = get_symb2 $ view cas c
  where
    get_symb2 :: UnitSymbol -> Maybe USymb
    get_symb2 (BaseSI _) = Nothing
    get_symb2 (DerivedSI _ v _) = Just v
    get_symb2 (Defined _ _) = Nothing


helperUnit :: UnitDefn -> [UID]
helperUnit a = case getSecondSymb a of
  Just _ -> [a ^. uid]
  Nothing -> getUnits a

--- These conveniences go here, because we need the class
-- | Combinator for raising a unit to a power
(^:) :: UnitDefn -> Integer -> UnitEquation
u ^: i = UE (helperUnit u) (upow (usymb u))
--u ^: i = UE ((helperUnit u) ^. uid) (upow (u ^. usymb))
  where
    upow (US l) = US $ map (second (* i)) l

-- | Combinator for dividing one unit by another
(/:) :: UnitDefn -> UnitDefn -> UnitEquation
u1 /: u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ helperUnit u2) (US $ l1 ++ map (second negate) l2)

-- | Combinator for multiplying two units together
(*:) :: UnitDefn -> UnitDefn -> UnitEquation
u1 *: u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ helperUnit u2) (US $ l1 ++ l2)

-- | Combinator for multiplying a unit and a symbol
(*$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 *$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ getCu u2) (US $ l1 ++ l2)

-- | Combinator for dividing a unit and a symbol
(/$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 /$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ getCu u2) (US $ l1 ++ map (second negate) l2)

-- | Combinator for mulitiplying two unit equations
(^$) :: UnitEquation -> UnitEquation -> UnitEquation
u1 ^$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (getCu u1 ++ getCu u2) (US $ l1 ++ l2)
 
-- | Combinator for scaling one unit by some number
scale :: IsUnit s => Double -> s -> UDefn
scale a b = UScale a (usymb b)

-- | Combinator for shifting one unit by some number
shift :: IsUnit s => Double -> s -> UDefn
shift a b = UShift a (usymb b)

-- | Smart constructor for new derived units from existing units.
newUnit :: String -> UnitEquation -> UnitDefn
newUnit s = makeDerU (unitCon s)

-- | Smart constructor for a "fundamental" unit
fund :: String -> String -> String -> UnitDefn
fund nam desc sym = UD (dcc nam (cn' nam) desc) (BaseSI $ US [(Label sym, 1)]) [nam]

-- | Variant of the above, useful for degree
fund' :: String -> String -> Symbol -> UnitDefn
fund' nam desc sym = UD (dcc nam (cn' nam) desc) (BaseSI $ US [(sym, 1)]) [nam]

-- | We don't want an Ord on units, but this still allows us to compare them
compUnitDefn :: UnitDefn -> UnitDefn -> Ordering
compUnitDefn a b = compUSymb (usymb a) (usymb b)
