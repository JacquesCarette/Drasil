{-# Language TemplateHaskell #-}
module Language.Drasil.Development.Unit (
    UnitDefn(..)
  , from_udefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, comp_unitdefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit)
  , IsUnit(getUnits)
  ) where

import Control.Lens ((^.), makeLenses, view)
import Control.Arrow (second)
import Data.Maybe
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), HasUnitSymbol(usymb), IsUnit(udefn, getUnits))
import Language.Drasil.Chunk.Concept (ConceptChunk, dcc, cc')
import Language.Drasil.Symbol (Symbol(Atomic))
import Language.Drasil.UnitLang (USymb(US), UDefn(UScale, USynonym, UShift), comp_usymb, from_udefn)
import Language.Drasil.UID
import Language.Drasil.NounPhrase (cn,cn',NP)

-- | for defining units
-- It is a concept chunk (defined what kind of unit it is),
-- has a unit symbol, maybe another (when it is a synonym),
-- perhaps a definition, and the list of UID of the units that make up
-- the definition.
data UnitDefn = UD { _vc :: ConceptChunk, 
                     _fsymb :: Maybe USymb,
                     _dsymb :: Maybe USymb,
                     _ud :: Maybe UDefn,
                     _cu :: [UID]}
makeLenses ''UnitDefn

instance HasUID        UnitDefn where uid = vc . uid
instance NamedIdea     UnitDefn where term   = vc . term
instance Idea          UnitDefn where getA c = getA (c ^. vc)
instance Definition    UnitDefn where defn = vc . defn
instance Eq            UnitDefn where a == b = (usymb a) == (usymb b)
instance ConceptDomain UnitDefn where cdom = vc . cdom
instance HasUnitSymbol UnitDefn where usymb u = if (isNothing $ u ^. fsymb) then (fromJust $ u ^. dsymb)
                                                  else (fromJust $ u ^. fsymb)
instance IsUnit        UnitDefn where udefn = ud
                                      getUnits u = view cu u
class MayHaveUnit u where
   getUnit :: u -> Maybe UnitDefn

data UnitEquation = UE {_contributingUnit :: [UID], _us :: USymb}
makeLenses ''UnitEquation
instance HasUnitSymbol UnitEquation where usymb u = u ^. us

getSecondSymb :: UnitDefn -> Maybe USymb
getSecondSymb c = view fsymb c

getCu :: UnitEquation -> [UID]
getCu a = view contributingUnit a

-- | Create a derived unit chunk from a concept and a unit equation
makeDerU :: ConceptChunk -> UnitEquation -> UnitDefn
makeDerU concept eqn = UD concept Nothing (Just $ from_udefn $ USynonym $ usymb eqn) (Just $ USynonym $ usymb eqn) (getCu eqn)

-- | Create a SI_Unit with two symbol representations
derCUC, derCUC' :: String -> String -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC a b c s ue = UD (dcc a (cn b) c) (Just $ US [(s,1)]) (Just $ usymb ue) (Just $ USynonym $ usymb ue) [a]
derCUC' a b c s ue = UD (dcc a (cn' b) c) (Just $ US [(s,1)]) (Just $ usymb ue) (Just $ USynonym $ usymb ue) [a]
-- | 
-- | Create a derived unit chunk from an id, term (as 'String'), definition,
-- symbol, and unit equation
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> UnitDefn
-- | Uses self-plural term
derUC  a b c s u = UD (dcc a (cn b) c) (Just $ US [(s,1)]) (Just $ from_udefn u) (Just u) []
-- | Uses term that pluralizes by adding *s* to the end
derUC' a b c s u = UD (dcc a (cn' b) c) (Just $ US [(s,1)]) (Just $ from_udefn u) (Just u) []

derCUC'' :: String -> NP -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC'' a b c s ue = UD (dcc a b c) (Just $ US [(s,1)]) (Just $ usymb ue) (Just $ USynonym $ usymb ue) (getCu ue)
-- | Create a derived unit chunk from an id, term (as noun phrase), definition, 
-- symbol, and unit equation
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> UnitDefn
derUC'' a b c s u = UD (dcc a b c) (Just $ US [(s,1)]) (Just $ from_udefn u) (Just u) []

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same string
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = dcc s (cn' s) s
---------------------------------------------------------

-- | For allowing lists to mix the two, thus forgetting
-- the definition part
unitWrapper :: (IsUnit u)  => u -> UnitDefn
unitWrapper u = UD (cc' u (u ^. defn)) (Just $ usymb u) Nothing (u ^. udefn) (getUnits u)

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
  UE ((helperUnit u1) ++ (helperUnit u2)) (US $ l1 ++ map (second negate) l2)

-- | Combinator for multiplying two units together
(*:) :: UnitDefn -> UnitDefn -> UnitEquation
u1 *: u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE ((helperUnit u1) ++ (helperUnit u2)) (US $ l1 ++ l2)

-- | Combinator for multiplying a unit and a symbol
(*$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 *$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE ((helperUnit u1) ++(getCu u2)) (US $ l1 ++ l2)

-- | Combinator for dividing a unit and a symbol
(/$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 /$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE ((helperUnit u1) ++ (getCu u2)) (US $ l1 ++ map (second negate) l2)

-- | Combinator for mulitiplying two unit equations
(^$) :: UnitEquation -> UnitEquation -> UnitEquation
u1 ^$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE ((getCu u1)++(getCu u2)) (US $ l1 ++ l2)
 
-- | Combinator for scaling one unit by some number
scale :: IsUnit s => Double -> s -> UDefn
scale a b = UScale a (usymb b)

-- | Combinator for shifting one unit by some number
shift :: IsUnit s => Double -> s -> UDefn
shift a b = UShift a (usymb b)

-- | Smart constructor for new derived units from existing units.
new_unit :: String -> UnitEquation -> UnitDefn
new_unit s u = makeDerU (unitCon s) u

-- | Smart constructor for a "fundamental" unit
fund :: String -> String -> String -> UnitDefn
fund nam desc sym = UD (dcc nam (cn' nam) desc) (Just $ US [(Atomic sym, 1)]) Nothing Nothing [nam]

-- | We don't want an Ord on units, but this still allows us to compare them
comp_unitdefn :: UnitDefn -> UnitDefn -> Ordering
comp_unitdefn a b = comp_usymb (usymb a) (usymb b)
