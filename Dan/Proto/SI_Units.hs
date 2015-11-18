{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 

module SI_Units where
import Chunk (VarChunk(..))
import ASTInternal (Field(..), Spec(..), Unit(..),
  Expr(..))
import Unicode (Circle(..), Unicode)
import Format (Format)

fundamentals :: Chunks mode
              
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

metre, kilogram, second, kelvin, joule, calorie, mole, -- centigrade,
              watt, ampere, candela :: Chunk mode

-- Fundamental SI Units --------------------------------------------------------
metre = newChunk "Metre" $
  [ (Symbol, S "m"),
    (Description, S "length (metre)"),
    (Name, S "Metre"),
    (SIU, M $ Fundamental)]
kilogram = newChunk "Kilogram" $
  [ (Symbol, S "kg"),
    (SIU, M $ Fundamental),
    (Description, S "mass (kilogram)"),
    (Name, S "Kilogram")]
second = newChunk "Second" $
  [ (Symbol, S "s"),
    (SIU, M $ Fundamental),
    (Description, S "time (second)"),
    (Name, S "Second")]
kelvin = newChunk "Kelvin" $
  [ (Symbol, S "K"),
    (SIU, M $ Fundamental),
    (Description, S "temperature (kelvin)"),
    (Name, S "Kelvin")]
mole = newChunk "Mole" $
  [ (Symbol, S "mol"),
    (SIU, M $ Fundamental),
    (Description, S "amount of substance (mole)"),
    (Name, S "Mole")]
ampere = newChunk "Ampere" $
  [ (Symbol, S "A"),
    (SIU, M $ Fundamental),
    (Description, S "electric current (ampere)"),
    (Name, S "Ampere")]
candela = newChunk "Candela" $
  [ (Symbol, S "cd"),
    (SIU, M $ Fundamental),
    (Description, S "luminous intensity (candela)"),
    (Name, S "Candela")]

------- END FUNDAMENTALS -------------------------------------------------------

centigrade :: (Format a, Unicode a Circle) => Chunk a
centigrade = newChunk "Centigrade" $
  [ (Symbol, U Circle :+: S "C"), 
    (Description, S "temperature (centigrade)"),
    (Name, S "Centigrade"),
    (SIU, M $ Derived (C kelvin :- (Dbl 273.15)))]
-- Not sure what to do with this right now.
joule = newChunk "Joule" $
  [ (Symbol, S "J"),
    (SIU, M $ Derived ((C kilogram :* (C metre :^ (Int 2))) :/ 
      (C second :^ (Int 2)))),
    (Description, S "energy (joule)"),
    (Name, S "Joule")]
calorie = newChunk "Calorie" $
  [ (Symbol, S "cal"),
    (Description, S ("energy (calorie)")),
    (Name, S "Calorie"),
    (SIU, M $ Derived ((Dbl 4.184) :* (C joule)))]
watt = newChunk "Watt" $
  [ (Symbol, S "W"),
    (Description, S "power (watt)"),
    (Name, S "Watt"),
    (SIU, M $ Derived 
      ((C kilogram :* (C metre :^ (Int 2))) :/ (C second :^ (Int 3))))]
