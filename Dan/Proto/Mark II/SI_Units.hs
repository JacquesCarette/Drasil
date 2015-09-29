{-# OPTIONS -Wall #-} 
module SI_Units where
import Chunk
import ASTInternal (Chunk,Chunks, Field(..), Spec(..), Unit(..),
  Unicode(..), Expr(..))

fundamentals :: Chunks
              
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              watt, ampere, candela :: Chunk

-- Fundamental SI Units --------------------------------------------------------
metre = newChunk $
  [ (Symbol, S "m"),
    (Description, S "length (metre)"),
    (Name, S "Metre"),
    (SIU, M $ Fundamental "m")]
kilogram = newChunk $
  [ (Symbol, S "kg"),
    (SIU, M $ Fundamental "kg"),
    (Description, S "mass (kilogram)"),
    (Name, S "Kilogram")]
second = newChunk $
  [ (Symbol, S "s"),
    (SIU, M $ Fundamental "s"),
    (Description, S "time (second)"),
    (Name, S "Second")]
kelvin = newChunk $
  [ (Symbol, S "K"),
    (SIU, M $ Fundamental "K"),
    (Description, S "temperature (kelvin)"),
    (Name, S "Kelvin")]
mole = newChunk $
  [ (Symbol, S "mol"),
    (SIU, M $ Fundamental "mol"),
    (Description, S "amount of substance (mole)"),
    (Name, S "Mole")]
ampere = newChunk $
  [ (Symbol, S "A"),
    (SIU, M $ Fundamental "A"),
    (Description, S "electric current (ampere)"),
    (Name, S "Ampere")]
candela = newChunk $
  [ (Symbol, S "cd"),
    (SIU, M $ Fundamental "cd"),
    (Description, S "luminous intensity (candela)"),
    (Name, S "Candela")]

------- END FUNDAMENTALS -------------------------------------------------------

centigrade = newChunk $
  [ (Symbol, U Circle :+: S "C"), 
    (Description, S "temperature (centigrade)"),
    (Name, S "Centigrade"),
    (SIU, M $ Derived "$^oC$" (C kelvin :- (Dbl 273.15)))]--This is TeX specific
joule = newChunk $
  [ (Symbol, S "J"),
    (SIU, M $ Derived "J" ((C kilogram :* (C metre :^ (Int 2))) :/ 
      (C second :^ (Int 2)))),
    (Description, S "energy (joule)"),
    (Name, S "Joule")]
calorie = newChunk $
  [ (Symbol, S "cal"),
    (Description, S ("energy (calorie)")),
    (Name, S "Calorie"),
    (SIU, M $ Derived "cal" ((Dbl 4.184) :* (C joule)))]
watt = newChunk $
  [ (Symbol, S "W"),
    (Description, S "power (watt)"),
    (Name, S "Watt"),
    (SIU, M $ Derived "W" 
      ((C kilogram :* (C metre :^ (Int 2))) :/ (C second :^ (Int 3))))]
