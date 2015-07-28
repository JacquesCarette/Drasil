{-# OPTIONS -Wall #-} 
module SI_Units where
import Chunk
import ASTInternal

fundamentals :: Chunks
              
fundamentals = [metre] --, kilogram, second, kelvin, mole, ampere, candela]

metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
	watt, ampere, candela :: Chunk

-- Fundamental SI Units --------------------------------------------------------
metre = newChunk $
  [ (Symbol, S "m"),
    (Description, S "length (metre)"),
    (Name, S "Metre"),
    (SIU, SI Fundamental)]
kilogram = newChunk $
  [ (Symbol, S "kg"),
    (SIU, SI Fundamental),
    (Description, S "mass (kilogram)"),
    (Name, S "Kilogram")]
second = newChunk $
  [ (Symbol, S "s"),
    (SIU, SI Fundamental),
    (Description, S "time (second)"),
    (Name, S "Second")]
kelvin = newChunk $
  [ (Symbol, S "K"),
    (SIU, SI Fundamental),
    (Description, S "temperature (kelvin)"),
    (Name, S "Kelvin")]
mole = newChunk $
  [ (Symbol, S "mol"),
    (SIU, SI Fundamental),
    (Description, S "amount of substance (mole)"),
    (Name, S "Mole")]
ampere = newChunk $
  [ (Symbol, S "A"),
    (SIU, SI Fundamental),
    (Description, S "electric current"),
    (Name, S "Ampere")]
candela = newChunk $
  [ (Symbol, S "cd"),
    (SIU, SI Fundamental),
    (Description, S "luminous intensity"),
    (Name, S "Candela")]
------- END FUNDAMENTALS -------------------------------------------------------

centigrade = newChunk $
  [ (Symbol, U Circle :+: S "C"),
    (Description, S "temperature (centigrade)"),
    (Name, S "Centigrade"),
    (SIU, SI $ Derived (C kelvin :- (Dbl 273.15)))]
joule = newChunk $
  [ (Symbol, S "J"),
    (SIU, SI $ Derived ((C kilogram :* (C metre :^ (Int 2))) :/ 
                           (C second :^ (Int 2)))),
    (Description, S "energy"),
    (Name, S "Joule")]
calorie = newChunk $
  [ (Symbol, S "cal"),
    (Description, S ("energy")),
    (Name, S "Calorie"),
    (SIU, SI $ Derived ((Dbl 4.184) :* (C joule)))]
watt = newChunk $
  [ (Symbol, S "W"),
    (Description, S "power"),
    (Name, S "Watt"),
    (SIU, SI $ 
      Derived ((C kilogram :* (C metre :^ (Int 2))) :/ (C second :^ (Int 3))))]