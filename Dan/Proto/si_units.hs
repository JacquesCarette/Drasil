{-# OPTIONS -Wall #-} 
module SI_Units where
import Chunk
import ASTInternal

si_units, fundamentals :: [Chunk FName FDesc]
si_units = [metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              watt]
              
fundamentals = [metre, kilogram, second, kelvin, mole, ampere, candela]

metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              watt, ampere, candela :: Chunk FName FDesc

-- Fundamental SI Units --------------------------------------------------------
metre = newChunk $
  [ (Symbol, S "m"),
    (Description, S "length (metre)"),
    (Name, S "Metre")]
kilogram = newChunk $
  [ (Symbol, S "kg"),
    (Description, S "mass (kilogram)"),
    (Name, S "Kilogram")]
second = newChunk $
  [ (Symbol, S "s"),
    (Description, S "time (second)"),
    (Name, S "Second")]
kelvin = newChunk $
  [ (Symbol, S "K"),
    (Description, S "temperature (kelvin)"),
    (Name, S "Kelvin")]
mole = newChunk $
  [ (Symbol, S "mol"),
    (Description, S "amount of substance (mole)"),
    (Name, S "Mole")]
ampere = newChunk $
  [ (Symbol, S "A"),
    (Description, S "electric current"),
    (Name, S "Ampere")]
candela = newChunk $
  [ (Symbol, S "cd"),
    (Description, S "luminous intensity"),
    (Name, S "Candela")]

------- END FUNDAMENTALS -------------------------------------------------------

centigrade = newChunk $
  [ (Symbol, S "$^oC$"), --This is TeX specific, will need to define super/sub
    (Description, S "temperature (centigrade)"),
    (Name, S "Centigrade")] --script somewhere
joule = newChunk $
  [ (Symbol, S "J" {- M $ Derived "J" ((C kilogram :* (C metre:^:(Int 2))) :/ 
      (C second:^:(Int 2)))-}),
    (Description, S "energy (joule, J=$\\mathrm{\\frac{kg m^2}{s^2}}$)"),
    (Name, S "Joule")]
  -- Again TeX specific formatting used above/below here. Needs to be changed
calorie = newChunk $
  [ (Symbol, S "cal"),
    (Description, S ("energy (calorie, cal $\\approx$ 4.2 " ++
      "$\\mathrm{\\frac{kg m^2}{s^2}}$)")),
    (Name, S "Calorie")]
watt = newChunk $
  [ (Symbol, S "W"),
    (Description, S "power (watt, W=$\\mathrm{\\frac{kgm^2}{s^3}}$)"),
    (Name, S "Watt")]