{-# OPTIONS -Wall #-} 
module SI_Units where
import Chunk
import ASTInternal

si_units :: [Chunk FName FDesc]
si_units = [metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              watt]
metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              watt :: Chunk FName FDesc
metre = newChunk $
  [ (Symbol, S "m"),
    (Description, S "length (metre)") ]
kilogram = newChunk $
  [ (Symbol, S "kg"),
    (Description, S "mass (kilogram)")]
second = newChunk $
  [ (Symbol, S "s"),
    (Description, S "time (second)")]
kelvin = newChunk $
  [ (Symbol, S "K"),
    (Description, S "temperature (kelvin)")]
centigrade = newChunk $
  [ (Symbol, S "$^oC$"), --This is TeX specific, will need to define super/sub
    (Description, S "temperature (centigrade)")] --script somewhere
joule = newChunk $
  [ (Symbol, S "J"),
    (Description, S "energy (joule, J=$\\mathrm{\\frac{kg m^2}{s^2}}$)")]
  -- Again TeX specific formatting used above/below here. Needs to be changed
calorie = newChunk $
  [ (Symbol, S "cal"),
    (Description, S ("energy (calorie, cal $\\approx$ 4.2 " ++
      "$\\mathrm{\\frac{kg m^2}{s^2}}$)"))]
mole = newChunk $
  [ (Symbol, S "mol"),
    (Description, S "amount of substance (mole)")]
watt = newChunk $
  [ (Symbol, S "W"),
    (Description, S "power (watt, W=$\\mathrm{\\frac{kgm^2}{s^3}}$)")]