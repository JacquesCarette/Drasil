module SI_Units where
import Chunk
import Text.PrettyPrint
import Config

si_units = [metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              watt]
  
metre = newChunk $
  [ (Symbol, "m"),
    (Description, "length (metre)") ]
kilogram = newChunk $
  [ (Symbol, "kg"),
    (Description, "mass (kilogram)")]
second = newChunk $
  [ (Symbol, "s"),
    (Description, "time (second)")]
kelvin = newChunk $
  [ (Symbol, "K"),
    (Description, "temperature (kelvin)")]
centigrade = newChunk $
  [ (Symbol, "$^oC$"), --This is TeX specific, will need to define super/sub
    (Description, "temperature (centigrade)")] --script somewhere
joule = newChunk $
  [ (Symbol, "J"),
    (Description, "energy (joule, J=$\\mathrm{\\frac{kg m^2}{s^2}}$)")]
  -- Again TeX specific formatting used above/below here. Needs to be changed
calorie = newChunk $
  [ (Symbol, "cal"),
    (Description, "energy (calorie, cal $\\approx$ 4.2 " ++
      "$\\mathrm{\\frac{kg m^2}{s^2}}$)")]
mole = newChunk $
  [ (Symbol, "mol"),
    (Description, "amount of substance (mole)")]
watt = newChunk $
  [ (Symbol, "W"),
    (Description, "power (watt, W=$\\mathrm{\\frac{kgm^2}{s^3}}$)")]