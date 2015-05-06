module SI_Units where
import Chunk
import Text.PrettyPrint

si_units = newChunk $
  [ ("m", "for length (metre)"),
    ("kg", "for mass (kilogram)"),
    ("s", "for time (second)"),
    ("K", "for temperature (kelvin)"),
    ("$^oC$", "for temperature (centigrade)"),
    ("J", "for energy (joule, J=$\\mathrm{\\frac{kg m^2}{s^2}}$)"),
    ("cal", 
      "for energy (calorie, cal " ++
      "$\\approx$ 4.2 $\\mathrm{\\frac{kg m^2}{s^2}}$)"),
    ("mol", "for amount of substance (mole)"),
    ("W", "for power (watt, W=$\\mathrm{\\frac{kgm^2}{s^3}}$)")
  ]