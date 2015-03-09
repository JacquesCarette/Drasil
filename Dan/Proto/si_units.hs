module SI_Units where
import Chunk
import Text.PrettyPrint

si_units = newChunk $
  [ ("m", text "for length (metre)"),
    ("kg", text "for mass (kilogram)"),
    ("s", text "for time (second)"),
    ("K", text "for temperature (kelvin)"),
    ("$^oC$", text "for temperature (centigrade)"),
    ("J", text "for energy (joule, J=$\\mathrm{\\frac{kg m^2}{s^2}}$)"),
    ("cal", 
      text "for energy (calorie, cal" <+>
      text "$\\approx$ 4.2 $\\mathrm{\\frac{kg m^2}{s^2}}$)"),
    ("mol", text "for amount of substance (mole)"),
    ("W", text "for power (watt, W=$\\mathrm{\\frac{kgm^2}{s^3}}$)")
  ]