module Drasil.GlassBR.DataDescriptions where

import Language.Drasil
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.Interpolation

--from TSD.txt:

read_table :: DataDesc
read_table = 
  [ singleLine (repeated [junk, listEntry [WithPattern] v_z_array]) ',',
    multiLine (repeated [listEntry [WithPattern, WithLine] v_x_array, listEntry [WithPattern, WithLine] v_y_array]) ','
  ]

-----

--from defaultInput.txt:

inputMod :: Mod
inputMod = ModData "InputFormat" [glassInputData]

glassInputData :: DataDesc
glassInputData = 
  [ junkLine,
    singleton plate_len, singleton plate_width, singleton nom_thick,
    junkLine,
    singleton glass_type, 
    junkLine,
    singleton char_weight, 
    junkLine, 
    singleton tNT, 
    junkLine,
    singleton sdx, singleton sdy, singleton sdz,
    junkLine,
    singleton pb_tol
  ]