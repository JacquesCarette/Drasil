module Drasil.GlassBR.DataDescriptions where

import Language.Drasil (junkLine, singleton, repeated, singleLine,
  multiLine, listEntry, junk,
  DataDesc, Mod (ModData), Ind (WithPattern, WithLine))

import Drasil.GlassBR.Unitals (plate_len, plate_width, nom_thick,
  glass_type, char_weight, tNT, sdx, sdy, sdz, pb_tol)
import Drasil.GlassBR.Interpolation (v_z_array, v_x_array, v_y_array)

--from TSD.txt:

read_table :: DataDesc
read_table = 
  [ singleLine (repeated [junk, listEntry [WithPattern] v_z_array]) ',',
    multiLine (repeated [listEntry [WithPattern, WithLine] v_x_array,
                         listEntry [WithPattern, WithLine] v_y_array]) ','
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
