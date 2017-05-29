module Modules.Interpolation (interpolation) where

import Language.Drasil.Code
import Prelude hiding (return)

-- TODO:  add gool support for non-object functions;  don't need a class for this
interpolation :: Module
interpolation = buildModule "Interpolation" ["numpy"] [] [interpolationFunc] []

interpolationFunc :: FunctionDecl
interpolationFunc = pubMethod (methodType float) "lin_interp" [y1, y2, x1, x2, input_param] 
    [ 
      block [
        varDecDef y0 float $ paramToVar y1 #+ (paramToVar y2 #- paramToVar y1) #/ (paramToVar x2 #- paramToVar x1) #* (paramToVar input_param #- paramToVar x1),
        return $ var y0
      ]
    ]     

    
y1, y2, x1, x2, input_param :: Parameter
y1 = param "y1" float
y2 = param "y2" float
x1 = param "x1" float
x2 = param "x2" float
input_param = param "input_param" float

y0 :: Label
y0 = "y0"