module Modules.InputParameters (inputParameters) where

import Language.Drasil.Code

-- TODO:  improve gool support for lists
variables :: [StateVar]
variables = [ 
    pubMVar 4 float "a",
    pubMVar 4 float "b",
    pubMVar 4 string "t",
    pubMVar 4 string "gt",
    pubMVar 4 float "w",
    pubMVar 4 float "tnt",
    pubMVar 4 float "sdx",
    pubMVar 4 float "sdy",
    pubMVar 4 float "sdz",
    pubMVar 4 float "pbtol",
    pubMVar 4 float "asprat",
    pubMVar 4 float "sd",
    pubMVar 4 float "h",
    pubMVar 4 float "gtf",
    pubMVar 4 float "ldf",
    pubMVar 4 float "wtnt",
    pubGVar 4 float "E",
    pubGVar 4 float "td",
    pubGVar 4 float "m",
    pubGVar 4 float "k",
    pubGVar 4 float "lsf"
  ]

vars :: [Value]
vars = map svToVar variables

vals :: [Value]
vals = [
    litFloat 0.0,
    litFloat 0.0,
    litString "2.5",
    litString "AN",
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    litFloat 0.0,
    (litFloat 7.17) #* ((litFloat 10.0) #^ (litFloat 7.0)),
    litFloat 3.0,
    litFloat 7.0,
    (litFloat 2.86) #* ((litFloat 10.0) #^ (litFloat (-53.0))),
    litFloat 1.0
  ]
  
inputParameters :: Module
inputParameters = buildModule "InputParameters" [] [] [] [inputParametersClass]

inputParametersClass :: Class
inputParametersClass = pubClass
  "InputParameters"
  Nothing
  variables
  [ constructor 
      "InputParameters" 
      []
      [zipBlockWith (&=) vars vals]
  ]      
    