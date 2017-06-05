module Modules.InputParameters (inputParameters) where

import Language.Drasil.Code
import Defs
 
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

variables :: [StateVar]
variables = [ 
    pubMVar 4 float l_a,
    pubMVar 4 float l_b,
    pubMVar 4 float l_t,
    pubMVar 4 int l_gt,
    pubMVar 4 float l_w,
    pubMVar 4 float l_tnt,
    pubMVar 4 float l_sdx,
    pubMVar 4 float l_sdy,
    pubMVar 4 float l_sdz,
    pubMVar 4 float l_pbtol,
    pubMVar 4 float l_asprat,
    pubMVar 4 float l_sd,
    pubMVar 4 float l_h,
    pubMVar 4 float l_gtf,
    pubMVar 4 float l_ldf,
    pubMVar 4 float l_wtnt,
    pubGVar 4 float l_E,
    pubGVar 4 float l_td,
    pubGVar 4 float l_m,
    pubGVar 4 float l_k,
    pubGVar 4 float l_lsf
  ]

vars :: [Value]
vars = map svToVar variables

vals :: [Value]
vals = [
    litFloat 0.0,
    litFloat 0.0,
    litFloat 2.5,
    litInt 1,
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