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
    pubMVar 0 float l_a,
    pubMVar 0 float l_b,
    pubMVar 0 float l_t,
    pubMVar 0 int l_gt,
    pubMVar 0 float l_w,
    pubMVar 0 float l_tnt,
    pubMVar 0 float l_sdx,
    pubMVar 0 float l_sdy,
    pubMVar 0 float l_sdz,
    pubMVar 0 float l_pbtol,
    pubMVar 0 float l_asprat,
    pubMVar 0 float l_sd,
    pubMVar 0 float l_h,
    pubMVar 0 float l_gtf,
    pubMVar 0 float l_ldf,
    pubMVar 0 float l_wtnt,
    pubMVar 0 float l_E,
    pubMVar 0 float l_td,
    pubMVar 0 float l_m,
    pubMVar 0 float l_k,
    pubMVar 0 float l_lsf
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