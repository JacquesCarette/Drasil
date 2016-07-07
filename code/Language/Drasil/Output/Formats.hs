{-# LANGUAGE GADTs #-}
module Language.Drasil.Output.Formats where

type Filename = String

data DocType = SRS Filename     --Filename with no extension
             | MG Filename
             | LPM Filename
             | Code Filename
             | Website Filename
             
data DocParams = DocClass String String --SqBracks vs. Braces
               | UsePackages [String] -- Package name list
               | ExDoc String String --SqBracks vs. Braces
