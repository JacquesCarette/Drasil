{-# LANGUAGE GADTs #-}
module Language.Drasil.Output.Formats where

type Filename = String

data DocType = SRS Filename     --Filename with no extension
             | LPM Filename
             | Code Filename
             | Website Filename
             
data DocClass = DocClass String String -- Sqbracks vs Braces
newtype UsePackages = UsePackages [String] -- Package name list
data ExDoc = ExDoc String String -- SqBracks vs. Braces
