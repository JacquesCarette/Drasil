{-# LANGUAGE GADTs #-}
module Language.Drasil.Output.Formats where

type Filename = String

data DocType = SRS Filename     --Filename with no extension
             | LPM Filename
             | Code Filename
             | Website Filename
             
data DocClass = DocClass (Maybe String) String
newtype UsePackages = UsePackages [String] -- Package name list
data ExDoc = ExDoc (Maybe String) String
