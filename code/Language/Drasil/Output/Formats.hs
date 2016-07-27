{-# LANGUAGE GADTs #-}
module Language.Drasil.Output.Formats where

type Filename = String

data DocType = SRS Filename     --Filename with no extension
             | MG Filename
             | MIS Filename
             | LPM Filename
             | Website Filename

instance Show DocType where
  show (SRS _) = "SRS"
  show (MG _)  = "MG"
  show (MIS _) = "MIS"
  show (LPM _) = "LPM"
  show (Website _) = "Website"
             
data DocClass = DocClass (Maybe String) String
newtype UsePackages = UsePackages [String] -- Package name list
data ExDoc = ExDoc (Maybe String) String
