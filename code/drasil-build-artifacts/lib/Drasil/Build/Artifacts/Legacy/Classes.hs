{-# LANGUAGE FunctionalDependencies #-}
module Drasil.Build.Artifacts.Legacy.Classes (
  HasPathAndDoc(..)
) where

class HasPathAndDoc a b | a -> b where
  getPath :: a -> FilePath
  getDoc :: a -> b
