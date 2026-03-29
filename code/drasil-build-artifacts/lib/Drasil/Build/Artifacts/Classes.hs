{-# LANGUAGE FunctionalDependencies #-}
module Drasil.Build.Artifacts.Classes (
  HasPathAndDoc(..)
) where

class HasPathAndDoc a b | a -> b where
  getPath :: a -> FilePath
  getDoc :: a -> b
