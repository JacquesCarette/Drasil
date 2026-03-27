{-# LANGUAGE FunctionalDependencies #-}
module Drasil.Artifacts.Classes (
  HasPathAndDoc(..)
) where

class HasPathAndDoc a b | a -> b where
  getPath :: a -> FilePath
  getDoc :: a -> b
