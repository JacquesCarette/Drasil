{-# LANGUAGE FunctionalDependencies #-}
module Utils.Drasil.TypeClasses (
  HasPathAndDoc(..)
) where

class HasPathAndDoc a b | a -> b where
  getPath :: a -> FilePath
  getDoc :: a -> b
