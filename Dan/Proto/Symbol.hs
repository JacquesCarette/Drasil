{-# LANGUAGE GADTs #-}
module Symbol where
import Unicode 

type Parameters = [Symbol]
type Variables = [Symbol]

data Symbol where
  Atomic :: String -> Symbol
  Special :: Render a => a -> Symbol
  -- Need some way to explain how the composite is supposed to be viewed. 
  -- Currently flying blind.
  Composite :: Symbol -> Parameters -> Variables -> Symbol
