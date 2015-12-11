{-# OPTIONS -Wall #-}
module Symbol where

type Parameters = [Symbol]
type Variables = [Symbol]

data Symbol = Atomic String
            | Composite Symbol Parameters Variables --Need some way to explain how the composite is supposed to be viewed. Currently flying blind.
            