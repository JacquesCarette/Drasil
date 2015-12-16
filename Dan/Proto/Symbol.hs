{-# OPTIONS -Wall #-}
module Symbol where
import Unicode 

type Parameters = [Symbol]
type Variables = [Symbol]



data Symbol = Atomic String
            | Composite Symbol Parameters Variables --Need some way to explain how the composite is supposed to be viewed. Currently flying blind.
            | Circ Circle --This is a hack. I'm thinking a GADT might be necessary to include special chars / "renderable" characters.
            | Ta Tau