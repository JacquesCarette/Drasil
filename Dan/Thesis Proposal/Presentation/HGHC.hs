module Example.Drasil.HGHC(srsBody, lpmBody) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Example.Drasil.HeatTransfer
import Example.Drasil.Units
import Example.Drasil.TableOfSymbols

import Language.Drasil
import Language.Drasil.SI_Units (si_units)

vars :: [EqChunk]
vars = [h_g, h_c]

s1, s2, s3, s4 :: LayoutObj
s1 = table_of_units si_units
s2 = table_of_symbols vars
s3 = Section 0 (S "Data Definitions") $ map (Definition . Data) vars
s4 = Section 0 (S "Code -- Test") $ map (CodeBlock . toCode CLang Calc) [h_c]

srs :: Quantity s => [s] -> String -> [LayoutObj] -> Document
srs ls author body =
  Document ((S "SRS for ") :+: 
    (foldr1 (:+:) (intersperse (S " and ") (map (\x -> U $ x ^. symbol) ls))))
    (S author) body  
  
srsBody :: Document
srsBody = srs vars "Spencer Smith" [s1, s2, s3, s4]