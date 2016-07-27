-- Standard code to make a table of symbols.
module Example.Drasil.GlassBR.TableOfSymbols where

import Control.Lens ((^.))

import Language.Drasil
import Language.Drasil.Unit

table_of_symbols :: (Unit' s, Quantity s) => [s] -> Section
table_of_symbols ls = Section 1 (S "Table of Symbols") [Con intro, Con (table ls)]

intro :: Contents
intro = Paragraph $ 
 S "The table that follows summarizes the symbols used in this " :+:
 S "document along with their units. The symbols are listed in " :+:
 S "alphabetical order." 
  
table :: (Unit' s, Quantity s) => [s] -> Contents
table ls = Table [S "Symbol", S "Description", S "Units"] (mkTable
  [(\ch -> P (ch ^. symbol)) , 
   (\ch -> ch ^. descr), 
   (\ch -> maybeUnits (ch ^. unit'))]
  ls)
  (S "Table of Symbols") False

maybeUnits (Just x) = Sy x
maybeUnits Nothing = S ""