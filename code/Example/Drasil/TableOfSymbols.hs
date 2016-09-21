-- Standard code to make a table of symbols.
module Drasil.TableOfSymbols(table_of_symbols) where

import Control.Lens ((^.))

import Language.Drasil

table_of_symbols :: (Unit' s, Quantity s) => [s] -> Section
table_of_symbols ls = Section (S "Table of Symbols") [Con intro, Con (table ls)]

intro :: Contents
intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made with the goal of being consistent with the nuclear " :+:
  S "physics literature and that used in the FP manual.  The SI " :+:
  S "units are listed in brackets following the definition of " :+:
  S "the symbol."
  
table :: (Unit' s) => [s] -> Contents
table ls = Table [S "Symbol", S "Description", S "Units"] (mkTable
  [(\ch -> P (ch ^. symbol)) , 
   (\ch -> ch ^. descr), 
   unit'2Contents]
  ls)
  (S "Table of Symbols") False
