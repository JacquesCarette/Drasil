{-# LANGUAGE GADTs #-}

module Language.Drasil.DataDesc where

import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.SymbolForm

data DataDesc = DataDesc [Contents] [Data]

data Contents = Contents CodeChunk

data Entry = Entry Contents             -- regular entry (float, int, bool, etc)
           | ListEntry [Ind] Contents   -- index to insert into list
           | Junk  Contents             -- junk should be skipped in input file

data Ind = Explicit Int   -- explicit index
         | WithPattern    -- use current repetition number in repeated pattern
         | WithLine       -- use current line number in multi-line data
           
type Sep = Char  -- delimiter
  
data Data = Singleton Contents
	      | Line LinePattern Sep
          | Lines LinePattern (Maybe Int) Sep   -- multi-line data
                                                -- (Maybe Int) = number of lines, Nothing = unknown so go to end of file  
          
data LinePattern = Straight [Entry]             -- line of data with no pattern
                 | Repeat [Entry] (Maybe Int)   -- line of data with repeated pattern
                                                -- (Maybe Int) = number of repetitions, Nothing = unknown so go to end of line          
          
contents :: (Quantity c, SymbolForm c) => c -> Contents
contents c = Contents $ codevar c


-- for glassbr read_table:

--c_z_array = contents z_array
--c_x_array = contents x_array
--c_y_array = contents y_array

--DataDesc [c_x_array, c_y_array, c_z_array] [
--    Line (Repeat [Junk, ListEntry [WithLine] z_array] Nothing) ',',
--    Lines (Repeat [ListEntry [WithPattern, WithLine] x_array, ListEntry [WithPattern, WithLine] y_array] Nothing) Nothing ','
--  ]
-- ^ pattern evident from TSD.txt

-----

--for glassbr defaultInput:

--c_a     = contents a     --plate length
--c_b     = contents b     --plate width
--c_t     = contents t     --nominal thickness
--c_gt    = contents gt    --glass type
--c_w     = contents w     --weight of charge
--c_tnt   = contents tnt   --tnt equivalrny factor
--c_sdx   = contents sdx   --stand off dist (x)
--c_sdy   = contents sdy   --stand off dist (y)
--c_sdz   = contents sdz   --stand off dist (z)
--c_pbtol = contents pbTol --tolerable probability

{-
DataDesc [c_a, c_b, c_t, c_gt, c_w, c_tnt, c_sdx, c_sdy, c_sdz, c_pbtol]
  [Singleton Junk,
  Singleton c_a, Singleton c_b, Singleton c_t,
  Singleton Junk,
  Singleton c_gt, 
  Singleton Junk,
  Singleton c_w, 
  Singleton Junk, 
  Singleton c_tnt, 
  Singleton Junk,
  Singleton c_sdx, Singleton c_sdy, Singleton c_sdz,
  Singleton Junk,
  Singleton c_pbtol
  ]

--FIXME: replace "Singleton _" with "Line Straight [_]" ?

-}