{-# LANGUAGE GADTs #-}

module Language.Drasil.DataDesc where

import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.SymbolForm

data DataDesc = DataDesc [Contents] [Data]

data Contents = Contents CodeChunk

data Entry = Entry Contents             -- regular entry (float, int, bool, etc)
           | ListEntry [Ind] Contents   -- index to insert into list
           | Junk                       -- junk should be skipped in input file

data Ind = Explicit Int   -- explicit index
         | WithPattern    -- use current repetition number in repeated pattern
         | WithLine       -- use current line number in multi-line data
           
type Sep = Char  -- delimiter
  
data Data = Line LinePattern Sep
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