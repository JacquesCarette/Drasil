module Language.Drasil.RefProg (RefInfo(..)) where

-- RefInfo is here to avoid any import cycles between Reference and Sentence.

-- | Holds any extra information needed for a 'Reference', be it an equation, pages, a note, or nothing.
data RefInfo = None
             | Equation [Int]
             | Page [Int]
             | RefNote String