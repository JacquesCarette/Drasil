{-# LANGUAGE GADTs #-}

-- A 'Symbol' is actually going to be a graphical description of what
-- gets rendered as a (unique) symbol.  This is actually NOT based on
-- semantics at all, but just a description of how things look.

module Symbol where
import Unicode 

data Symbol where
  Atomic :: String -> Symbol
  Special :: Render a => a -> Symbol
  -- ul, ll, ur, lr
  Corners :: [Symbol] -> [Symbol] -> [Symbol] -> [Symbol] -> Symbol -> Symbol
  Catenate :: Symbol -> Symbol -> Symbol

upper_left :: Symbol -> Symbol -> Symbol
upper_left b ul = Corners [ul] [] [] [] b

sub :: Symbol -> Symbol -> Symbol
sub b lr = Corners [] [] [] [lr] b

sup :: Symbol -> Symbol -> Symbol
sup b ur = Corners [] [] [ur] [] b