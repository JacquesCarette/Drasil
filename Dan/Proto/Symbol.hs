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
upper_left ul b = Corners [ul] [] [] [] b

sub :: Symbol -> Symbol -> Symbol
sub lr b = Corners [] [] [] [lr] b

