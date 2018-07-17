data Symbol =
    Atomic  String
  | Special Special
--  | Greek   Greek
  | Atop    Decoration Symbol
  | Corners [Symbol] [Symbol] [Symbol] [Symbol] Symbol
          -- upleft   lowleft  upright  lowright base
          -- [1]      [2]      [3]      [4]      [5]
          --  Visually:  [1]   [3]
          --    (out)       [5]
          --             [2]   [4]
  | Concat [Symbol]
            -- [s1, s2] -> s1s2
  | Empty
  deriving Eq

  data Decoration = Hat | Vector | Prime deriving (Eq, Ord)

compsy :: Symbol -> Symbol -> Ordering
compsy (Atomic x)             (Atomic y)            = 
compare (map toLower x) (map toLower y)
compsy (Atomic _)             _                     = LT
compsy  _                    (Atomic _)             = GT
compsy (Special a)           (Special b)            = compare a b
compsy (Special _)            _                     = LT
compsy _                     (Special _)            = GT
compsy  Empty                 Empty                 = EQ
