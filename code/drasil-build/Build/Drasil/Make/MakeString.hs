module Build.Drasil.Make.MakeString where

data MakeString = Mr String
                | Mc MakeString MakeString

instance Semigroup MakeString where
  (<>) = Mc

instance Monoid MakeString where
  mempty = Mr ""

(+:+) :: MakeString -> MakeString -> MakeString
a +:+ (Mr "") = a
(Mr "") +:+ b = b
a +:+ b = a <> Mr " " <> b

renderMS :: MakeString -> String
renderMS (Mr s) = s
renderMS (Mc a b) = renderMS a ++ renderMS b

makeS :: String -> MakeString
makeS = Mr
