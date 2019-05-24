module Build.Drasil.Make.MakeString where

type VarName = String
type VarVal = String

data MakeString = Mr String
                | Mv MVar
                | Mc MakeString MakeString

instance Semigroup MakeString where
  (<>) = Mc

instance Monoid MakeString where
  mempty = Mr ""

(+:+) :: MakeString -> MakeString -> MakeString
a +:+ (Mr "") = a
(Mr "") +:+ b = b
a +:+ b = a <> Mr " " <> b

data MVar = Os VarName VarVal VarVal VarVal
          | Implicit VarName
          | Free VarName
          deriving Eq

renderMS :: MakeString -> String
renderMS (Mr s) = s
renderMS (Mv v) = renderVar (\x -> "$(" ++ x ++ ")") v
renderMS (Mc a b) = renderMS a ++ renderMS b

renderVar :: (String -> String) -> MVar -> String
renderVar f (Os nm _ _ _) = f nm
renderVar f (Implicit nm) = "\"" ++ f nm ++ "\""
renderVar f (Free nm) = f nm

makeS :: String -> MakeString
makeS = Mr

mkWindowsVar :: VarName -> VarVal -> VarVal -> MakeString
mkWindowsVar n w e = Mv $ Os n w e e

mkImplicitVar :: VarName -> MakeString
mkImplicitVar = Mv . Implicit

mkFreeVar :: VarName -> MakeString
mkFreeVar = Mv . Free
