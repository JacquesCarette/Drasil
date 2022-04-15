-- | Defines types and functions for generating Makefiles.
module Build.Drasil.Make.MakeString where

-- * Types

-- | Type synonym for variable names.
type VarName = String
-- | Type synonym for variable values.
type VarVal = String

data MakeString = Mr String -- ^ A string for Makefiles.
                | Mv MVar -- ^ Holds a Makefile variable.
                | Mc MakeString MakeString -- ^ Concatenates two 'MakeString's.

instance Semigroup MakeString where
  (<>) = Mc

instance Monoid MakeString where
  mempty = Mr ""

-- | For creating Makefile variables.
data MVar = Os VarName VarVal VarVal VarVal -- ^ Operating System specific variable. Holds information for Windows, Mac, and Linux systems.
          | Implicit VarName -- ^ Implicit OS.
          | Free VarName -- ^ Independent of OS.
          deriving Eq

-- * Functions

-- | Concatenates two 'MakeString's with a space in between.
(+:+) :: MakeString -> MakeString -> MakeString
a +:+ (Mr "") = a
(Mr "") +:+ b = b
a +:+ b = a <> Mr " " <> b

-- | Renders a 'MakeString'. Variables have the form \"$(@var@)\".
renderMS :: MakeString -> String
renderMS (Mr s) = s
renderMS (Mv v) = renderVar (\x -> "$(" ++ x ++ ")") v
renderMS (Mc a b) = renderMS a ++ renderMS b

-- | Renders variables. Takes in a function for the variable, and the type of variable.
renderVar :: (String -> String) -> MVar -> String
renderVar f (Os nm _ _ _) = f nm
renderVar f (Implicit nm) = "\"" ++ f nm ++ "\""
renderVar f (Free nm) = f nm

-- | Constructor for converting a 'String' into a 'MakeString'.
makeS :: String -> MakeString
makeS = Mr

-- | Constructor for Windows OS variables.
mkWindowsVar :: VarName -> VarVal -> VarVal -> MakeString
mkWindowsVar n w e = Mv $ Os n w e e

-- | Constructor for OS variables.
mkOSVar :: VarName -> VarVal -> VarVal -> VarVal -> MakeString
mkOSVar n w m l = Mv $ Os n w m l

-- | Constructor for 'Implicit' variables.
mkImplicitVar :: VarName -> MakeString
mkImplicitVar = Mv . Implicit

-- | Constructor for 'Free' variables.
mkFreeVar :: VarName -> MakeString
mkFreeVar = Mv . Free
