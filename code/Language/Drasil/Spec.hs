{-# LANGUAGE GADTs #-}
module Language.Drasil.Spec where

import Language.Drasil.Unicode (Greek,Special)
import Language.Drasil.Symbol
import Data.Char (toLower)
import Data.Drasil.Plurals (irregularPlurals, pLook)

data Accent = Grave | Acute deriving Eq

-- For writing "sentences" via combining smaller elements

-- Sentences are made up of some known vocabulary of things:
-- - units (their visual representation)
-- - words (via String)
-- - special characters
-- - accented letters
-- - References to specific layout objects
infixr 5 :+:
data Sentence where
  Sy    :: USymb -> Sentence
  S     :: String -> Sentence       -- Strings, used for Descriptions in Chunks
  G     :: Greek -> Sentence
  Sp    :: Special -> Sentence
  P     :: Symbol -> Sentence
  F     :: Accent -> Char -> Sentence  -- Special formatting for certain special
                                       -- chars
  Ref   :: RefType -> Sentence -> Sentence  -- Needs helper func to create Ref
                                    -- See Reference.hs
  Quote :: Sentence -> Sentence     -- Adds quotation marks around a sentence
                                    
  -- Direct concatenation of two Specs (no implicit spaces!)
  (:+:) :: Sentence -> Sentence -> Sentence   

-- Language of unit equations, to define a unit relative
-- to another
--Moving this here to avoid cyclic imports
data USymb = UName Symbol
           | UProd [USymb]
           | UPow USymb Integer --can be negative, should not be 0
           | UDiv USymb USymb   --Get proper division (not neg pow)
                                --  necessary for things like J/(kg*C)

data RefType = Tab
             | Fig
             | Sect
             | Def
             | Mod
             | Req
             | Assump
             | LC
             | UC

instance Show RefType where
  show Tab = "Table"
  show Fig = "Figure"
  show Sect = "Section"
  show Def = "Definition"
  show Mod = "Module"
  show Req = "Requirement"
  show Assump = "Assumption"
  show LC = "Likely Change"
  show UC = "Unlikely Change"
  
-- this is a horrible hack that assumes too much from sentences!
sMap :: (String->String) -> Sentence -> Sentence
sMap f (S a) = S (f a)
sMap f (a :+: b) = sMap f a :+: sMap f b
sMap _ a = a

sLower :: Sentence -> Sentence
sLower = sMap (map toLower)

sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

(+:+) :: Sentence -> Sentence -> Sentence
(S "") +:+ b = b
a +:+ (S "") = a
a +:+ b = a :+: S " " :+: b

sC :: Sentence -> Sentence -> Sentence
a `sC` b = a :+: S "," +:+ b

(+:+.) :: Sentence -> Sentence -> Sentence
a +:+. b = a +:+ b :+: S "."
