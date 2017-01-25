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
a +:+ b = a :+: S " " :+: b

sC :: Sentence -> Sentence -> Sentence
a `sC` b = a :+: S "," +:+ b

(+:+.) :: Sentence -> Sentence -> Sentence
a +:+. b = a +:+ b :+: S "."

--This will likely need to be moved.
data PluralType = AddS
                | AddE
                | AddES
                | SelfPlur
                | IrregPlur

sPlur :: Sentence -> PluralType -> Sentence
sPlur s@(S _) AddS = s :+: S "s"
sPlur s@(S _) AddE = s :+: S "e"
sPlur s@(S _) AddES = sPlur (sPlur s AddE) AddS
--sPlur s@(S _) SelfPlur = s -- Is there any reason we'd want this?
sPlur (S sts) IrregPlur = getIrreg sts
sPlur (a :+: b) pt = a :+: sPlur b pt
sPlur a _ = S "MISSING PLURAL FOR:" +:+ a

addS, addE, addES, irregPlur :: Sentence -> Sentence

addS = \s -> sPlur s AddS
addE = \s -> sPlur s AddE
addES = \s -> sPlur s AddES
irregPlur = \s -> sPlur s IrregPlur
--selfPlur = id

getIrreg :: String -> Sentence
getIrreg sts = S (message $ pLook toPlural irregularPlurals)
  where toPlural = (last . words) sts
        message (Just x) = unwords $ ((init . words) sts) ++ [x]
        message (Nothing) = "MISSING PLURAL FOR:" ++ toPlural