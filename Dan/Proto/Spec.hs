{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Spec where

import Unicode (Render)
import Symbol

data Accent = Grave | Acute deriving Eq

-- For writing "sentences" via combining smaller elements
infixr 5 :+:
data Sentence where
  N     :: Symbol -> Sentence
  Sy    :: USymb -> Sentence
  S     :: String -> Sentence       -- Strings, used for Descriptions in Chunks
    -- Concatenation of two Specs (e.g. delta :+: T -> deltaT)
  (:+:) :: Sentence -> Sentence -> Sentence     
  U     :: (Render r) => r -> Sentence  -- special characters
  F     :: Accent -> Char -> Sentence  -- Special formatting for certain special
                                       -- chars
  Ref   :: RefType -> Sentence -> Sentence  -- Needs helper func to create Ref
                                    -- See Reference.hs

--Moving this here to avoid cyclic imports
data USymb = UName Symbol
           | UProd [USymb]
           | UPow USymb Integer --can be negative, should not be 0
           | UDiv USymb USymb   --Get proper division (not neg pow)
                                --  necessary for things like J/(kg*C)
-- Language of unit equations, to define a unit relative
-- to another

data RefType = Tab
             | Fig
             | Sec
             | Def

instance Show RefType where
  show Tab = "Table"
  show Fig = "Figure"
  show Sec = "Section"
  show Def = "Definition"
  
-- this is a horrible hack that assumes too much from sentences!
sMap :: (String->String) -> Sentence -> Sentence
sMap f (S a) = S (f a)
sMap f (a :+: b) = sMap f a :+: sMap f b
sMap _ a = a

