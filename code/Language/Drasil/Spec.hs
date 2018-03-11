{-# LANGUAGE GADTs #-}
-- | Contains Sentences and helpers
module Language.Drasil.Spec where

import Language.Drasil.Unicode (Greek,Special,Special(CurlyBrOpen,CurlyBrClose,SqBrOpen,SqBrClose))
import Language.Drasil.Symbol
import Language.Drasil.Expr

type RefName = Sentence
type RefAdd  = String

-- | For writing accented characters
data Accent = Grave | Acute deriving Eq

-- | For writing "sentences" via combining smaller elements
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
  Ref   :: RefType -> RefAdd -> RefName -> Sentence  -- Needs helper func to create Ref
                                    -- See Reference.hs
  Quote :: Sentence -> Sentence     -- Adds quotation marks around a sentence
                                    
  -- Direct concatenation of two Specs (no implicit spaces!)
  (:+:) :: Sentence -> Sentence -> Sentence   
  EmptyS :: Sentence
  E :: Expr -> Sentence

instance Monoid Sentence where
  mempty = EmptyS
  mappend = (:+:)

--Moving this here to avoid cyclic imports
-- | Language of unit equations, to define a unit relative
-- to another
data USymb = UName Symbol
           | UProd [USymb] -- ^ Product
           | UPow USymb Integer -- ^ can be negative, should not be 0
           | UDiv USymb USymb   -- ^ Get proper division (not neg pow)
                                -- necessary for things like J/(kg*C)
  deriving (Eq, Ord)

-- | For building references. Defines the possible type of reference.
data RefType = Tab    -- ^ Table
             | Fig    -- ^ Figure
             | Sect   -- ^ Section
             | Def    -- ^ Definition (includes theoretical models)
             | Mod    -- ^ Module
             | Req    -- ^ Requirement
             | Assump -- ^ Assumption
             | LC     -- ^ Likely Change
             | UC     -- ^ Unlikely Change
             | EqnB   -- ^ Equation Block
             | Cite   -- ^ Citation
             | Goal   -- ^ Goal Statement
             | PSD    -- ^ Physical System Description
             
instance Show RefType where
  show Tab    = "Table"
  show Fig    = "Figure"
  show Sect   = "Section"
  show Mod    = "Module"
  show Def    = "Definition"
  show Req    = "Requirement"
  show Assump = "Assumption"
  show LC     = "Likely Change"
  show UC     = "Unlikely Change"
  show Cite   = "Citation"
  show Goal   = "Goal Statement"
  show PSD    = "Physical System Description"
  show EqnB   = "Equation"

-- | Helper function for wrapping sentences in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

sParenNum :: Int -> Sentence
sParenNum y = sParen (S (show y))

-- | Helper function for wrapping sentences in square brackets.
sSqBr :: Sentence -> Sentence
sSqBr x = Sp SqBrOpen :+: x :+: Sp SqBrClose

sSqBrNum :: Int -> Sentence
sSqBrNum y = sSqBr (S (show y))

sCurlyBr :: Sentence -> Sentence
sCurlyBr x = Sp CurlyBrOpen :+: x :+: Sp CurlyBrClose

-- | Helper for concatenating two sentences with a space between them.
(+:+) :: Sentence -> Sentence -> Sentence
EmptyS +:+ b = b
a +:+ EmptyS = a
a +:+ b = a :+: S " " :+: b

-- | Helper for concatenating two sentences with a comma and space between them.
sC :: Sentence -> Sentence -> Sentence
a `sC` b = a :+: S "," +:+ b

-- | Helper which concatenates two sentences using '+:+' then adds a period to
-- the end.
(+:+.) :: Sentence -> Sentence -> Sentence
a +:+. b = a +:+ b :+: S "."

-- | Helper which concatenates two sentences using ':+:' then adds a period to
-- the end.
(+.) :: Sentence -> Sentence -> Sentence
a +. b = a :+: b :+: S "."

-- | Helper which concatenates two sentences using '+:+' then adds a colon to
-- the end.
(+:) :: Sentence -> Sentence -> Sentence
a +: b = a +:+ b :+: S ":"

-- | Helper for concatenating two sentences with a semi-colon and space between them.
semiCol :: Sentence -> Sentence -> Sentence
a `semiCol` b = a :+: S ";" +:+ b

sParenDash :: Sentence -> Sentence
sParenDash = \x -> S " (" :+: x :+: S ") - "

sDash :: Sentence -> Sentence -> Sentence
y `sDash` z = y +:+ S "-" +:+ z
