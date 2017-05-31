module Language.Drasil.Chunk.CommonIdea
  ( CommonIdea(..) --, commonidea, CI
  , CINP, commonINP, commonINP' , commonINP''
  ) where

import Prelude hiding (id)

import Language.Drasil.Chunk (Chunk(id))
import Language.Drasil.Chunk.NamedIdea
import Control.Lens (Simple, Lens)
import Language.Drasil.Spec (Sentence(S,P))
import Language.Drasil.NounPhrase
import Language.Drasil.Symbol (Symbol)

-- | CommonIdea is a chunk that is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: Simple Lens c Sentence

--FIXME: Change CINP to CI and remove Sentence (term).

-- | The common idea (with nounPhrase) data type. It must have a 
-- 'NounPhrase' for its 'term'.
data CINP = CINP String Sentence NP 
-- ^ The first Sentence here is now deprecated

instance Chunk CINP where
  id f (CINP a b c) = fmap (\x -> CINP x b c) (f a)
instance NamedIdea CINP where
  term f (CINP a b c) = fmap (\x -> CINP a b x) (f c)
  getA (CINP _ b _) = Just b
instance CommonIdea CINP where
  abrv f (CINP a b c) = fmap (\x -> CINP a x c) (f b)
instance NounPhrase CINP where
  phrase       (CINP _ _ c) = phrase c
  plural       (CINP _ _ c) = plural c
  sentenceCase (CINP _ _ c) = sentenceCase c
  titleCase    (CINP _ _ c) = titleCase c
  
-- | The commonINP smart constructor requires a chunk id, 
-- term (of type 'NP'), and abbreviation (as a string)
commonINP :: String -> NP -> String -> CINP
commonINP i t a = CINP i (S a) t

-- | Similar to commonINP, except the abbreviation is a Symbol
commonINP' :: String -> NP -> Symbol -> CINP
commonINP' i t sy = CINP i (P sy) t 

-- | Similar to commonINP, except the abbreviation can be anything of type 'Sentence'
-- ('Symbol', 'String', 'Greek', etc.)
commonINP'' :: String -> NP -> Sentence -> CINP
commonINP'' i t ab = CINP i ab t
