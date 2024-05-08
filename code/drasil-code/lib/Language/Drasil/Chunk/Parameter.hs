{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Parameter (
  PassBy(..), Param(..), pcAuto, pcVal
) where

import Language.Drasil hiding (Ref, CodeIdea(..), CodeChunk)
import Language.Drasil.Chunk.Code (CodeIdea(..), CodeChunk)

import Control.Lens ((^.), makeLenses, view)

-- | Determines whether a parameter is passed by value or by reference.
data PassBy = Val | Ref

-- | Chunk representing a parameter.
data Param = PC {_pcc :: CodeChunk
                         , passBy :: PassBy}
makeLenses ''Param

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'Param'.
instance HasUID      Param where uid = pcc . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'Param'.
instance NamedIdea   Param where term = pcc . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'Param'.
instance Idea        Param where getA = getA . view pcc
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'Param'.
instance HasSpace    Param where typ = pcc . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'Param'.
instance HasSymbol   Param where symbol = symbol . view pcc
-- | 'Param's have a 'Quantity'.
instance Quantity    Param
-- | Finds the code name and 'CodeChunk' of a 'Param'.
instance CodeIdea    Param where
  codeName = codeName . view pcc
  codeChunk = codeChunk . view pcc
-- | Equal if 'UID's are equal.
instance Eq          Param where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'Param'.
instance MayHaveUnit Param where getUnit = getUnit . view pcc

-- | Automatically chooses 'PassBy' based on 'Space' ('Vect'ors and 'Actor's passed by reference).
pcAuto :: (CodeIdea c) => c -> Param
pcAuto c = PC cdch (choosePB $ cdch ^. typ)
  where cdch = codeChunk c
        choosePB (Vect _) = Ref
        choosePB (Actor _) = Ref
        choosePB _ = Val

-- | Constructs a pass-by-value parameter.
pcVal :: (CodeIdea c) => c -> Param
pcVal c = PC (codeChunk c) Val