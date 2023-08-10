{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Parameter (
  PassBy(..), ParameterChunk(..), pcAuto, pcVal
) where

import Language.Drasil hiding (Ref, CodeIdea(..), CodeChunk)
import Language.Drasil.Chunk.Code (CodeIdea(..), CodeChunk)

import Control.Lens ((^.), makeLenses, view)

-- | Determines whether a parameter is passed by value or by reference.
data PassBy = Val | Ref

-- | Chunk representing a parameter.
data ParameterChunk = PC {_pcc :: CodeChunk
                         , passBy :: PassBy}
makeLenses ''ParameterChunk

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'ParameterChunk'.
instance HasUID      ParameterChunk where uid = pcc . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'ParameterChunk'.
instance NamedIdea   ParameterChunk where term = pcc . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'ParameterChunk'.
instance Idea        ParameterChunk where getA = getA . view pcc
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'ParameterChunk'.
instance HasSpace    ParameterChunk where typ = pcc . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'ParameterChunk'.
instance HasSymbol   ParameterChunk where symbol = symbol . view pcc
-- | 'ParameterChunk's have a 'Quantity'.
instance Quantity    ParameterChunk
-- | Finds the code name and 'CodeChunk' of a 'ParameterChunk'.
instance CodeIdea    ParameterChunk where
  codeName = codeName . view pcc
  codeChunk = codeChunk . view pcc
-- | Equal if 'UID's are equal.
instance Eq          ParameterChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'ParameterChunk'.
instance MayHaveUnit ParameterChunk where getUnit = getUnit . view pcc

-- | Automatically chooses 'PassBy' based on 'Space' ('Vect'ors and 'Actor's passed by reference).
pcAuto :: (CodeIdea c) => c -> ParameterChunk
pcAuto c = PC cdch (choosePB $ cdch ^. typ)
  where cdch = codeChunk c
        choosePB (Vect _) = Ref
        choosePB (Actor _) = Ref
        choosePB _ = Val

-- | Constructs a pass-by-value parameter.
pcVal :: (CodeIdea c) => c -> ParameterChunk
pcVal c = PC (codeChunk c) Val