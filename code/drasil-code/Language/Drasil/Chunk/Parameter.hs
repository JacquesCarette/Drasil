{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Parameter (
  PassBy(..), ParameterChunk(..), pcAuto, pcVal
) where

import Language.Drasil hiding (Ref)
import Language.Drasil.Chunk.Code (CodeIdea(..), CodeChunk)

import Control.Lens ((^.), makeLenses, view)

-- Determines whether a parameter is passed by value or by reference
data PassBy = Val | Ref

-- Chunk representing a parameter
data ParameterChunk = PC {_pcc :: CodeChunk
                         , passBy :: PassBy}
makeLenses ''ParameterChunk

instance HasUID      ParameterChunk where uid = pcc . uid
instance NamedIdea   ParameterChunk where term = pcc . term
instance Idea        ParameterChunk where getA = getA . view pcc
instance HasSpace    ParameterChunk where typ = pcc . typ
instance HasSymbol   ParameterChunk where symbol = symbol . view pcc
instance Quantity    ParameterChunk
instance CodeIdea    ParameterChunk where
  codeName = codeName . view pcc
  codeChunk = codeChunk . view pcc
instance Eq          ParameterChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance MayHaveUnit ParameterChunk where getUnit = getUnit . view pcc

-- | Automatically chooses PassBy based on Space (Vectors and Actors passed by reference)
pcAuto :: (CodeIdea c) => c -> ParameterChunk
pcAuto c = PC cdch (choosePB $ cdch ^. typ)
  where cdch = codeChunk c
        choosePB (Vect _) = Ref
        choosePB (Actor _) = Ref
        choosePB _ = Val

-- Constructs a pass-by-value parameter
pcVal :: (CodeIdea c) => c -> ParameterChunk
pcVal c = PC (codeChunk c) Val