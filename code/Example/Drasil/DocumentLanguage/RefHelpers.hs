{-# Language GADTs #-}

module Drasil.DocumentLanguage.RefHelpers 
  ( refA
  )where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (assumption)

import Control.Lens ((^.))
import Prelude hiding (id)

refA :: HasAssumpRefs db => db -> AssumpChunk -> Sentence
refA adb a = Ref (rType (Assumption a)) (refAdd a) (short assumption :+:
  S ((show $ snd $ assumpLookup a (adb ^. assumpRefTable))))
