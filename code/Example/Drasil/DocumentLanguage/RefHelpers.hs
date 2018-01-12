{-# Language GADTs #-}

module Drasil.DocumentLanguage.RefHelpers 
  ( refA
  )where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (assumption)

import Control.Lens ((^.))
import Prelude hiding (id)

refA :: HasAssumpRefs db => db -> AssumpChunk -> Sentence
refA adb a = Ref (rType (Assumption a)) (short assumption :+:
  S (":A" ++ (show $ snd $ assumpLookup a (adb ^. assumpRefTable))))
  --FIXME: Currently using the A:A format to conform with "refName" which needs
  -- to be replaced before this can be fixed.

