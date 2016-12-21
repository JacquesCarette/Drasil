{-# LANGUAGE GADTs #-}

-- This module is going to need a lot of thought.  Needs to be very high level
-- to capture what the code needs to do while not using any paradigm specific
-- constructs such that it can be imported to a generic AST of any paradigm

-- Holding off for now, just using imperative OO AST for now

module Language.Drasil.Code.AST where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Eq

type HighLevelCode = [Module]
type Name = String

data Module = Name [Field] [Function] Uses
type Uses = [Module]

data Function = Function Name Visibility [In] Out [Objective]
              | MainFunction [Objective]

data In where
  In :: (Quantity c) => c -> In

data Out where
  Out :: (Quantity c) => c -> Out


data Field where
  Field :: (Quantity c) => Visibility -> c -> Field

data Visibility = Public
                | Private

data Objective where
  Calculation :: EqChunk -> Objective
  Call :: Function -> [Expr] -> Objective
  GetInput :: (Quantity c) => c -> Objective
  PrintOutput :: Output -> Objective

data Output where
  Str :: String -> Output
  Var :: (Quantity c) => c -> Output