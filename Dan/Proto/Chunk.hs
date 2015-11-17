{-# OPTIONS -Wall #-} 
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Chunk where

import qualified Data.Map.Strict as Map
import qualified ASTInternal as AST
import Data.Maybe (fromMaybe)
import Control.Lens

--How to design the chunks? --

class Chunk c mode where
   name :: Simple Lens c (AST.Spec mode)
   descr :: Simple Lens c (AST.Spec mode)
   symbol :: Simple Lens c (AST.Spec mode)

class Chunk c mode => EqChunk c mode where
  equat :: Simple Lens c (AST.Spec mode)
  siu :: Simple Lens c (AST.Spec mode)
  dependencies :: Simple Lens c (AST.Spec mode)

data VarChunk mode = VC { vname :: AST.Spec mode
                        , vdesc :: AST.Spec mode
                        , vsymb :: AST.Spec mode}

data FullChunk mode = FC { cname :: AST.Spec mode
                         , cdesc :: AST.Spec mode
                         , csymb :: AST.Spec mode 
                         , cequat :: AST.Spec mode
                         , csiu :: AST.Spec mode
                         , cdep :: AST.Spec mode }

instance Chunk (VarChunk mode) mode where
instance Chunk (FullChunk mode) mode where

instance EqChunk (FullChunk mode) mode where

newChunk :: String -> [(AST.FName, AST.FDesc a)] -> AST.Chunk a
newChunk nm l = AST.Chunk nm (Map.fromList l)

find :: AST.FName -> AST.Chunk a -> String -> AST.FDesc a
find f@AST.Equation (AST.Chunk _ chunk) errmsg = 
  fromMaybe (fromMaybe (error errmsg) (Map.lookup AST.Symbol chunk)) (Map.lookup f chunk)
find f (AST.Chunk _ chunk) errmsg = 
  fromMaybe (error errmsg) (Map.lookup f chunk)

findOptional :: AST.FName -> AST.Chunk a -> Maybe (AST.FDesc a)
findOptional f (AST.Chunk _ m) = Map.lookup f m
