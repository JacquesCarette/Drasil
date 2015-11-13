{-# OPTIONS -Wall #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
module Chunk where

import qualified Data.Map.Strict as Map
import qualified ASTInternal as AST
import Data.Maybe (fromMaybe)
import Control.Lens

--How to design the chunks? --

--varname is redundant, hopefully can be removed.
--Going to need a chunk type to make these instances of.
--Not sure what to do with it right now.
-- data Stub a = Stub { stubname :: AST.Spec a
                 -- , stubvarname :: AST.Spec a
                 -- , stubdescription :: AST.Spec a}

-- data Calc a = Calc { calcname :: AST.Spec a
                 -- , calcdescription :: AST.Spec a
                 -- , calcequation :: AST.Spec a
                 -- , calcdependencies :: AST.Spec a
                 -- , calcsiu :: AST.Spec a}

                 
-- class (Format b) => Chunk (T b) where --with T as a type constructor for Stub or Calc
  -- name :: AST.Spec b
  -- description :: AST.Spec b
  
-- instance Chunk (Stub a) where
  -- name = stubname
  -- description = stubdescription
  
-- instance Chunk (Calc a) where
  -- name = calcname
  -- description = calcdescription
  
--Keep this stuff for now until the Chunk type is solidified.

class Chunk c mode where
   name :: Simple Lens c (AST.Spec mode)
   descr :: Simple Lens c (AST.Spec mode)
   
-- class Chunk c where
   -- type SRep c :: *
   -- name :: Simple Lens c (SRep c)
   -- descr :: Simple Lens c (SRep c)

class Chunk c mode => EqChunk c mode where
  equat :: Simple Lens c (AST.Spec mode)
  symbol :: Simple Lens c (AST.Spec mode)

-- You can allow more generality by not hard-coding String, and do something like




newChunk :: String -> [(AST.FName, AST.FDesc a)] -> AST.Chunk a
newChunk nm l = AST.Chunk nm (Map.fromList l)

find :: AST.FName -> AST.Chunk a -> String -> AST.FDesc a
find f@AST.Equation (AST.Chunk _ chunk) errmsg = 
  fromMaybe (fromMaybe (error errmsg) (Map.lookup AST.Symbol chunk)) (Map.lookup f chunk)
find f (AST.Chunk _ chunk) errmsg = 
  fromMaybe (error errmsg) (Map.lookup f chunk)

findOptional :: AST.FName -> AST.Chunk a -> Maybe (AST.FDesc a)
findOptional f (AST.Chunk _ m) = Map.lookup f m
