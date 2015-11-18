{-# OPTIONS -Wall #-} 
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Chunk where

import Control.Lens
import Unit using (Unit(..))

--How to design the chunks? --

class Chunk c where
   name :: Simple Lens c String
   descr :: Simple Lens c String
   symbol :: Simple Lens c String

{-
class Chunk c => EqChunk c mode where
  equat :: Simple Lens c (AST.Expr mode)
  siu :: Simple Lens c (AST.Unit mode)
  dependencies :: Simple Lens c [c]
-}

data VarChunk = VC { vname :: String
                   , vdesc :: String
                   , vsymb :: String}

instance Eq VarChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)

data UnitalChunk = UC { ch :: VarChunk
                      , usiu :: Unit }
{-
data FullChunk mode = FC { cname :: String
                         , cdesc :: String
                         , csymb :: String
                         , cequat :: Expr mode
                         , csiu :: Unit mode
                         , cdep :: [c] }
-}

instance Chunk VarChunk where
  name f (VC n d s) = fmap (\x -> VC x d s) (f n)
  descr f (VC n d s) = fmap (\x -> VC n x s) (f d)
  symbol f (VC n d s) = fmap (\x -> VC n d x) (f s)

{-
instance Chunk (FullChunk mode) mode where
  name   f c = fmap (\x -> c {cname = x}) (f $ cname c)
  descr  f c = fmap (\x -> c {cdesc = x}) (f $ cdesc c)
  symbol f c = fmap (\x -> c {csymb = x}) (f $ csymb c)
-}

-- instance EqChunk (FullChunk mode) mode where
{-
newChunk :: String -> [(AST.FName, AST.FDesc a)] -> AST.Chunk a
newChunk nm l = AST.Chunk nm (Map.fromList l)

find :: AST.FName -> AST.Chunk a -> String -> AST.FDesc a
find f@AST.Equation (AST.Chunk _ chunk) errmsg = 
  fromMaybe (fromMaybe (error errmsg) (Map.lookup AST.Symbol chunk)) (Map.lookup f chunk)
find f (AST.Chunk _ chunk) errmsg = 
  fromMaybe (error errmsg) (Map.lookup f chunk)

findOptional :: AST.FName -> AST.Chunk a -> Maybe (AST.FDesc a)
findOptional f (AST.Chunk _ m) = Map.lookup f m
-}
