{-# LANGUAGE GADTs #-}
module Language.Drasil.Space where

-- FIXME: These need to be spaces and not just types, but until Steven
--  has a chance to integrate his work I think this should be left alone
data Space where
  Integer :: Space
  Rational :: Space
  Boolean :: Space
  Char :: Space
  String :: Space
  Vect :: Space -> Space