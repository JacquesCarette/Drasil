{-# OPTIONS -Wall #-} 
module HTMLHelpers where

import Text.PrettyPrint
import Data.Char

html = wrap "html"
head_tag = wrap "head"
body = wrap "body"
title = wrap "title"

wrap s = \x -> 
  vcat [tb s, x, tb $ "/"++s]
  where tb c = text $ "<" ++ c ++ ">"
  
sub = \x -> "<sub>" ++ x ++ "</sub>"