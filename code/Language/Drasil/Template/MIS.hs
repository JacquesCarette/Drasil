module Language.Drasil.Template.MIS (makeMIS) where

import Language.Drasil.Document
import Language.Drasil.Spec
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Method
import Language.Drasil.Template.Helpers

import Data.List (intersperse, concat)
import Control.Lens ((^.))

makeMIS :: [ModuleChunk] -> [Section]
makeMIS mcs = misIntro:map misModule mcs

misIntro :: Section
misIntro = Section (S $ "Introduction")
  [ Con $ Paragraph $
    S "The following document details the Module Interface Specifications " :+:
    S "for the implemented modules in a program that calculates . " :+:
    S "It is intended to ease navigation through the program for design " :+:
    S "and maintenance purposes.  Complementary documents include the " :+:
    S "System Requirement Specifications and Module Guide."
  ]

misModule :: ModuleChunk -> Section
misModule mc = Section (S $ "MIS for " ++ formatName mc)
  [ Sub $ misInterfaceSyntax mc,
    Sub $ misInterfaceSemantics mc ]

misInterfaceSyntax :: ModuleChunk -> Section
misInterfaceSyntax mc = Section (S $ "Interface Syntax")
  [ Sub $ Section (S $ "Exported Access Programs")
    [ Con $ misExportedAP mc ]
  ]

-- var types not implemented yet so just map to "real" for now
misExportedAP :: ModuleChunk -> Contents
misExportedAP mc = Table [S "Name", S "In", S "Out", S "Exceptions"]
  ( map ( \x -> (convertName (x ^. name)) :
                (S $ concat $ intersperse ", " $ map (\_ -> "real") (input x)) :
                (S $ concat $ intersperse ", " $ map (\_ -> "real") (output x)) :
                (S $ concat $ intersperse ", " $ map show (exc x)) :
                [] ) (method mc)
  ) (S $ "Access programs exported by the " ++ formatName mc) False


misInterfaceSemantics :: ModuleChunk -> Section
misInterfaceSemantics mc = Section (S $ "Interface Semantics")
  [ Sub $ Section (S $ "Access Program Semantics")
    (map (Sub . misAPSemantics) (method mc))
  ]

misAPSemantics :: MethodChunk -> Section
misAPSemantics mec = Section (convertName (mec ^. name))
  [ Con $ Enumeration $ Desc
    [(S "Input", Flat $ foldl1 (:+:) $
        intersperse (S ", ") $ map (\x -> P $ x ^. symbol) (input mec)),
     (S "Exceptions", Flat $ foldl1 (:+:) $
        intersperse (S ", ") $ map (S . show) (exc mec)),
     (S "Output", Flat $ foldl1 (:+:) $
             intersperse (S ", ") $ map (\x -> P $ x ^. symbol) (output mec))
    ]
  ]