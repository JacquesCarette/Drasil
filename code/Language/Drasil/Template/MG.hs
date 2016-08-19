module Language.Drasil.Template.MG(makeMG) where

import Language.Drasil.Document
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Other
import Language.Drasil.Chunk.Req
import Language.Drasil.Chunk.LC
import Language.Drasil.Spec
import Language.Drasil.Printing.Helpers
import Language.Drasil.Reference
import Language.Drasil.Template.Helpers

import Control.Lens ((^.))
import Data.List (nub, intersperse)
import Data.Maybe (fromJust, isNothing)


makeMG :: [LCChunk] -> [UCChunk] -> [ModuleChunk] -> ([Section], [Contents])
makeMG lccs uccs mcs = let mhier  = buildMH $ splitLevels mcs
                           mpairs = map createMPair (getMHOrder mhier)
                           hierTable = mgHierarchy $ formatMH mhier
                           s2 = mgChanges lccs uccs
                           s3 = mgModuleHierarchy mpairs hierTable
                           s4 = mgModuleDecomp mpairs
                           s5 = mgTrace lccs
                           s6 = mgUses mcs
                           secDescr = [
                             (s2, S " lists the likely and unlikely " :+:
                                  S "changes of the software requirements.") ,
                             (s3, S " summarizes the module decomposition " :+:
                                  S "that was constructed according to the " :+:
                                  S "likely changes.") ,
                             (s4, S " gives a detailed description of the " :+:
                                  S "modules.") ,
                             (s5, S " includes two traceability matrices. " :+:
                                  S "One checks the completeness of the " :+:
                                  S "design against the requirements " :+:
                                  S "provided in the SRS. The other shows " :+:
                                  S "the relation between anticipated " :+:
                                  S "changes and the modules.") ,
                             (s6, S " describes the use relation between " :+:
                                  S "modules.")
                             ]
                           docDescr = docOutline secDescr
  in ( [ mgIntro docDescr,
         s2,
         s3,
         s4,
         s5,
         s6 ],
       getMods mpairs )

mgIntro :: Contents -> Section
mgIntro docDesc =
  Section 0 (S "Introduction") (
    [ Con $ Paragraph $
        S "Decomposing a system into modules is a commonly accepted " :+:
        S "approach to developing software.  A module is a work assignment " :+:
        S "for a programmer or programming team. In the best " :+:
        S "practices for scientific computing, Wilson et al advise a " :+:
        S "modular design, but are silent on the criteria to use to " :+:
        S "decompose the software into modules.  We advocate a " :+:
        S "decomposition based on the principle of information hiding. " :+:
        S "This principle supports design for change, because the " :+:
        Quote (S "secrets") :+:
        S " that each module hides represent likely future " :+:
        S "changes.  Design for change is valuable in SC, where " :+:
        S "modifications are frequent, especially during initial " :+:
        S "development as the solution space is explored." ,
      Con $ Paragraph $
        S "Our design follows the rules laid out by Parnas, as follows:" ,
      Con $ Enumeration $ Bullet $
        [ Flat $ S "System details that are likely to change independently " :+:
                 S "should be the secrets of separate modules." ,
          Flat $ S "Any other program that requires information stored in " :+:
                 S "a module's data structures must obtain it by calling " :+:
                 S "access programs belonging to that module."
        ] ,
      Con $ Paragraph $
        S "After completing the first stage of the design, the Software " :+:
        S "Requirements Specification (SRS), the Module Guide (MG) is " :+:
        S "developed. The MG specifies the modular structure of the " :+:
        S "system and is intended to allow both designers and maintainers " :+:
        S "to easily identify the parts of the software.  The potential " :+:
        S "readers of this document are as follows:" ,
      Con $ Enumeration $ Bullet $
        [ Flat $ S "New project members: This document can be a guide for " :+:
                 S "a new project member to easily understand the overall " :+:
                 S "structure and quickly find the relevant modules they " :+:
                 S "are searching for." ,
          Flat $ S "Maintainers: The hierarchical structure of the module " :+:
                 S "guide improves the maintainers' understanding when " :+:
                 S "they need to make changes to the system. It is " :+:
                 S "important for a maintainer to update the relevant " :+:
                 S "sections of the document after changes have been made." ,
          Flat $ S "Designers: Once the module guide has been written, it " :+:
                 S "can be used to check for consistency, feasibility and " :+:
                 S "flexibility. Designers can verify the system in " :+:
                 S "various ways, such as consistency among modules, " :+:
                 S "feasibility of the decomposition, and flexibility of " :+:
                 S "the design."
        ] ,
        Con docDesc
    ]
  )

mgChanges :: [LCChunk] -> [UCChunk] -> Section
mgChanges lccs uccs = let secLikely = mgLikelyChanges lccs
                          secUnlikely = mgUnlikelyChanges uccs
  in
    Section 0 (S "Likely and Unlikely Changes") (
      [ Con $ Paragraph $
          S "This section lists possible changes to the system. According " :+:
          S "to the likeliness of the change, the possible changes are " :+:
          S "classified into two categories. Likely changes are listed in " :+:
          (makeRef secLikely) :+: S " and unlikely changes are " :+:
          S "listed in " :+: (makeRef secUnlikely) ,
        Sub secLikely,
        Sub secUnlikely
      ]
    )

mgLikelyChanges :: [LCChunk] -> Section
mgLikelyChanges lccs =
  Section 1 (S "Likely Changes") (
    [ Con mgLikelyChangesIntro ]
    ++ map (Con . LikelyChange) lccs
  )

mgUnlikelyChanges :: [UCChunk] -> Section
mgUnlikelyChanges uccs =
  Section 1 (S "Unikely Changes") (
    [ Con mgUnlikelyChangesIntro ]
    ++ map (Con . UnlikelyChange) uccs
  )

mgLikelyChangesIntro :: Contents
mgLikelyChangesIntro = Paragraph $
  S "Likely changes are the source of the information that " :+:
  S "is to be hidden inside the modules. Ideally, changing one of the " :+:
  S "likely changes will only require changing the one module that " :+:
  S "hides the associated decision. The approach adapted here is called " :+:
  S "design for change."

mgUnlikelyChangesIntro :: Contents
mgUnlikelyChangesIntro = Paragraph $
  S "The module design should be as general as possible. However, a " :+:
  S "general system is more complex. Sometimes this complexity is not " :+:
  S "necessary. Fixing some design decisions at the system architecture " :+:
  S "stage can simplify the software design. If these decision should " :+:
  S "later need to be changed, then many parts of the design will " :+:
  S "potentially need to be modified. Hence, it is not intended that " :+:
  S "these decisions will be changed.  As an example, the model is assumed " :+:
  S "to follow the definition in the SRS.  If a new model is used, this " :+:
  S "will mean a change to the input format, fit parameters module, " :+:
  S "control, and output format modules."

mgModuleHierarchy :: [MPair] -> Contents -> Section
mgModuleHierarchy mpairs hierTable =
  Section 0 (S "Module Hierarchy") (
    [ Con $ mgModuleHierarchyIntro hierTable ]
    ++ (map Con $ getMods mpairs)
    ++ [Con hierTable]
  )

mgModuleHierarchyIntro :: Contents -> Contents
mgModuleHierarchyIntro t@(Table _ _ _ _) = Paragraph $
  S "This section provides an overview of the module design. Modules are " :+:
  S "summarized in a hierarchy decomposed by secrets in " :+:
  makeRef t :+:
  S ". The modules listed below, which are leaves in the hierarchy tree, " :+:
  S "are the modules that will actually be implemented."


mgHierarchy :: [[Sentence]] -> Contents
mgHierarchy mh = let cnt = length $ head mh
                     hdr = map (\x -> S $ "Level " ++ show x) $ take cnt [1..]
                 in  Table hdr mh (S "Module Hierarchy") True

mgModuleDecomp :: [MPair] -> Section
mgModuleDecomp mpairs = let levels = splitLevels $ getChunks mpairs
  in Section 0 (S "Module Decomposition") (
       [Con $ mgModuleDecompIntro $ getChunks mpairs]
       ++ map (\x -> Sub (mgModuleInfo x levels)) mpairs
     )

mgModuleDecompIntro :: [ModuleChunk] -> Contents
mgModuleDecompIntro mcs =
  let impl ccs = foldl1 (:+:) $ map (\x -> (S $ "If the entry is " ++
       (x ^. name) ++ ", this means that the module is provided by the ")
       :+: (x ^. descr) :+: S ". ") ccs
  in Paragraph $
    S "Modules are decomposed according to the principle of " :+:
    Quote (S "information hiding") :+:
    S " proposed by Parnas. The Secrets field in a module " :+:
    S "decomposition is a brief statement of the design decision hidden by " :+:
    S "the module. The Services field specifies what the module will do " :+:
    S "without documenting how to do it. For each module, a suggestion for " :+:
    S "the implementing software is given under the Implemented By title. " :+:
    impl (nub $ getImps mcs) :+:
    S "Only the leaf modules in the hierarchy have to be implemented. If a " :+:
    S "dash (--) is shown, this means that the module is not a leaf and " :+:
    S "will not have to be implemented. Whether or not this module is " :+:
    S "implemented depends on the programming language selected."
      where
        getImps []     = []
        getImps (m:ms) = if imp m == Nothing
                         then getImps ms
                         else (fromJust $ imp m):getImps ms

mgModuleInfo :: MPair -> [[ModuleChunk]] -> Section
mgModuleInfo (mc, m) ls = let title = if   isNothing m
                                   then S (formatName mc)
                                   else S (formatName mc) :+: S " (" :+:
                                          (makeRef $ fromJust m) :+: S ")"
                              level = getLevel mc ls
  in Section (1 + level)
    title
    [ Con $ Enumeration $ Desc
      [(S "Secrets", Flat (secret mc)),
       (S "Services", Flat (mc ^. descr)),
       (S "Implemented By", Flat (getImp $ imp mc))
      ]
    ]
    where
      getImp (Just x) = S (x ^. name)
      getImp _        = S "--"


mgTrace :: [LCChunk] -> Section
mgTrace lccs = let lct = mgTraceLC lccs
  in Section 0 ( S "Traceability Matrix") (
       [ Con $ Paragraph $
           S "This section shows two traceability matrices: between the " :+:
           S "modules and the requirements in Table **todo** and between " :+:
           S "the modules and the likely changes in " :+: makeRef lct :+: S ".",
         Con $ lct ]
     )

mgTraceLC :: [LCChunk] -> Contents
mgTraceLC lccs = Table [S "Likely Change", S "Modules"]
  (map mgLCTraceEntry lccs) (S "Trace Between Likely Changes and Modules") True

mgLCTraceEntry :: LCChunk -> [Sentence]
mgLCTraceEntry lcc = [ makeRef (LikelyChange lcc),
                       mgListModules (lcRelatedModules lcc)
                     ]

mgListModules :: [ModuleChunk] -> Sentence
mgListModules mcs = foldl (:+:) (S "") $ intersperse (S ", ") $
  map (\x -> makeRef $ Module x) mcs

mgUses :: [ModuleChunk] -> Section
mgUses mcs = let uh = mgUH mcs
  in Section 0 ( S "Uses Hierarchy" ) (
       [ Con $ Paragraph $
           S "In this section, the uses hierarchy between modules is " :+:
           S "provided. Parnas said of two programs A and B that A uses B if " :+:
           S "correct execution of B may be necessary for A to complete " :+:
           S "the task described in its specification. That is, A uses B if " :+:
           S "there exist situations in which the correct functioning of A " :+:
           S "depends upon the availability of a correct implementation of B. " :+:
           makeRef uh :+:
           S " illustrates the uses hierarchy between the " :+:
           S "modules. The graph is a directed acyclic graph (DAG). Each " :+:
           S "level of the hierarchy offers a testable and usable subset of " :+:
           S "the system, and modules in the higher level of the hierarchy " :+:
           S "are essentially simpler because they use modules from the lower " :+:
           S "levels.",
         Con uh
       ]
     )

mgUH :: [ModuleChunk] -> Contents
mgUH mcs = UsesHierarchy $ makePairs mcs
  where makePairs []      = []
        makePairs (m:mcs) = if (null $ uses m)
                            then makePairs mcs
                            else (m, uses m):makePairs mcs