-- | Defines printers for generating Makefiles.
module Drasil.Makefile.Make.Print (printMakefile) where

import Prelude hiding ((<>))
import Text.PrettyPrint (Doc, empty, text, (<>), (<+>), ($+$), ($$), hsep, vcat)
import qualified Data.Text as T
import Text.Wrap

import Drasil.Makefile.Make.AST (Annotation, Command(C),
  CommandOpts(IgnoreReturnCode), Dependencies, Makefile(M), Rule(R), Target,
  Type(Abstract))
import Drasil.Makefile.Make.Helpers (addCommonFeatures, tab)
import Drasil.Makefile.Make.MakeString (renderMS)

-- | Render a 'Makefile' to a 'Doc'.
printMakefile :: Makefile -> Doc
printMakefile (M rules) = addCommonFeatures rules $
  vcat (map (\x -> printRule x $+$ text "") rules) $$ printPhony rules

-- | Renders specific makefile rules. Called by 'build'.
printRule :: Rule -> Doc
printRule (R c t d _ cmd) = printComments c $+$ printTarget t d $+$ printCmds cmd

-- | Renders a makefile comment
printComment :: String -> Doc
printComment [] = empty
printComment c  = text $ T.unpack (wrapText wrapSettings 80 $ T.pack c) ++ "\n"

wrapSettings :: WrapSettings
wrapSettings = WrapSettings { preserveIndentation = True
                 , breakLongWords = False
                 , fillStrategy = FillPrefix (T.pack "# ")
                 , fillScope = FillAll
                 }

-- | Renders multiple comments
printComments :: Annotation -> Doc
printComments = foldr (($+$) . printComment) empty

-- | Gathers all rules to abstract targets and tags them as phony.
printPhony :: [Rule] -> Doc
printPhony = (<+>) (text ".PHONY:") . hsep . map (\(R _ t _ _ _) -> text $ renderMS t) .
  filter (\(R _ _ _ t _) -> t == Abstract)

-- | Renders targets with their dependencies.
printTarget :: Target -> Dependencies -> Doc
printTarget nameLb deps = text (renderMS nameLb ++ ":") <+> hsep (map (text . renderMS) deps)

-- | Renders a makefile command.
printCmd :: Command -> Doc
printCmd (C c opts) = text $ (if IgnoreReturnCode `elem` opts then "-" else "") ++ renderMS c

-- | Renders multiple commands.
printCmds :: [Command] -> Doc
printCmds = foldr (($+$) . (<>) tab . printCmd) empty
