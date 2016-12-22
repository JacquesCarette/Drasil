{-# OPTIONS -Wall #-} 
module Language.Drasil.Generate (gen, genCode) where

import System.IO
import Text.PrettyPrint.HughesPJ
import Prelude hiding (id)
import System.Directory
import Language.Drasil.Output.Formats (DocType (SRS,MG,MIS,LPM,Website))
import Language.Drasil.TeX.Print (genTeX)
import Language.Drasil.HTML.Print (genHTML)
import Language.Drasil.HTML.Helpers (makeCSS)
import Language.Drasil.Code.Import (toCode)
import Language.Drasil.Make.Print (genMake)
import Language.Drasil.Document
import Language.Drasil.Format(Format(TeX, HTML))
import Language.Drasil.Recipe(Recipe(Recipe))
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk
import Language.Drasil.Code.Imperative.LanguageRenderer
import Language.Drasil.Code.Imperative.Helpers
import Control.Lens


-- temporary
import Language.Drasil.Code.Code
import Language.Drasil.Code.CodeTest
import Language.Drasil.Code.CodeGeneration
import Language.Drasil.Code.Imperative.Parsers.ConfigParser

-- Generate a number of artifacts based on a list of recipes.
gen :: [Recipe] -> IO ()
gen rl = mapM_ prnt rl

prnt :: Recipe -> IO ()
prnt (Recipe dt@(SRS _) body) =
  do prntDoc dt body
     prntMake dt
prnt (Recipe dt@(MG _) body) =
  do prntDoc dt body
     prntMake dt
--     prntCode body
prnt (Recipe dt@(MIS _) body) =
  do prntDoc dt body
     prntMake dt
prnt (Recipe dt@(LPM _) body) =
  do prntDoc dt body
prnt (Recipe dt@(Website fn) body) =
  do prntDoc dt body
     outh2 <- openFile ("Website/" ++ fn ++ ".css") WriteMode
     hPutStrLn outh2 $ render (makeCSS body)
     hClose outh2

prntDoc :: DocType -> Document -> IO ()
prntDoc dt body = case dt of
  (SRS fn)     -> prntDoc' dt fn TeX body
  (MG fn)      -> prntDoc' dt fn TeX body
  (MIS fn)     -> prntDoc' dt fn TeX body
  (LPM fn)     -> prntDoc' dt fn TeX body
  (Website fn) -> prntDoc' dt fn HTML body
  where prntDoc' dt' fn format body' = do
          createDirectoryIfMissing False $ show dt'
          outh <- openFile (show dt' ++ "/" ++ fn ++ getExt format) WriteMode
          hPutStrLn outh $ render $ (writeDoc format dt' body')
          hClose outh
          where getExt TeX  = ".tex"
                getExt HTML = ".html"
                getExt _    = error "we can only write TeX/HTML (for now)"


prntMake :: DocType -> IO ()
prntMake dt =
  do outh <- openFile (show dt ++ "/Makefile") WriteMode
     hPutStrLn outh $ render $ genMake [dt]
     hClose outh

writeDoc :: Format -> DocType -> Document -> Doc
writeDoc TeX  = genTeX
writeDoc HTML = genHTML
writeDoc _    = error "we can only write TeX/HTML (for now)"



genCode :: NamedChunk -> [ModuleChunk] -> IO ()
genCode cc mcs = prntCode cc (getCodeModules cc mcs)
  where getCodeModules :: NamedChunk -> [ModuleChunk] -> [ModuleChunk]
        getCodeModules _ [] = []
        getCodeModules cc' ((mc@(MoC {imp = Just cc''})):mcs') =
          if cc' == cc'' then mc:getCodeModules cc' mcs' else getCodeModules cc' mcs'
        getCodeModules cc' (mc:mcs') = getCodeModules cc' mcs'


-- generate code for all supported languages (will add language selection later)
prntCode :: NamedChunk -> [ModuleChunk] -> IO ()
prntCode cc mcs = let absCode = toCode cc mcs
                      code l  = makeCode l
                        (Options Nothing Nothing Nothing (Just "Code"))
                        (map (\mc ->
                          makeClassNameValid $ (modcc mc) ^. id) mcs)
                        absCode
                      writeCode c lang = do
                        let newDir = c ++ "/" ++ lang
                        createDirectoryIfMissing False newDir
                        setCurrentDirectory newDir
                        createCodeFiles $ code lang

                  in  do
                      workingDir <- getCurrentDirectory
                      let writeCode' = writeCode workingDir
                      writeCode' cppLabel
                      writeCode' javaLabel
                      writeCode' luaLabel
                      writeCode' cSharpLabel
                      writeCode' goolLabel
                      writeCode' objectiveCLabel
                      writeCode' pythonLabel
                      setCurrentDirectory workingDir