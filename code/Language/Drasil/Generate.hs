module Language.Drasil.Generate (gen, genCode) where

import System.IO
import Text.PrettyPrint.HughesPJ
import Prelude hiding (id)
import System.Directory
import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,Website),DocSpec(DocSpec),Filename)
import Language.Drasil.TeX.Print (genTeX)
import Language.Drasil.HTML.Print (genHTML)
import Language.Drasil.HTML.Helpers (makeCSS)
import Language.Drasil.Make.Print (genMake)
import Language.Drasil.Document
import Language.Drasil.Format(Format(TeX, HTML))
import Language.Drasil.Recipe(Recipe(Recipe))
import Language.Drasil.Code.Imperative.Import (generator, generateCode)
import Language.Drasil.CodeSpec
import Language.Drasil.ChunkDB (HasSymbolTable(..))


-- | Generate a number of artifacts based on a list of recipes.
gen :: HasSymbolTable s => [Recipe] -> s -> IO ()
gen rl sm = mapM_ (flip prnt sm) rl

-- | Generate the output artifacts (TeX+Makefile or HTML)
prnt :: HasSymbolTable s => Recipe -> s -> IO ()
prnt (Recipe dt@(DocSpec Website fn) body) sm =
  do prntDoc dt body sm
     outh2 <- openFile ("Website/" ++ fn ++ ".css") WriteMode
     hPutStrLn outh2 $ render (makeCSS body)
     hClose outh2
prnt (Recipe dt@(DocSpec _ _) body) sm =
  do prntDoc dt body sm
     prntMake dt

-- | Helper for writing the documents (TeX / HTML) to file
prntDoc :: HasSymbolTable s => DocSpec -> Document -> s -> IO ()
prntDoc dt body sm = case dt of
  (DocSpec SRS _)     -> prntDoc' dt TeX body
  (DocSpec MG _)      -> prntDoc' dt TeX body
  (DocSpec MIS _)     -> prntDoc' dt TeX body
  (DocSpec Website _) -> prntDoc' dt HTML body
  where prntDoc' (DocSpec dt' fn) format body' = do
          createDirectoryIfMissing False $ show dt'
          outh <- openFile (show dt' ++ "/" ++ fn ++ getExt format) WriteMode
          hPutStrLn outh $ render $ (writeDoc format fn body' sm)
          hClose outh
          where getExt TeX  = ".tex"
                getExt HTML = ".html"
                getExt _    = error "we can only write TeX/HTML (for now)"

-- | Helper for writing the Makefile(s)
prntMake :: DocSpec -> IO ()
prntMake ds@(DocSpec dt _) =
  do outh <- openFile (show dt ++ "/Makefile") WriteMode
     hPutStrLn outh $ render $ genMake [ds]
     hClose outh

-- | Renders the documents
writeDoc :: HasSymbolTable s => Format -> Filename -> Document -> s -> Doc
writeDoc TeX  _  doc s = genTeX doc s
writeDoc HTML fn doc s = genHTML fn doc s
writeDoc _    _  _   _ = error "we can only write TeX/HTML (for now)"

-- | Calls the code generator
genCode :: Choices -> CodeSpec -> IO ()
genCode ch spec = 
  let g = generator ch spec
  in
    do 
      workingDir <- getCurrentDirectory
      createDirectoryIfMissing False "src"
      setCurrentDirectory "src"
      generateCode ch g
      setCurrentDirectory workingDir


-- -- | Calls the code generator using the 'ModuleChunk's
-- genCode :: NamedIdea c => c -> [ModuleChunk] -> s -> IO ()
-- genCode cc mcs m = prntCode cc (filter generated mcs) m

-- -- | Generate code for all supported languages (will add language selection later)
-- prntCode :: NamedIdea c => c -> [ModuleChunk] -> s -> IO ()
-- prntCode cc mcs m = 
  -- let absCode = toCode cc mcs m
      -- code l  = makeCode l
        -- (Options Nothing Nothing Nothing (Just "Code"))
        -- (map (\mc -> makeClassNameValid $ (modcc mc) ^. id) mcs)
        -- absCode
      -- writeCode c lang = do
        -- let newDir = c ++ "/" ++ lang
        -- createDirectoryIfMissing False newDir
        -- setCurrentDirectory newDir
        -- createCodeFiles $ code lang

  -- in  do
      -- workingDir <- getCurrentDirectory
      -- let writeCode' = writeCode workingDir
      -- writeCode' cppLabel
 -- --     writeCode' javaLabel
 -- --     writeCode' luaLabel
 -- --     writeCode' cSharpLabel
 -- --     writeCode' goolLabel
 -- --     writeCode' objectiveCLabel
 -- --     writeCode' pythonLabel
      -- setCurrentDirectory workingDir
