{-# OPTIONS -Wall #-} 
module Gen_MK2 where
import System.IO
import Text.PrettyPrint.HughesPJ
import PrintTeX_MK2 (genTeX)
import ASTInternal_MK2 (Document, OutFormat (TeX, Plain), DocType (SRS,LPM,Code))

data Recipe = Recipe (DocType, String, Doc)
        --DocType, Filename, 'Body'

gen :: [Recipe] -> IO ()
gen ((Recipe (x,y,z)):[]) = do prnt x y z
gen ((Recipe (x,y,z)):xs) = do prnt x y z
                               gen xs
gen ([])                  = return ()

prnt :: DocType -> String -> Doc -> IO ()  
prnt SRS filename body = do outh <- openFile filename WriteMode
                            hPutStrLn outh $ render $ body
                            hClose outh
prnt LPM filename body = do outh <- openFile filename WriteMode
                            hPutStrLn outh $ render $ body
                            hClose outh
  -- No difference b/w SRS and LPM as yet, but there may be in the future so
  -- keeping them separate for the time being is a good idea
prnt Code _ _ = error "Code DocType is not implemented yet"

writeDoc :: OutFormat -> DocType -> Document -> Doc
writeDoc TeX    = genTeX
writeDoc Plain  = error "Not yet implemented"--genPlain