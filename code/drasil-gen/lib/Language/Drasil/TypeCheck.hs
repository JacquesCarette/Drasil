{-# LANGUAGE FlexibleContexts #-}
module Language.Drasil.TypeCheck where

import qualified Data.Map.Strict as M

import Language.Drasil
import Database.Drasil (symbolTable)
import Data.Either (isRight)
import Control.Lens ((^.))
import Data.Bifunctor (second)
import Data.List (partition)
import SysInfo.Drasil (SystemInformation(SI))

typeCheckSI :: SystemInformation -> IO ()
typeCheckSI
  (SI _ _ _ _ _ _ ims dds _ _ _ _ _ _ chks _ _)
  = do
    -- build a variable context (a map of UIDs to "Space"s [types])
    let cxt = M.map (\(dict, _) -> dict ^. typ) (symbolTable chks)

    -- dump out the list of variables
    putStr "Symbol Table: "
    print $ M.toList cxt

    putStrLn "=====[ Start type checking ]====="
    let
      exprSpaceTups :: (HasUID t, RequiresChecking t Expr Space) => [t] -> [(UID, [(Expr, Space)])] 
      exprSpaceTups = map (\t -> (t ^. uid, requiredChecks t))

    -- grab all type-check-able expressions (w.r.t. Space) from DDs and IMs
    let toChk = exprSpaceTups ims ++ exprSpaceTups dds

    -- split up theories by "ones that contain things to type check" vs "not",
    -- but in reverse
    let (notChkd, chkd) = partition (\(_, exsps) -> null exsps) toChk

    -- note that some theories didn't expose anything to type-check
    mapM_ 
      (\(t, _) -> putStrLn $ "WARNING: `" ++ show t ++ "` does not expose any expressions to type check.")
      notChkd

    -- type check them
    let chkdd = map (second (map (uncurry (check cxt)))) chkd

    -- format 'ok' messages and 'type error' messages, as applicable
    let formattedChkd :: [Either [Char] ([Char], [Either Space TypeError])]
        formattedChkd = map 
                          (\(t, tcs) -> if any isRight tcs
                            then Right ("`" ++ show t ++ "` exposes ill-typed expressions!", filter isRight tcs)
                            else Left $ "`" ++ show t ++ "` OK!") 
                          chkdd

    mapM_ (either
            putStrLn
            (\(tMsg, tcs) -> do
              putStrLn tMsg
              mapM_ (\(Right s) -> do
                putStr "  - ERROR: "
                putStrLn $ temporaryIndent "  " s) tcs
              )
      ) formattedChkd
    putStrLn "=====[ Finished type checking ]====="

    -- TODO: When we want to have Drasil panic on type-errors, use the following code:
    -- add back import: Control.Monad (when)
    -- when (any isRight formattedChkd) $ error "Type errors occurred, please check your expressions and adjust accordingly"
