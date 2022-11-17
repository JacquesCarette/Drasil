module Language.Drasil.TypeCheck where

import qualified Data.Map.Strict as M

import Language.Drasil
import Database.Drasil (symbolTable)
import Data.Either (isRight)
import Control.Lens ((^.))
import Data.Bifunctor (second)
import Data.List (partition)
import SysInfo.Drasil (SystemInformation(SI))

-- FIXME: I don't quite like this placement. I like the idea of it being done on
-- the entire system at once, it makes debugging (right now) easily, but it
-- should be closer to individual instances in the future.
typeCheckSI :: SystemInformation -> IO ()
typeCheckSI
  (SI _ _ _ _ _ _ ims _ _ _ _ _ _ _ chks _ _)
  = do
    putStrLn "[ Start type checking ]"

    -- build a variable context (a map of UIDs to "Space"s [types])
    let cxt = M.map (\(dict, _) -> dict ^. typ) (symbolTable chks)

    -- dump out the list of variables
    print $ M.toList cxt

    -- grab all of the IMs and their type-check-able expressions
    let toChk = map (\im -> (im ^. uid, typeCheckExpr im :: [(Expr, Space)])) ims

    let (notChkd, chkd) = partition (\(_, exsps) -> null exsps) toChk

    mapM_ (\(im, _) -> putStrLn $ "WARNING: `" ++ show im ++ "` does not expose any expressions to type check.") notChkd

    -- type check them
    let chkdd = map (second (map (uncurry (check cxt)))) chkd

    -- format 'ok' messages and 'type error' messages, as applicable
    let formattedChkd :: [Either [Char] ([Char], [Either Space TypeError])]
        formattedChkd = map 
                          (\(im, tcs) -> if any isRight tcs
                            then Right ("`" ++ show im ++ "` exposes ill-typed expressions!", filter isRight tcs)
                            else Left $ "`" ++ show im ++ "` OK!") 
                          chkdd

    mapM_ (either
            putStrLn
            (\(imMsg, tcs) -> do 
              putStrLn imMsg
              mapM_ (\(Right s) -> do
                putStr "  - " -- TODO: we need to be able to dump the expression to the console so that we can identify which expression caused the issue
                putStrLn s) tcs
              )
      ) formattedChkd
    putStrLn "[ Finished type checking ]"
    -- FIXME: We want the program to "error out," but from where? Here doesn't seem right.
    -- add back import: Control.Monad (when)
    -- when (any isRight formattedChkd) $ error "Type errors occurred, please check your expressions and adjust accordingly"
