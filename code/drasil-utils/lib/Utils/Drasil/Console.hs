module Utils.Drasil.Console (
    -- * Colourized Console Messages
    succMsg, warnMsg, errMsg
) where

succMsg :: String -> String
succMsg msg = "\ESC[32m" ++ msg ++ "\ESC[0m"

warnMsg :: String -> String
warnMsg msg = "\ESC[33m" ++ msg ++ "\ESC[0m"

errMsg :: String -> String
errMsg msg = "\ESC[91m" ++ msg ++ "\ESC[0m"
