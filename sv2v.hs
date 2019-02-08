{- sv2v
   Author: Zachary Snow <zach@zachjs.com>

   conversion entry point
-}

import System.IO
import System.Exit

main :: IO ()
main = do
    let res = Left "unimplemented"
    case res of
        Left  err -> do
            hPrint stderr err
            exitFailure
        Right _ -> do
            exitSuccess
