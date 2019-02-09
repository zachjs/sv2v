{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO
import System.Exit
import System.Environment

import Language.SystemVerilog.Parser

main :: IO ()
main = do
    [filePath] <- getArgs
    content <- readFile filePath
    let ast = parseFile [] filePath content
    let res = Left ast
    case res of
        Left  err -> do
            hPrint stderr err
            exitSuccess
            --exitFailure
        Right _ -> do
            exitSuccess
