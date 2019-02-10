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
    let res = Right ast
    case res of
        Left  _ -> do
            --hPrint stderr err
            exitFailure
        Right str -> do
            hPrint stdout str
            exitSuccess
