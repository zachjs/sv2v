{-# LANGUAGE DeriveDataTypeable #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO
import System.Exit

import Args (readArgs, target, file)
import Convert (convert)
import Language.SystemVerilog.Parser

main :: IO ()
main = do
    args <- readArgs
    let filePath = file args
    content <- readFile filePath
    let ast = parseFile [] filePath content
    let res = Right (convert (target args) ast)
    case res of
        Left  _ -> do
            --hPrint stderr err
            exitFailure
        Right str -> do
            hPrint stdout str
            exitSuccess
