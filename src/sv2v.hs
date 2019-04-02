{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO
import System.Exit

import Data.List (elemIndex)
import Job (readJob, files, exclude, incdir, define)
import Convert (convert)
import Language.SystemVerilog.Parser

splitDefine :: String -> (String, String)
splitDefine str =
    case elemIndex '=' str of
        Nothing -> (str, "")
        Just idx -> (take idx str, drop (idx + 1) str)

main :: IO ()
main = do
    job <- readJob
    -- parse the input file
    let includePaths = incdir job
    let defines = map splitDefine $ define job
    asts <- mapM (parseFile includePaths defines) (files job)
    let ast = concat asts
    -- convert the file
    let ast' = convert (exclude job) ast
    -- print the converted file out
    hPrint stdout ast'
    exitSuccess
