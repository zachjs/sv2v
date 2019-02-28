{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO
import System.Exit

import Job (readJob, file, target)
import Convert (convert)
import Language.SystemVerilog.Parser

main :: IO ()
main = do
    job <- readJob
    -- parse the input file
    let filePath = file job
    content <- readFile filePath
    let ast = parseFile [] filePath content
    -- convert the file
    let ast' = convert (target job) ast
    -- print the converted file out
    hPrint stdout ast'
    exitSuccess
