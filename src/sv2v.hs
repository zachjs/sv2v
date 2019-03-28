{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO
import System.Exit

import Job (readJob, files, exclude, incdir)
import Convert (convert)
import Language.SystemVerilog.Parser

main :: IO ()
main = do
    job <- readJob
    -- parse the input file
    let includePaths = incdir job
    asts <- mapM (parseFile includePaths []) (files job)
    let ast = concat asts
    -- convert the file
    let ast' = convert (exclude job) ast
    -- print the converted file out
    hPrint stdout ast'
    exitSuccess
