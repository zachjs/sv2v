{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO
import System.Exit

import Data.List (elemIndex)
import Job (readJob, files, exclude, incdir, define, siloed)
import Convert (convert)
import Language.SystemVerilog.Parser (parseFiles)

splitDefine :: String -> (String, String)
splitDefine str =
    case elemIndex '=' str of
        Nothing -> (str, "")
        Just idx -> (take idx str, drop (idx + 1) str)

main :: IO ()
main = do
    job <- readJob
    -- parse the input files
    let includePaths = incdir job
    let defines = map splitDefine $ define job
    let singleton = \x -> [x]
    let toFileLists = if siloed job then map singleton else singleton
    astLists <- mapM
        (parseFiles includePaths defines)
        (toFileLists $ files job)
    let asts = concat astLists
    -- convert the files
    let asts' = convert (exclude job) asts
    -- print the converted files out
    hPrint stdout $ concat asts'
    exitSuccess
