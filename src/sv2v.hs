{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO
import System.Exit

import Data.List (elemIndex)
import Job (readJob, files, exclude, incdir, define, siloed, skipPreprocessor)
import Convert (convert)
import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser (parseFiles)

splitDefine :: String -> (String, String)
splitDefine str =
    case elemIndex '=' str of
        Nothing -> (str, "")
        Just idx -> (take idx str, drop (idx + 1) str)

isInterface :: Description -> Bool
isInterface (Part _ _ Interface _ _ _ _ ) = True
isInterface _ = False

isPackage :: Description -> Bool
isPackage Package{} = True
isPackage _ = False

emptyWarnings :: [AST] -> [AST] -> IO ()
emptyWarnings before after =
    if all null before || any (not . null) after then
        return ()
    else if any (any isInterface) before then
        hPutStr stderr $ "Warning: Source includes an interface but output is "
            ++ "empty because there is no top-level module which has no ports "
            ++ "which are interfaces."
    else if any (any isPackage) before then
        hPutStr stderr $ "Warning: Source includes packages but no modules. "
            ++ "Please convert packages alongside the modules that use them."
    else
        return ()

main :: IO ()
main = do
    job <- readJob
    -- parse the input files
    let defines = map splitDefine $ define job
    result <- parseFiles (incdir job) defines (siloed job)
        (skipPreprocessor job) (files job)
    case result of
        Left msg -> do
            hPutStr stderr $ msg ++ "\n"
            exitFailure
        Right asts -> do
            -- convert the files
            let asts' = convert (exclude job) asts
            emptyWarnings asts asts'
            -- print the converted files out
            hPrint stdout $ concat asts'
            exitSuccess
