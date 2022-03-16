{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO (hPrint, hPutStrLn, stderr, stdout)
import System.Exit (exitFailure, exitSuccess)

import Control.Monad (when, zipWithM_)
import Control.Monad.Except (runExceptT)

import Convert (convert)
import Job (readJob, Job(..), Write(..))
import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser (initialEnv, parseFiles, Config(..))

isInterface :: Description -> Bool
isInterface (Part _ _ Interface _ _ _ _ ) = True
isInterface _ = False

isPackage :: Description -> Bool
isPackage Package{} = True
isPackage _ = False

isComment :: Description -> Bool
isComment (PackageItem (Decl CommentDecl{})) = True
isComment _ = False

emptyWarnings :: AST -> AST -> IO ()
emptyWarnings before after =
    if all isComment before || not (all isComment after) then
        return ()
    else if any isInterface before then
        hPutStrLn stderr $ "Warning: Source includes an interface but output is"
            ++ " empty because there is no top-level module which has no ports"
            ++ " which are interfaces."
    else if any isPackage before then
        hPutStrLn stderr $ "Warning: Source includes packages but no modules."
            ++ " Please convert packages alongside the modules that use them."
    else
        return ()

rewritePath :: FilePath -> IO FilePath
rewritePath path = do
    when (end /= ext) $ do
        hPutStrLn stderr $ "Refusing to write adjacent to " ++ show path
            ++ " because that path does not end in " ++ show ext
        exitFailure
    return $ base ++ ".v"
    where
        ext = ".sv"
        (base, end) = splitAt (length path - length ext) path

writeOutput :: Write -> [FilePath] -> [AST] -> IO ()
writeOutput _ [] [] =
    hPutStrLn stderr "Warning: No input files specified (try `sv2v --help`)"
writeOutput Stdout _ asts =
    hPrint stdout $ concat asts
writeOutput (File f) _ asts =
    writeFile f $ show $ concat asts
writeOutput Adjacent inPaths asts = do
    outPaths <- mapM rewritePath inPaths
    let results = map (++ "\n") $ map show asts
    zipWithM_ writeFile outPaths results

main :: IO ()
main = do
    job <- readJob
    -- parse the input files
    let config = Config
            { cfEnv              = initialEnv (define job)
            , cfIncludePaths     = incdir job
            , cfSiloed           = siloed job
            , cfSkipPreprocessor = skipPreprocessor job
            , cfOversizedNumbers = oversizedNumbers job
            }
    result <- runExceptT $ parseFiles config (files job)
    case result of
        Left msg -> do
            hPutStrLn stderr msg
            exitFailure
        Right asts -> do
            -- convert the files if requested
            asts' <- if passThrough job
                        then return asts
                        else convert (dumpPrefix job) (exclude job) asts
            emptyWarnings (concat asts) (concat asts')
            -- write the converted files out
            writeOutput (write job) (files job) asts'
            exitSuccess
