{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - conversion entry point
 -}

import System.IO (hPrint, hPutStrLn, stderr, stdout)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (combine, splitExtension)

import Control.Monad (when, zipWithM_)
import Control.Monad.Except (runExceptT)
import Data.List (nub)

import Convert (convert)
import Job (readJob, Job(..), Write(..))
import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser (parseFiles, Config(..))

isComment :: Description -> Bool
isComment (PackageItem (Decl CommentDecl{})) = True
isComment _ = False

droppedKind :: Description -> Identifier
droppedKind description =
    case description of
        Part _ _ Interface _ _ _ _ -> "interface"
        Package{} -> "package"
        Class{}   -> "class"
        PackageItem Function{}     -> "function"
        PackageItem Task    {}     -> "task"
        PackageItem (Decl Param{}) -> "localparam"
        _ -> ""

emptyWarnings :: AST -> AST -> IO ()
emptyWarnings before after =
    if all isComment before || not (all isComment after) || null kinds then
        return ()
    else if elem "interface" kinds then
        hPutStrLn stderr $ "Warning: Source includes an interface but the"
            ++ " output is empty because there are no modules without any"
            ++ " interface ports. Please convert interfaces alongside the"
            ++ " modules that instantiate them."
    else
        hPutStrLn stderr $ "Warning: Source includes a " ++ kind ++ " but no"
            ++ " modules. Such elements are elaborated into the modules that"
            ++ " use them. Please convert all sources in one invocation."
    where
        kinds = nub $ filter (not . null) $ map droppedKind before
        kind = head kinds

rewritePath :: FilePath -> IO FilePath
rewritePath path = do
    when (end /= ext) $ do
        hPutStrLn stderr $ "Refusing to write adjacent to " ++ show path
            ++ " because that path does not end in " ++ show ext
        exitFailure
    return $ base ++ ".v"
    where
        ext = ".sv"
        (base, end) = splitExtension path

splitModules :: FilePath -> AST -> [(FilePath, String)]
splitModules dir (PackageItem (Decl CommentDecl{}) : ast) =
    splitModules dir ast
splitModules dir (description : ast) =
    (path, output) : splitModules dir ast
    where
        Part _ _ Module _ name _ _ = description
        path = combine dir $ name ++ ".v"
        output = show description ++ "\n"
splitModules _ [] = []

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
writeOutput (Directory d) _ asts = do
    let (outPaths, outputs) = unzip $ splitModules d $ concat asts
    zipWithM_ writeFile outPaths outputs

main :: IO ()
main = do
    job <- readJob
    -- parse the input files
    let config = Config
            { cfDefines          = define job
            , cfIncludePaths     = incdir job
            , cfLibraryPaths     = libdir job
            , cfSiloed           = siloed job
            , cfSkipPreprocessor = skipPreprocessor job
            , cfOversizedNumbers = oversizedNumbers job
            }
    result <- runExceptT $ parseFiles config (files job)
    case result of
        Left msg -> do
            hPutStrLn stderr msg
            exitFailure
        Right inputs -> do
            let (inPaths, asts) = unzip inputs
            -- convert the files if requested
            let converter = convert (top job) (dumpPrefix job) (exclude job)
            asts' <-
                if passThrough job then
                    return asts
                else
                    converter asts
            emptyWarnings (concat asts) (concat asts')
            -- write the converted files out
            writeOutput (write job) inPaths asts'
            exitSuccess
