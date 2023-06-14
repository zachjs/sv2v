{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
    ( parseFiles
    , Config(..)
    ) where

import Control.Monad.Except
import Data.List (elemIndex)
import Data.Maybe (catMaybes)
import System.Directory (findFile)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.SystemVerilog.AST (AST)
import Language.SystemVerilog.Parser.Lex (lexStr)
import Language.SystemVerilog.Parser.Parse (parse)
import Language.SystemVerilog.Parser.Preprocess (preprocess, annotate, Env, Contents)

type Output = (FilePath, AST)
type Strings = Set.Set String

data Config = Config
    { cfDefines :: [String]
    , cfIncludePaths :: [FilePath]
    , cfLibraryPaths :: [FilePath]
    , cfSiloed :: Bool
    , cfSkipPreprocessor :: Bool
    , cfOversizedNumbers :: Bool
    }

data Context = Context
    { ctConfig :: Config
    , ctEnv :: Env
    , ctUsed :: Strings
    , ctHave :: Strings
    }

-- parse CLI macro definitions into the internal macro environment format
initialEnv :: [String] -> Env
initialEnv = Map.map (, []) . Map.fromList . map splitDefine

-- split a raw CLI macro definition at the '=', if present
splitDefine :: String -> (String, String)
splitDefine str =
    case elemIndex '=' str of
        Nothing -> (str, "")
        Just idx -> (name, tail rest)
            where (name, rest) = splitAt idx str

-- parse a list of files according to the given configuration
parseFiles :: Config -> [FilePath] -> ExceptT String IO [Output]
parseFiles config = parseFiles' context . zip (repeat "")
    where
        context = Context config env mempty mempty
        env = initialEnv $ cfDefines config

-- parse files, keeping track of which parts are defined and used
parseFiles' :: Context -> [(String, FilePath)] -> ExceptT String IO [Output]

-- look for missing parts in libraries if any library paths were provided
parseFiles' context []
    | null libdirs = return []
    | otherwise = do
        possibleFiles <- catMaybes <$> mapM lookupLibrary missingParts
        if null possibleFiles
            then return []
            else parseFiles' context possibleFiles
    where
        missingParts = Set.toList $ ctUsed context Set.\\ ctHave context
        libdirs = cfLibraryPaths $ ctConfig context
        lookupLibrary partName = ((partName, ) <$>) <$> lookupLibFile partName
        lookupLibFile = liftIO . findFile libdirs . (++ ".sv")

-- load the files, but complain if an expected part is missing
parseFiles' context ((part, path) : files) = do
    (context', ast) <- parseFile context path
    let misdirected = not $ null part || Set.member part (ctHave context')
    when misdirected $ throwError $
        "Expected to find module or interface " ++ show part ++ " in file "
        ++ show path ++ " selected from the library path."
    ((path, ast) :) <$> parseFiles' context' files

-- parse an individual file, updating the context
parseFile :: Context -> FilePath -> ExceptT String IO (Context, AST)
parseFile context path = do
    (context', contents) <- preprocessFile context path
    tokens <- liftEither $ runExcept $ lexStr contents
    (ast, used, have) <- parse (cfOversizedNumbers config) tokens
    let context'' = context' { ctUsed = used <> ctUsed context
                             , ctHave = have <> ctHave context }
    return (context'', ast)
    where config = ctConfig context

-- preprocess an individual file, potentially updating the environment
preprocessFile :: Context -> FilePath -> ExceptT String IO (Context, Contents)
preprocessFile context path
    | cfSkipPreprocessor config =
        (context, ) <$> annotate path
    | otherwise = do
        (env', contents) <- preprocess (cfIncludePaths config) env path
        let context' = context { ctEnv = if cfSiloed config then env else env' }
        return (context', contents)
    where
        config = ctConfig context
        env = ctEnv context
