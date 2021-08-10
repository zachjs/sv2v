{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
    ( initialEnv
    , parseFiles
    , Config(..)
    ) where

import Control.Monad.Except
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map

import Language.SystemVerilog.AST (AST)
import Language.SystemVerilog.Parser.Lex (lexStr)
import Language.SystemVerilog.Parser.Parse (parse)
import Language.SystemVerilog.Parser.Preprocess (preprocess, annotate, Env, Contents)

data Config = Config
    { cfEnv :: Env
    , cfIncludePaths :: [FilePath]
    , cfSiloed :: Bool
    , cfSkipPreprocessor :: Bool
    , cfOversizedNumbers :: Bool
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
parseFiles :: Config -> [FilePath] -> ExceptT String IO [AST]
parseFiles _ [] = return []
parseFiles config (path : paths) = do
    (config', ast) <- parseFile config path
    fmap (ast :) $ parseFiles config' paths

-- parse an individual file, potentially updating the configuration
parseFile :: Config -> FilePath -> ExceptT String IO (Config, AST)
parseFile config path = do
    (config', contents) <- preprocessFile config path
    tokens <- liftEither $ runExcept $ lexStr contents
    ast <- parse (cfOversizedNumbers config) tokens
    return (config', ast)

-- preprocess an individual file, potentially updating the configuration
preprocessFile :: Config -> FilePath -> ExceptT String IO (Config, Contents)
preprocessFile config path | cfSkipPreprocessor config =
    fmap (config, ) $ annotate path
preprocessFile config path = do
    (env', contents) <- preprocess (cfIncludePaths config) env path
    let config' = config { cfEnv = if cfSiloed config then env else env' }
    return (config', contents)
    where env = cfEnv config
