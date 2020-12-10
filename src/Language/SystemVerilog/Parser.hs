{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
    ( parseFiles
    ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Language.SystemVerilog.AST (AST)
import Language.SystemVerilog.Parser.Lex (lexStr)
import Language.SystemVerilog.Parser.Parse (parse)
import Language.SystemVerilog.Parser.Preprocess (preprocess, annotate, Env)
import Language.SystemVerilog.Parser.Tokens (Position(..), tokenPosition)

-- parses a compilation unit given include search paths and predefined macros
parseFiles :: [FilePath] -> [(String, String)] -> Bool -> Bool -> [FilePath] -> IO (Either String [AST])
parseFiles includePaths defines siloed skipPreprocessor paths = do
    let env = Map.map (\a -> (a, [])) $ Map.fromList defines
    runExceptT (parseFiles' includePaths env siloed skipPreprocessor paths)

-- parses a compilation unit given include search paths and predefined macros
parseFiles' :: [FilePath] -> Env -> Bool -> Bool -> [FilePath] -> ExceptT String IO [AST]
parseFiles' _ _ _ _ [] = return []
parseFiles' includePaths env siloed skipPreprocessor (path : paths) = do
    (ast, envEnd) <- parseFile' includePaths env skipPreprocessor path
    let envNext = if siloed then env else envEnd
    asts <- parseFiles' includePaths envNext siloed skipPreprocessor paths
    return $ ast : asts

-- parses a file given include search paths, a table of predefined macros, and
-- the file path
parseFile' :: [String] -> Env -> Bool -> FilePath -> ExceptT String IO (AST, Env)
parseFile' includePaths env skipPreprocessor path = do
    let runner = if skipPreprocessor then annotate else preprocess
    preResult <- liftIO $ runner includePaths env path
    (contents, env') <- liftEither preResult
    tokens <- liftEither $ uncurry lexStr $ unzip contents
    let position =
            if null tokens
                then Position path 1 1
                else tokenPosition $ head tokens
    ast <- evalStateT parse (position, tokens)
    return (ast, env')
