{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
    ( parseFiles
    ) where

import qualified Data.Map.Strict as Map
import Language.SystemVerilog.AST (AST)
import Language.SystemVerilog.Parser.Lex (lexFile, Env)
import Language.SystemVerilog.Parser.Parse (parse)

-- parses a compilation unit given include search paths and predefined macros
parseFiles :: [FilePath] -> [(String, String)] -> [FilePath] -> IO [AST]
parseFiles includePaths defines paths = do
    let env = Map.map (\a -> (a, [])) $ Map.fromList defines
    parseFiles' includePaths env paths

-- parses a compilation unit given include search paths and predefined macros
parseFiles' :: [FilePath] -> Env -> [FilePath] -> IO [AST]
parseFiles' _ _ [] = return []
parseFiles' includePaths env (path : paths) = do
    (ast, env') <- parseFile' includePaths env path
    asts <- parseFiles' includePaths env' paths
    return $ ast : asts

-- parses a file given include search paths, a table of predefined macros, and
-- the file path
parseFile' :: [String] -> Env -> FilePath -> IO (AST, Env)
parseFile' includePaths env path = do
    (tokens, env') <- lexFile includePaths env path
    let ast = parse tokens
    return (ast, env')
