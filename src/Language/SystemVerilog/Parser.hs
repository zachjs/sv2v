{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
( parseFile
) where

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.Lex
import Language.SystemVerilog.Parser.Parse
import Language.SystemVerilog.Parser.Preprocess

import Control.Monad.State
import qualified Data.Map.Strict as Map

-- parses a file given a table of predefined macros and the file name
parseFile :: [(String, String)] -> FilePath -> IO AST
parseFile env file = do
    let initialEnv = Map.map alexScanTokens $ Map.fromList env
    let initialState = PP initialEnv []
    ast <- evalStateT (loadFile file) initialState
    return $ descriptions ast
