{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
( parseFile
) where

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.Parse
import Language.SystemVerilog.Parser.Preprocess

-- parses a file given a table of predefined macros and the file name
parseFile :: [String] -> [(String, String)] -> FilePath -> IO AST
parseFile includePaths env file =
    loadFile file >>=
    preprocess includePaths env >>=
    return . descriptions
