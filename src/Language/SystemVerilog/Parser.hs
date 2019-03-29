{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
( parseFile
) where

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.Lex
import Language.SystemVerilog.Parser.Parse

-- parses a file given a table of predefined macros and the file name
parseFile :: [String] -> [(String, String)] -> FilePath -> IO AST
parseFile includePaths env file =
    lexFile includePaths env file >>=
    return . descriptions
