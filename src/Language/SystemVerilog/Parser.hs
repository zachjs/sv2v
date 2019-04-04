{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -}
module Language.SystemVerilog.Parser
    ( parseFile
    ) where

import Language.SystemVerilog.AST (AST)
import Language.SystemVerilog.Parser.Lex (lexFile)
import Language.SystemVerilog.Parser.Parse (parse)

-- parses a file given include search paths, a table of predefined macros, and
-- the file path
parseFile :: [String] -> [(String, String)] -> FilePath -> IO AST
parseFile includePaths defines path =
    lexFile includePaths defines path >>= return . parse
