module Language.SystemVerilog.Parser
  ( parseFile
  , preprocess
  ) where

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.Lex
import Language.SystemVerilog.Parser.Parse
import Language.SystemVerilog.Parser.Preprocess
import Language.SystemVerilog.Parser.Tokens

-- | Parses a file given a table of predefined macros, the file name, and the file contents.
parseFile :: [(String, String)] -> FilePath -> String -> AST
parseFile env file content = descriptions tokens
  where
  tokens = map relocate $ alexScanTokens $ preprocess env file content
  relocate :: Token -> Token
  relocate (Token t s (Position _ l c)) = Token t s $ Position file l c

