{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - Helpers for printing AST items
 -}

module Language.SystemVerilog.AST.ShowHelp
    ( showPad
    , showPadBefore
    , indent
    , unlines'
    , commas
    , indentedParenList
    , showEither
    ) where

import Data.List (intercalate)

showPad :: Show t => t -> String
showPad x =
    if str == ""
        then ""
        else str ++ " "
    where str = show x

showPadBefore :: Show t => t -> String
showPadBefore x =
    if str == ""
        then ""
        else " " ++ str
    where str = show x

indent :: String -> String
indent a = '\t' : f a
    where
        f [] = []
        f ('\n' : xs) = "\n\t" ++ f xs
        f (x : xs) = x : f xs

unlines' :: [String] -> String
unlines' = intercalate "\n"

commas :: [String] -> String
commas = intercalate ", "

indentedParenList :: [String] -> String
indentedParenList [] = "()"
indentedParenList [x] = "(" ++ x ++ ")"
indentedParenList l = "(\n" ++ (indent $ intercalate ",\n" l) ++ "\n)"

showEither :: (Show a, Show b) => Either a b -> String
showEither (Left  v) = show v
showEither (Right v) = show v
