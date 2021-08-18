{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - This AST allows for the representation of many syntactically invalid things,
 - like input regs or modport declarations inside a module. Representing only
 - syntactically valid files would make working with the AST a nightmare. We
 - have placed an emphasis on making the conversion procedures in this project
 - more easier to write, interpret, and maintain.
 -
 - In the future, we may want to have a utility which performs some basic
 - invariant checks. I want to avoid making a full type-checker though, as we
 - should only be given valid SystemVerilog input files.
 -}

module Language.SystemVerilog.AST
    ( AST
    , module Attr
    , module Decl
    , module Description
    , module Expr
    , module GenItem
    , module LHS
    , module ModuleItem
    , module Number
    , module Op
    , module Stmt
    , module Type
    , exprToLHS
    , lhsToExpr
    , exprToType
    , shortHash
    ) where

import Text.Printf (printf)
import Data.Bits ((.&.))
import Data.Hashable (hash)

import Language.SystemVerilog.AST.Attr as Attr
import Language.SystemVerilog.AST.Decl as Decl
import Language.SystemVerilog.AST.Description as Description
import Language.SystemVerilog.AST.Expr as Expr
import Language.SystemVerilog.AST.GenItem as GenItem
import Language.SystemVerilog.AST.LHS as LHS
import Language.SystemVerilog.AST.ModuleItem as ModuleItem
import Language.SystemVerilog.AST.Number as Number
import Language.SystemVerilog.AST.Op as Op
import Language.SystemVerilog.AST.Stmt as Stmt
import Language.SystemVerilog.AST.Type as Type

type AST = [Description]

exprToLHS :: Expr -> Maybe LHS
exprToLHS (Ident   x  ) = Just $ LHSIdent x
exprToLHS (Bit   l e  ) = do
    l' <- exprToLHS l
    Just $ LHSBit   l' e
exprToLHS (Range l m r) = do
    l' <- exprToLHS l
    Just $ LHSRange l' m r
exprToLHS (Dot   l x  ) = do
    l' <- exprToLHS l
    Just $ LHSDot   l' x
exprToLHS (Concat ls  ) = do
    ls' <- mapM exprToLHS ls
    Just $ LHSConcat ls'
exprToLHS (Pattern ls ) = do
    ls' <- mapM exprToLHS $ map snd ls
    if all ((== Right Nil) . fst) ls
        then Just $ LHSConcat ls'
        else Nothing
exprToLHS (Stream o e ls) = do
    ls' <- mapM exprToLHS ls
    Just $ LHSStream o e ls'
exprToLHS _ = Nothing

lhsToExpr :: LHS -> Expr
lhsToExpr (LHSIdent    x   ) = Ident x
lhsToExpr (LHSBit    l e   ) = Bit   (lhsToExpr l) e
lhsToExpr (LHSRange  l m r ) = Range (lhsToExpr l) m r
lhsToExpr (LHSDot    l x   ) = Dot   (lhsToExpr l) x
lhsToExpr (LHSConcat     ls) = Concat $ map lhsToExpr ls
lhsToExpr (LHSStream o e ls) = Stream o e $ map lhsToExpr ls

-- attempt to convert an expression to a syntactically equivalent type
exprToType :: Expr -> Maybe Type
exprToType (Ident       x) = Just $ Alias       x []
exprToType (PSIdent y   x) = Just $ PSAlias y   x []
exprToType (CSIdent y p x) = Just $ CSAlias y p x []
exprToType (Range e NonIndexed r) = do
    (tf, rs) <- fmap typeRanges $ exprToType e
    Just $ tf (rs ++ [r])
exprToType (Bit e i) = do
    (tf, rs) <- fmap typeRanges $ exprToType e
    let r = (BinOp Sub i (RawNum 1), RawNum 0)
    Just $ tf (rs ++ [r])
exprToType _ = Nothing

shortHash :: (Show a) => a -> String
shortHash x =
    printf "%05X" $ val .&. 0xFFFFF
    where val = hash $ show x
