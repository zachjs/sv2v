{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Create declarations for implicit nets
 -}

module Convert.ImplicitNet (convert) where

import Control.Monad (when)
import Data.List (isPrefixOf, mapAccumL)

import Convert.Scoper
import Language.SystemVerilog.AST

type DefaultNetType = Maybe NetType

convert :: [AST] -> [AST]
convert =
    snd . mapAccumL
        (mapAccumL traverseDescription)
        (Just TWire)

traverseDescription :: DefaultNetType -> Description
    -> (DefaultNetType, Description)
traverseDescription defaultNetType (PackageItem (Directive str)) =
    (defaultNetType', PackageItem $ Directive str)
    where
        prefix = "`default_nettype "
        defaultNetType' =
            if isPrefixOf prefix str
                then parseDefaultNetType $ drop (length prefix) str
                else defaultNetType
traverseDescription defaultNetType (part @ Part{}) =
    (defaultNetType, partScoper traverseDeclM
        (traverseModuleItemM defaultNetType)
        return return part)
traverseDescription defaultNetType other = (defaultNetType, other)

traverseDeclM :: Decl -> Scoper () Decl
traverseDeclM decl = do
    case decl of
        Variable _ _ x _ _ -> insertElem x ()
        Param _ _ x _ -> insertElem x ()
        ParamType{} -> return ()
        CommentDecl{} -> return ()
    return decl

traverseModuleItemM :: DefaultNetType -> ModuleItem -> Scoper () ModuleItem
traverseModuleItemM _ (Genvar x) =
    insertElem x () >> return (Genvar x)
traverseModuleItemM defaultNetType (orig @ (Assign _ x _)) = do
    needsLHS defaultNetType x
    return orig
traverseModuleItemM defaultNetType (orig @ (NInputGate _ _ x lhs exprs)) = do
    insertElem x ()
    needsLHS defaultNetType lhs
    _ <- mapM (needsExpr defaultNetType) exprs
    return orig
traverseModuleItemM defaultNetType (orig @ (NOutputGate _ _ x lhss expr)) = do
    insertElem x ()
    _ <- mapM (needsLHS defaultNetType) lhss
    needsExpr defaultNetType expr
    return orig
traverseModuleItemM defaultNetType (orig @ (Instance _ _ _ _ ports)) = do
    _ <- mapM (needsExpr defaultNetType . snd) ports
    return orig
traverseModuleItemM _ item = return item

needsExpr :: DefaultNetType -> Expr -> Scoper () ()
needsExpr defaultNetType (Ident x) = needsIdent defaultNetType x
needsExpr _ _ = return ()

needsLHS :: DefaultNetType -> LHS -> Scoper () ()
needsLHS defaultNetType (LHSIdent x) = needsIdent defaultNetType x
needsLHS _ _ = return ()

needsIdent :: DefaultNetType -> Identifier -> Scoper () ()
needsIdent defaultNetType x = do
    details <- lookupElemM x
    when (details == Nothing) $ do
        insertElem x ()
        injectItem decl
    where
        t = impliedNetType x defaultNetType Unspecified []
        decl = MIPackageItem $ Decl $ Variable Local t x [] Nil

impliedNetType :: String -> DefaultNetType -> Signing -> [Range] -> Type
impliedNetType var Nothing =
    error $ "implicit declaration of " ++
        show var ++ " but default_nettype is none"
impliedNetType _ (Just netType) = Net (NetType netType)

parseDefaultNetType :: String -> DefaultNetType
parseDefaultNetType "tri"    = Just TTri
parseDefaultNetType "triand" = Just TTriand
parseDefaultNetType "trior"  = Just TTrior
parseDefaultNetType "trireg" = Just TTrireg
parseDefaultNetType "tri0"   = Just TTri0
parseDefaultNetType "tri1"   = Just TTri1
parseDefaultNetType "uwire"  = Just TUwire
parseDefaultNetType "wire"   = Just TWire
parseDefaultNetType "wand"   = Just TWand
parseDefaultNetType "wor"    = Just TWor
parseDefaultNetType "none"   = Nothing
parseDefaultNetType str      = error $ "bad default_nettype: " ++ show str
