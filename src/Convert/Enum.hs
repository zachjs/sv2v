{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `enum`
 -
 - This conversion replaces the enum items with localparams declared at the
 - global scope. We leave it to the package item nesting conversion to determine
 - where the generated localparams are needed. The localparams are explicitly
 - sized to match the size of the converted enum type.
 -
 - SystemVerilog allows for enums to have any number of the items' values
 - specified or unspecified. If the first one is unspecified, it is 0. All other
 - values take on the value of the previous item, plus 1.
 -
 - It is an error for multiple items of the same enum to take on the same value,
 - whether implicitly or explicitly. We catch try to catch "obvious" instances
 - of conflicts.
 -}

module Convert.Enum (convert) where

import Control.Monad.Writer.Strict
import Data.List (elemIndices)
import qualified Data.Set as Set

import Convert.ExprUtils
import Convert.Traverse
import Language.SystemVerilog.AST

type EnumInfo = (Type, [(Identifier, Expr)])
type Enums = Set.Set EnumInfo

convert :: [AST] -> [AST]
convert = map $ concatMap convertDescription

convertDescription :: Description -> [Description]
convertDescription (Package ml name items) =
    [Package ml name $ concatMap convertPackageItem items]
convertDescription description =
    (map PackageItem enumItems) ++ [description']
    where (description', enumItems) = convertDescription' description

-- explode a package item with its corresponding enum items
convertPackageItem :: PackageItem -> [PackageItem]
convertPackageItem item = do
    item' : enumItems
    where
        (PackageItem item', enumItems) =
            convertDescription' $ PackageItem item

-- replace and collect the enum types in a description
convertDescription' :: Description -> (Description, [PackageItem])
convertDescription' description =
    (description', enumItems)
    where
        -- replace and collect the enum types in this description
        (description', enums) = runWriter $
            traverseModuleItemsM traverseModuleItemM description
        traverseModuleItemM = traverseTypesM $ traverseNestedTypesM traverseType
        -- convert the collected enums into their corresponding localparams
        enumItems = concatMap makeEnumItems $ Set.toList enums

-- replace, but write down, enum types
traverseType :: Type -> Writer Enums Type
traverseType (Enum (t @ Alias{}) v rs) =
    return $ Enum t v rs -- not ready
traverseType (Enum (t @ PSAlias{}) v rs) =
    return $ Enum t v rs -- not ready
traverseType (Enum (t @ CSAlias{}) v rs) =
    return $ Enum t v rs -- not ready
traverseType (Enum (Implicit sg rl) v rs) =
    traverseType $ Enum t' v rs
    where
        -- default to a 32 bit logic
        t' = IntegerVector TLogic sg rl'
        rl' = if null rl
            then [(RawNum 31, RawNum 0)]
            else rl
traverseType (Enum t v rs) = do
    let (tf, rl) = typeRanges t
    rlParam <- case rl of
        [ ] -> return [(RawNum 0, RawNum 0)]
        [_] -> return rl
        _   -> error $ "unexpected multi-dim enum type: " ++ show (Enum t v rs)
    tell $ Set.singleton (tf rlParam, v) -- type of localparams
    return $ tf (rl ++ rs) -- type of variables
traverseType other = return other

makeEnumItems :: EnumInfo -> [PackageItem]
makeEnumItems (itemType, l) =
    -- check for obviously duplicate values
    if noDuplicates
        then zipWith toPackageItem keys vals
        else error $ "enum conversion has duplicate vals: "
                ++ show (zip keys vals)
    where
        keys = map fst l
        vals = tail $ scanl step (UniOp UniSub $ RawNum 1) (map snd l)
        noDuplicates = all (null . tail . flip elemIndices vals) vals
        step :: Expr -> Expr -> Expr
        step expr Nil = simplify $ BinOp Add expr (RawNum 1)
        step _ expr = expr
        toPackageItem :: Identifier -> Expr -> PackageItem
        toPackageItem x v =
            Decl $ Param Localparam itemType x v'
            where v' = simplify v
