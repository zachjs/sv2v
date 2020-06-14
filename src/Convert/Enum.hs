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

import Control.Monad.Writer
import Data.List (elemIndices)
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type EnumInfo = (Type, [(Identifier, Expr)])
type Enums = Set.Set EnumInfo

convert :: [AST] -> [AST]
convert = map $ concatMap convertDescription

convertDescription :: Description -> [Description]
convertDescription (description @ Package{}) =
    [Package ml name (items ++ enumItems)]
    where (Package ml name items, enumItems) = convertDescription' description
convertDescription description =
    (map PackageItem enumItems) ++ [description']
    where (description', enumItems) = convertDescription' description

-- replace and collect the enum types in a description
convertDescription' :: Description -> (Description, [PackageItem])
convertDescription' description =
    (description', enumItems)
    where
        -- replace and collect the enum types in this description
        (description', enums) = runWriter $
            traverseModuleItemsM (traverseTypesM traverseType) description
        -- convert the collected enums into their corresponding localparams
        enumItems = concatMap makeEnumItems $ Set.toList enums

-- replace, but write down, enum types
traverseType :: Type -> Writer Enums Type
traverseType (Enum (t @ Alias{}) v rs) =
    return $ Enum t v rs -- not ready
traverseType (Enum (Implicit sg rl) v rs) =
    traverseType $ Enum t' v rs
    where
        -- default to a 32 bit logic
        t' = IntegerVector TLogic sg rl'
        rl' = if null rl
            then [(Number "31", Number "0")]
            else rl
traverseType (Enum t v rs) = do
    let (tf, rl) = typeRanges t
    rlParam <- case rl of
        [ ] -> return [(Number "0", Number "0")]
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
        vals = tail $ scanl step (Number "-1") (map snd l)
        noDuplicates = all (null . tail . flip elemIndices vals) vals
        step :: Expr -> Expr -> Expr
        step expr Nil = simplify $ BinOp Add expr (Number "1")
        step _ expr = expr
        toPackageItem :: Identifier -> Expr -> PackageItem
        toPackageItem x v =
            Decl $ Param Localparam itemType x v'
            where v' = simplify v
