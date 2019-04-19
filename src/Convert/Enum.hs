{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `enum`
 -
 - This conversion replaces the enum items with localparams declared within any
 - modules in which that enum type appears. This is not necessarily foolproof,
 - as some tools do allow the use of an enum item even if the actual enum type
 - does not appear in that description. The localparams are explicitly sized to
 - match the size of the converted enum type.
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
import Data.List (elemIndices, sortOn)
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type EnumInfo = (Range, [(Identifier, Maybe Expr)])
type Enums = Set.Set EnumInfo

convert :: AST -> AST
convert = traverseDescriptions convertDescription

defaultType :: Type
defaultType = IntegerVector TLogic Unspecified [(Number "31", Number "0")]

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ _ _ _ _)) =
    Part extern kw lifetime name ports (enumItems ++ items)
    where
        -- replace and collect the enum types in this description
        (Part extern kw lifetime name ports items, enums) =
            runWriter $
            traverseModuleItemsM (traverseTypesM traverseType) $
            traverseModuleItems (traverseExprs $ traverseNestedExprs traverseExpr) $
            description
        -- convert the collected enums into their corresponding localparams
        itemType r = Implicit Unspecified [r]
        enumPairs = sortOn snd $ concatMap enumVals $ Set.toList enums
        enumItems = map toItem enumPairs
        toItem ((r, x), v) =
            MIDecl $ Localparam (itemType r) x v'
            where v' = sizedExpr x r (simplify v)
convertDescription other = other

toBaseType :: Maybe Type -> Type
toBaseType Nothing = defaultType
toBaseType (Just (Implicit _ rs)) =
    fst (typeRanges defaultType) rs
toBaseType (Just t) =
    if null rs
        then tf [(Number "0", Number "0")]
        else t
    where (tf, rs) = typeRanges t

-- replace, but write down, enum types
traverseType :: Type -> Writer Enums Type
traverseType (Enum t v rs) = do
    let baseType = toBaseType t
    let (tf, [r]) = typeRanges baseType
    () <- tell $ Set.singleton (simplifyRange r, v)
    return $ tf (r : rs)
traverseType other = return other

simplifyRange :: Range -> Range
simplifyRange (a, b) = (simplify a, simplify b)

-- drop any enum type casts in favor of implicit conversion from the
-- converted type
traverseExpr :: Expr -> Expr
traverseExpr (Cast (Left (Enum _ _ _)) e) = e
traverseExpr other = other

enumVals :: (Range, [(Identifier, Maybe Expr)]) -> [((Range, Identifier), Expr)]
enumVals (r, l) =
    -- check for obviously duplicate values
    if noDuplicates
        then res
        else error $ "enum conversion has duplicate vals: "
                ++ show (zip keys vals)
    where
        keys = map fst l
        vals = tail $ scanl step (Number "-1") (map snd l)
        res = zip (zip (repeat r) keys) vals
        noDuplicates = all (null . tail . flip elemIndices vals) vals
        step :: Expr -> Maybe Expr -> Expr
        step _ (Just expr) = expr
        step expr Nothing =
            simplify $ BinOp Add expr (Number "1")
