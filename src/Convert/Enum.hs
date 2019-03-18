{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `enum`
 -
 - TODO: We do not yet properly support enums which specify the value for some,
 - but not all items. My understanding is that they should continue in order
 - from the previous value.
 -}

module Convert.Enum (convert) where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad.Writer
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type EnumInfo = (Maybe Type, [(Identifier, Maybe Expr)])
type Enums = Set.Set EnumInfo

convert :: AST -> AST
convert = traverseDescriptions convertDescription

defaultType :: Type
defaultType = Logic [(Number "31", Number "0")]

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ _ _)) =
    Part kw name ports (enumItems ++ items)
    where
        enumPairs = concat $ map (uncurry enumVals) $ Set.toList enums
        enumItems = map (\(x, v) -> MIDecl $ Localparam (Implicit []) x v) enumPairs
        (Part kw name ports items, enums) =
            runWriter $ traverseModuleItemsM (traverseTypesM traverseType) $
            traverseModuleItems (traverseExprs $ traverseNestedExprs traverseExpr) $
            description
        traverseType :: Type -> Writer Enums Type
        traverseType (Enum t v r) = do
            () <- tell $ Set.singleton (t, v)
            let baseType = fromMaybe defaultType t
            let (tf, rs) = typeRanges baseType
            return $ tf (rs ++ r)
        traverseType other = return other
        -- drop any enum type casts in favor of implicit conversion from the
        -- converted type
        traverseExpr :: Expr -> Expr
        traverseExpr (Cast (Enum _ _ _) e) = e
        traverseExpr other = other
convertDescription other = other

enumVals :: Maybe Type -> [(Identifier, Maybe Expr)] -> [(Identifier, Expr)]
enumVals _ l = zip
    (map fst l)
    (tail $ scanl step (Number "-1") (map snd l))
    where
        step :: Expr -> Maybe Expr -> Expr
        step _ (Just expr) = expr
        step (Number n) Nothing =
            case (readMaybe n) :: Maybe Int of
                Just value -> Number (show $ value + 1)
                Nothing -> BinOp Add (Number n) (Number "1")
        step expr Nothing =
            BinOp Add expr (Number "1")
