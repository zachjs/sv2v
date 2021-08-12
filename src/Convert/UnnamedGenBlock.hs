{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Labels any unnamed generate blocks, per IEEE 1800-2017 Section 27.6
 -
 - This transformation is performed before any others, and is only performed
 - once. The AST traversal utilities are not used here to avoid the automatic
 - elaboration they perform.
 -}

module Convert.UnnamedGenBlock (convert) where

import Control.Monad.State.Strict
import Data.List (isPrefixOf)

import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ map traverseDescription

traverseDescription :: Description -> Description
traverseDescription (Part attrs extern kw lifetime name ports items) =
    Part attrs extern kw lifetime name ports $
        evalState (mapM traverseModuleItemM items) initialState
traverseDescription other = other

type S = State Info
type Info = ([Identifier], Int)

initialState :: Info
initialState = ([], 1)

traverseModuleItemM :: ModuleItem -> S ModuleItem
traverseModuleItemM item@(Genvar x) = declaration x item
traverseModuleItemM item@(NInputGate  _ _ x _ _) = declaration x item
traverseModuleItemM item@(NOutputGate _ _ x _ _) = declaration x item
traverseModuleItemM item@(Instance    _ _ x _ _) = declaration x item
traverseModuleItemM (MIPackageItem (Decl decl)) =
    traverseDeclM decl >>= return . MIPackageItem . Decl
traverseModuleItemM (MIAttr attr item) =
    traverseModuleItemM item >>= return . MIAttr attr
traverseModuleItemM (Generate items) =
    mapM traverseGenItemM items >>= return . Generate
traverseModuleItemM item = return item

-- add a declaration to the conflict list
traverseDeclM :: Decl -> S Decl
traverseDeclM decl =
    case decl of
        Variable  _ _ x _ _ -> declaration x decl
        Net   _ _ _ _ x _ _ -> declaration x decl
        Param     _ _ x _   -> declaration x decl
        ParamType _   x _   -> declaration x decl
        CommentDecl{} -> return decl

-- label the generate blocks within an individual generate item which is already
-- in a list of generate items (top level or generate block)
traverseGenItemM :: GenItem -> S GenItem
traverseGenItemM item@GenIf{} = do
    item' <- labelGenElse item
    incrCount >> return item'
traverseGenItemM item@GenBlock{} = do
    item' <- labelBlock item
    incrCount >> return item'
traverseGenItemM (GenFor a b c item) = do
    item' <- labelBlock item
    incrCount >> return (GenFor a b c item')
traverseGenItemM (GenCase expr cases) = do
    let (exprs, items) = unzip cases
    items' <- mapM labelBlock items
    let cases' = zip exprs items'
    incrCount >> return (GenCase expr cases')
traverseGenItemM (GenModuleItem item) =
    traverseModuleItemM item >>= return . GenModuleItem
traverseGenItemM GenNull = return GenNull

-- increment the counter each time a generate construct is encountered
incrCount :: S ()
incrCount = modify' $ \(idents, count) -> (idents, count + 1)

genblk :: Identifier
genblk = "genblk"

-- adds the given identifier to the list of possible identifier conflicts, if
-- necessary, and then returns the second argument as a shorthand courtesy
declaration :: Identifier -> a -> S a
declaration x a = do
    when (genblk `isPrefixOf` x) $ do
        let ident = drop (length genblk) x
        modify' $ \(idents, count) -> (ident : idents, count)
    return a

-- generate a locally unique gen block name
makeBlockName :: S Identifier
makeBlockName = do
    (idents, count) <- get
    let uniqueSuffix = prependZeroes idents (show count)
    return $ genblk ++ uniqueSuffix

-- prepend zeroes until the string isn't in the list
prependZeroes :: [String] -> String -> String
prependZeroes xs x | notElem x xs = x
prependZeroes xs x = prependZeroes xs ('0' : x)

-- if the item is a generate conditional item, give its `then` block and any
-- direct `else if` blocks the same name
labelGenElse :: GenItem -> S GenItem
labelGenElse (GenIf cond thenItem elseItem) = do
    thenItem' <- labelBlock thenItem
    elseItem' <- labelGenElse elseItem
    return $ GenIf cond thenItem' elseItem'
labelGenElse other = labelBlock other

-- transform the given item into a named generate block
labelBlock :: GenItem -> S GenItem
labelBlock (GenBlock "" items) =
    makeBlockName >>= labelBlock . flip GenBlock items
labelBlock (GenBlock x items) =
    return $ GenBlock x $
        evalState (mapM traverseGenItemM items) initialState
labelBlock GenNull = return GenNull
labelBlock item = labelBlock $ GenBlock "" [item]
