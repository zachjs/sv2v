{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Split descriptions into individual files
 -}

module Split (splitDescriptions) where

import Data.List (isPrefixOf)

import Language.SystemVerilog.AST

splitDescriptions :: AST -> [PackageItem] -> ([(String, AST)], [PackageItem])
splitDescriptions (PackageItem item : ungrouped) itemsBefore =
    (grouped, item : itemsAfter)
    where
        (grouped, itemsAfter) = splitDescriptions ungrouped (item : itemsBefore)
splitDescriptions (description : descriptions) itemsBefore =
    ((name, surrounded) : grouped, itemsAfter)
    where
        (grouped, itemsAfter) = splitDescriptions descriptions itemsBefore
        name = case description of
            Part _ _ _  _ x _ _ -> x
            Package     _ x   _ -> x
            Class       _ x _ _ -> x
        surrounded = surroundDescription itemsBefore description itemsAfter
splitDescriptions [] _ = ([], [])

data SurroundState = SurroundState
    { sKeptBefore :: [PackageItem]
    , sKeptAfter :: [PackageItem]
    , sCellDefine :: Bool
    , sUnconnectedDrive :: Maybe PackageItem
    , sDefaultNettype :: Maybe PackageItem
    , sComment :: Maybe PackageItem
    }

-- filter and include the surrounding package items for this description
surroundDescription :: [PackageItem] -> Description -> [PackageItem] -> AST
surroundDescription itemsBefore description itemsAfter =
    map PackageItem itemsBefore' ++ description : map PackageItem itemsAfter'
    where
        itemsBefore' = extraBefore ++ reverse (sKeptBefore state2)
        itemsAfter' = sKeptAfter state2 ++ reverse extraAfter

        state0 = SurroundState [] [] False Nothing Nothing Nothing
        state1 = foldr stepBefore state0 itemsBefore
        state2 = foldr stepAfter state1 itemsAfter

        (extraBefore, extraAfter) = foldr (<>) mempty $ map ($ state2) $
            [ applyLeader sDefaultNettype
            , applyCellDefine
            , applyUnconnectedDrive
            , applyLeader sComment
            ]

applyCellDefine :: SurroundState -> ([PackageItem], [PackageItem])
applyCellDefine state
    | sCellDefine state =
        ([Directive "`celldefine"], [Directive "`endcelldefine"])
    | otherwise = ([], [])

applyUnconnectedDrive :: SurroundState -> ([PackageItem], [PackageItem])
applyUnconnectedDrive state
    | Just item <- sUnconnectedDrive state =
        ([item], [Directive "`nounconnected_drive"])
    | otherwise = ([], [])

applyLeader :: (SurroundState -> Maybe PackageItem) -> SurroundState
    -> ([PackageItem], [PackageItem])
applyLeader getter state
    | Just item <- getter state = ([item], [])
    | otherwise = ([], [])

-- update the state with a pre-description item
stepBefore :: PackageItem -> SurroundState -> SurroundState
stepBefore item@(Decl CommentDecl{}) state =
    state { sComment = Just item }
stepBefore item@(Directive directive) state
    | matches "celldefine" = state { sCellDefine = True }
    | matches "endcelldefine" = state { sCellDefine = False }
    | matches "unconnected_drive" = state { sUnconnectedDrive = Just item }
    | matches "nounconnected_drive" = state { sUnconnectedDrive = Nothing }
    | matches "default_nettype" = state { sDefaultNettype = Just item }
    | matches "resetall" = state
        { sCellDefine = False
        , sUnconnectedDrive = Nothing
        , sDefaultNettype = Nothing
        }
    where matches = flip isPrefixOf directive . ('`' :)
stepBefore item state =
    state { sKeptBefore = item : sKeptBefore state }

-- update the state with a post-description item
stepAfter :: PackageItem -> SurroundState -> SurroundState
stepAfter (Decl CommentDecl{}) state = state
stepAfter (Directive directive) state
    | matches "celldefine" = state
    | matches "endcelldefine" = state
    | matches "unconnected_drive" = state
    | matches "nounconnected_drive" = state
    | matches "default_nettype" = state
    | matches "resetall" = state
    where matches = flip isPrefixOf directive . ('`' :)
stepAfter item state =
    state { sKeptAfter = item : sKeptAfter state }
