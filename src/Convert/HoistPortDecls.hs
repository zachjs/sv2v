{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - VCS doesn't like port declarations inside of `generate` blocks, so we hoist
 - them out with this conversion. This obviously isn't ideal, but it's
 - relatively straightforward, and testing in VCS is important.
 -}

module Convert.HoistPortDecls (convert) where

import Data.List (partition)

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions hoistPortDecls

hoistPortDecls :: Description -> Description
hoistPortDecls (Part extern kw lifetime name ports items) =
    Part extern kw lifetime name ports (concat $ map explode items)
    where
        explode :: ModuleItem -> [ModuleItem]
        explode (Generate genItems) =
            if null rest
                then portDecls
                else portDecls ++ [Generate rest]
            where
                (wrappedPortDecls, rest) = partition isPortDecl genItems
                portDecls = map (\(GenModuleItem item) -> item) wrappedPortDecls
                isPortDecl :: GenItem -> Bool
                isPortDecl (GenModuleItem (MIDecl (Variable dir _ _ _ _))) =
                    dir /= Local
                isPortDecl _ = False
        explode other = [other]
hoistPortDecls other = other
