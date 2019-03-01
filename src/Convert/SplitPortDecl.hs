{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for splitting up complex port declarations. VTR doesn't support:
 - `input wire foo;` but does suport: `input foo; wire foo;`.
 -}

module Convert.SplitPortDecl (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (Module name ports items) =
    Module name ports (concat $ map splitPortDecl items)
convertDescription other = other

splitPortDecl :: ModuleItem -> [ModuleItem]
splitPortDecl (orig @ (MIDecl (Variable Local _ _ _ _))) = [orig]
splitPortDecl (orig @ (MIDecl (Variable _ (Implicit _) _ _ _))) = [orig]
splitPortDecl (MIDecl (Variable d t x a me)) =
    [ MIDecl $ Variable d     (Implicit r) x a Nothing
    , MIDecl $ Variable Local t            x a me      ]
    where (_, r) = typeRanges t
splitPortDecl other = [other]
