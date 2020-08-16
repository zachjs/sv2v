{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion to remove duplicate genvar declarations
 -}

module Convert.DuplicateGenvar (convert) where

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

traverseDescription :: Description -> Description
traverseDescription = partScoper return traverseModuleItemM return return

traverseModuleItemM :: ModuleItem -> Scoper () ModuleItem
traverseModuleItemM (Genvar x) = do
    details <- lookupElemM x
    if details == Nothing
        then insertElem x () >> return (Genvar x)
        else return $ Generate []
traverseModuleItemM item = return item
