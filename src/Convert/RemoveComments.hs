{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for removing any comments
 -}

module Convert.RemoveComments (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map convertFile

convertFile :: AST -> AST
convertFile =
    traverseDescriptions (traverseModuleItems convertModuleItem) .
    filter (not . isComment)

isComment :: Description -> Bool
isComment (PackageItem (Comment _)) = True
isComment _ = False

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (MIPackageItem (Comment _)) = Generate []
convertModuleItem other = other
