{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `always_comb` and `always_ff`
 -}

module Convert.AlwaysKW (convert) where

import Convert.Template.ModuleItem (moduleItemConverter)

import Language.SystemVerilog.AST

convert :: AST -> AST
convert = moduleItemConverter convertModuleItem

-- Conversions:
-- `always_comb` -> `always @*`
-- `always_ff` -> `always`

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (AlwaysC AlwaysComb stmt) =
    AlwaysC Always $ Timing SenseStar stmt
convertModuleItem (AlwaysC AlwaysFF stmt) =
    AlwaysC Always stmt
convertModuleItem other = other
