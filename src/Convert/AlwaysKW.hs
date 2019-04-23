{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `always_comb` and `always_ff`
 -
 - `always_comb` -> `always @*`
 - `always_ff` -> `always`
 -}

module Convert.AlwaysKW (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions $ traverseModuleItems replaceAlwaysKW

replaceAlwaysKW :: ModuleItem -> ModuleItem
replaceAlwaysKW (AlwaysC AlwaysComb stmt) =
    AlwaysC Always $ Timing (Event SenseStar) stmt
replaceAlwaysKW (AlwaysC AlwaysFF stmt) =
    AlwaysC Always stmt
replaceAlwaysKW other = other
