{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `always_latch`, `always_comb`, and `always_ff`
 -
 - `always_latch` -> `always @*`
 - `always_comb` -> `always @*`
 - `always_ff` -> `always`
 -}

module Convert.AlwaysKW (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems replaceAlwaysKW

replaceAlwaysKW :: ModuleItem -> ModuleItem
replaceAlwaysKW (AlwaysC AlwaysLatch stmt) =
    AlwaysC Always $ Timing (Event EventStar) stmt
replaceAlwaysKW (AlwaysC AlwaysComb stmt) =
    AlwaysC Always $ Timing (Event EventStar) stmt
replaceAlwaysKW (AlwaysC AlwaysFF stmt) =
    AlwaysC Always stmt
replaceAlwaysKW other = other
