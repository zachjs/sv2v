{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog to Verilog conversion
 -}

module Convert (convert) where

import Language.SystemVerilog.AST

import qualified Convert.AlwaysKW
import qualified Convert.CaseKW
import qualified Convert.Logic
import qualified Convert.Typedef
import qualified Convert.PackedArrayFlatten
import qualified Convert.StarPort

type Phase = AST -> AST

phases :: [Phase]
phases =
    [ Convert.AlwaysKW.convert
    , Convert.CaseKW.convert
    , Convert.Logic.convert
    , Convert.Typedef.convert
    , Convert.PackedArrayFlatten.convert
    , Convert.StarPort.convert
    ]

run :: Phase
run = foldr (.) id phases

convert :: Phase
convert descriptions =
    let descriptions' = run descriptions
    in
        if descriptions == descriptions'
            then descriptions
            else convert descriptions'
