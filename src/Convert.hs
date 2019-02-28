{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog to Verilog conversion
 -}

module Convert (convert) where

import Language.SystemVerilog.AST
import Job (Target(..))

import qualified Convert.AlwaysKW
import qualified Convert.CaseKW
import qualified Convert.Logic
import qualified Convert.Typedef
import qualified Convert.PackedArray
import qualified Convert.SplitPortDecl
import qualified Convert.StarPort

type Phase = AST -> AST

phases :: Target -> [Phase]
phases YOSYS =
    [ Convert.Typedef.convert
    , Convert.PackedArray.convert
    , Convert.StarPort.convert
    ]
phases VTR =
    (phases YOSYS) ++
    [ Convert.AlwaysKW.convert
    , Convert.CaseKW.convert
    , Convert.Logic.convert
    , Convert.SplitPortDecl.convert
    ]

run :: Target -> Phase
run target = foldr (.) id $ phases target

convert :: Target -> Phase
convert target = convert'
    where
        convert' :: Phase
        convert' descriptions =
            if descriptions == descriptions'
                then descriptions
                else convert' descriptions'
            where descriptions' = run target descriptions
