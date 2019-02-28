{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog to Verilog conversion
 -}

module Convert (convert) where

import Language.SystemVerilog.AST
import qualified Args as Args

import qualified Convert.AlwaysKW
import qualified Convert.CaseKW
import qualified Convert.Logic
import qualified Convert.Typedef
import qualified Convert.PackedArray
import qualified Convert.SplitPortDecl
import qualified Convert.StarPort

type Phase = AST -> AST

phases :: Args.Target -> [Phase]
phases Args.YOSYS =
    [ Convert.Typedef.convert
    , Convert.PackedArray.convert
    , Convert.StarPort.convert
    ]
phases Args.VTR =
    (phases Args.YOSYS) ++
    [ Convert.AlwaysKW.convert
    , Convert.CaseKW.convert
    , Convert.Logic.convert
    , Convert.SplitPortDecl.convert
    ]

run :: Args.Target -> Phase
run target = foldr (.) id $ phases target

convert :: Args.Target -> Phase
convert target = convert'
    where
        convert' :: Phase
        convert' descriptions =
            if descriptions == descriptions'
                then descriptions
                else convert' descriptions'
            where descriptions' = run target descriptions
