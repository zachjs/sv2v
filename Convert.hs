{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog to Verilog conversion
 -}

module Convert (convert) where

import Language.SystemVerilog.AST

import qualified Convert.AlwaysKW
import qualified Convert.Logic

type Phase = [Module] -> [Module]

phases :: [Phase]
phases =
    [ Convert.AlwaysKW.convert
    , Convert.Logic.convert
    ]

run :: Phase
run = foldr (.) id phases

convert :: Phase
convert modules =
    let modules' = run modules
    in
        if modules == modules'
            then modules
            else convert modules'
