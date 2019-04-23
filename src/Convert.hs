{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog to Verilog conversion
 -}

module Convert (convert) where

import Language.SystemVerilog.AST
import qualified Job (Exclude(..))

import qualified Convert.AlwaysKW
import qualified Convert.AsgnOp
import qualified Convert.Assertion
import qualified Convert.Bits
import qualified Convert.Enum
import qualified Convert.FuncRet
import qualified Convert.Interface
import qualified Convert.KWArgs
import qualified Convert.Logic
import qualified Convert.NamedBlock
import qualified Convert.NestTF
import qualified Convert.PackedArray
import qualified Convert.Return
import qualified Convert.StarPort
import qualified Convert.StmtBlock
import qualified Convert.Struct
import qualified Convert.Typedef
import qualified Convert.UnbasedUnsized
import qualified Convert.Unique

type Phase = AST -> AST

phases :: [Job.Exclude] -> [Phase]
phases excludes =
    [ Convert.AsgnOp.convert
    , Convert.NamedBlock.convert
    , Convert.Assertion.convert
    , Convert.Bits.convert
    , selectExclude (Job.Logic    , Convert.Logic.convert)
    , Convert.FuncRet.convert
    , Convert.Enum.convert
    , Convert.KWArgs.convert
    , Convert.PackedArray.convert
    , Convert.StarPort.convert
    , Convert.StmtBlock.convert
    , Convert.Struct.convert
    , Convert.Return.convert
    , Convert.Typedef.convert
    , Convert.UnbasedUnsized.convert
    , Convert.Unique.convert
    , selectExclude (Job.Interface, Convert.Interface.convert)
    , selectExclude (Job.Always   , Convert.AlwaysKW.convert)
    , Convert.NestTF.convert
    ]
    where
        selectExclude :: (Job.Exclude, Phase) -> Phase
        selectExclude (exclude, phase) =
            if elem exclude excludes
                then id
                else phase

run :: [Job.Exclude] -> Phase
run excludes = foldr (.) id $ phases excludes

convert :: [Job.Exclude] -> Phase
convert excludes = convert'
    where
        convert' :: Phase
        convert' descriptions =
            if descriptions == descriptions'
                then descriptions
                else convert' descriptions'
            where descriptions' = run excludes descriptions
