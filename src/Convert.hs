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
import qualified Convert.BlockDecl
import qualified Convert.EmptyArgs
import qualified Convert.Enum
import qualified Convert.ForDecl
import qualified Convert.FuncRet
import qualified Convert.Interface
import qualified Convert.IntTypes
import qualified Convert.KWArgs
import qualified Convert.Logic
import qualified Convert.Mux
import qualified Convert.NamedBlock
import qualified Convert.NestPI
import qualified Convert.Package
import qualified Convert.PackedArray
import qualified Convert.RemoveComments
import qualified Convert.Return
import qualified Convert.StarPort
import qualified Convert.StmtBlock
import qualified Convert.Stream
import qualified Convert.Struct
import qualified Convert.Typedef
import qualified Convert.UnbasedUnsized
import qualified Convert.Unique

type Phase = [AST] -> [AST]

phases :: [Job.Exclude] -> [Phase]
phases excludes =
    [ Convert.AsgnOp.convert
    , Convert.NamedBlock.convert
    , Convert.Assertion.convert
    , Convert.Bits.convert
    , Convert.BlockDecl.convert
    , selectExclude (Job.Logic    , Convert.Logic.convert)
    , Convert.ForDecl.convert
    , Convert.FuncRet.convert
    , Convert.EmptyArgs.convert
    , Convert.IntTypes.convert
    , Convert.KWArgs.convert
    , Convert.Mux.convert
    , Convert.PackedArray.convert
    , Convert.StarPort.convert
    , Convert.StmtBlock.convert
    , Convert.Stream.convert
    , Convert.Struct.convert
    , Convert.Typedef.convert
    , Convert.UnbasedUnsized.convert
    , Convert.Unique.convert
    , Convert.Package.convert
    , Convert.Enum.convert
    , Convert.NestPI.convert
    , Convert.Return.convert
    , selectExclude (Job.Interface, Convert.Interface.convert)
    , selectExclude (Job.Always   , Convert.AlwaysKW.convert)
    , selectExclude (Job.Succinct , Convert.RemoveComments.convert)
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
