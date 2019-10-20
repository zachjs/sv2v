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
import qualified Convert.BlockDecl
import qualified Convert.DimensionQuery
import qualified Convert.EmptyArgs
import qualified Convert.Enum
import qualified Convert.ForDecl
import qualified Convert.Foreach
import qualified Convert.FuncRet
import qualified Convert.Interface
import qualified Convert.IntTypes
import qualified Convert.Jump
import qualified Convert.KWArgs
import qualified Convert.Logic
import qualified Convert.LogOp
import qualified Convert.MultiplePacked
import qualified Convert.NamedBlock
import qualified Convert.NestPI
import qualified Convert.Package
import qualified Convert.ParamType
import qualified Convert.RemoveComments
import qualified Convert.SignCast
import qualified Convert.Simplify
import qualified Convert.SizeCast
import qualified Convert.StarPort
import qualified Convert.StmtBlock
import qualified Convert.Stream
import qualified Convert.Struct
import qualified Convert.Typedef
import qualified Convert.UnbasedUnsized
import qualified Convert.Unique
import qualified Convert.UnpackedArray
import qualified Convert.Unsigned

type Phase = [AST] -> [AST]

phases :: [Job.Exclude] -> [Phase]
phases excludes =
    [ Convert.AsgnOp.convert
    , Convert.NamedBlock.convert
    , Convert.Assertion.convert
    , Convert.BlockDecl.convert
    , selectExclude (Job.Logic    , Convert.Logic.convert)
    , Convert.ForDecl.convert
    , Convert.FuncRet.convert
    , Convert.EmptyArgs.convert
    , Convert.IntTypes.convert
    , Convert.KWArgs.convert
    , Convert.LogOp.convert
    , Convert.MultiplePacked.convert
    , Convert.DimensionQuery.convert
    , Convert.ParamType.convert
    , Convert.SizeCast.convert
    , Convert.Simplify.convert
    , Convert.StarPort.convert
    , Convert.StmtBlock.convert
    , Convert.Stream.convert
    , Convert.Struct.convert
    , Convert.Typedef.convert
    , Convert.UnbasedUnsized.convert
    , Convert.Unique.convert
    , Convert.UnpackedArray.convert
    , Convert.Unsigned.convert
    , Convert.SignCast.convert
    , Convert.Package.convert
    , Convert.Enum.convert
    , Convert.NestPI.convert
    , Convert.Jump.convert
    , Convert.Foreach.convert
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
