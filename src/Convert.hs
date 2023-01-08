{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog to Verilog conversion
 -}

module Convert (convert) where

import Control.Monad ((>=>))

import Language.SystemVerilog.AST
import qualified Job (Exclude(..))

import qualified Convert.AlwaysKW
import qualified Convert.AsgnOp
import qualified Convert.Assertion
import qualified Convert.BlockDecl
import qualified Convert.Cast
import qualified Convert.DimensionQuery
import qualified Convert.DoWhile
import qualified Convert.DuplicateGenvar
import qualified Convert.EmptyArgs
import qualified Convert.Enum
import qualified Convert.EventEdge
import qualified Convert.ExprAsgn
import qualified Convert.ForAsgn
import qualified Convert.Foreach
import qualified Convert.FuncRet
import qualified Convert.FuncRoutine
import qualified Convert.HierConst
import qualified Convert.ImplicitNet
import qualified Convert.Inside
import qualified Convert.Interface
import qualified Convert.IntTypes
import qualified Convert.Jump
import qualified Convert.KWArgs
import qualified Convert.Logic
import qualified Convert.LogOp
import qualified Convert.MultiplePacked
import qualified Convert.NamedBlock
import qualified Convert.Package
import qualified Convert.ParamNoDefault
import qualified Convert.ParamType
import qualified Convert.PortDecl
import qualified Convert.RemoveComments
import qualified Convert.ResolveBindings
import qualified Convert.Simplify
import qualified Convert.Stream
import qualified Convert.StringParam
import qualified Convert.StringType
import qualified Convert.Struct
import qualified Convert.StructConst
import qualified Convert.TFBlock
import qualified Convert.Typedef
import qualified Convert.TypeOf
import qualified Convert.UnbasedUnsized
import qualified Convert.Unique
import qualified Convert.UnnamedGenBlock
import qualified Convert.UnpackedArray
import qualified Convert.Unsigned
import qualified Convert.Wildcard

type Phase = [AST] -> [AST]
type IOPhase = [AST] -> IO [AST]
type Selector = Job.Exclude -> Phase -> Phase

finalPhases :: Selector -> [Phase]
finalPhases _ =
    [ Convert.NamedBlock.convert
    , Convert.DuplicateGenvar.convert
    , Convert.AsgnOp.convert
    , Convert.EmptyArgs.convert
    , Convert.FuncRet.convert
    , Convert.TFBlock.convert
    , Convert.StringType.convert
    ]

mainPhases :: Selector -> [Phase]
mainPhases selectExclude =
    [ Convert.BlockDecl.convert
    , selectExclude Job.Logic Convert.Logic.convert
    , Convert.ImplicitNet.convert
    , Convert.Inside.convert
    , Convert.IntTypes.convert
    , Convert.MultiplePacked.convert
    , selectExclude Job.UnbasedUnsized Convert.UnbasedUnsized.convert
    , Convert.Cast.convert
    , Convert.ParamType.convert
    , Convert.HierConst.convert
    , Convert.TypeOf.convert
    , Convert.DimensionQuery.convert
    , Convert.Simplify.convert
    , Convert.Stream.convert
    , Convert.Struct.convert
    , Convert.Typedef.convert
    , Convert.UnpackedArray.convert
    , Convert.Unsigned.convert
    , Convert.Wildcard.convert
    , Convert.Enum.convert
    , Convert.StringParam.convert
    , selectExclude Job.Interface Convert.Interface.convert
    , selectExclude Job.Succinct Convert.RemoveComments.convert
    ]

initialPhases :: Selector -> [Phase]
initialPhases selectExclude =
    [ Convert.ForAsgn.convert
    , Convert.Jump.convert
    , Convert.ExprAsgn.convert
    , Convert.KWArgs.convert
    , Convert.Unique.convert
    , Convert.EventEdge.convert
    , Convert.LogOp.convert
    , Convert.DoWhile.convert
    , Convert.Foreach.convert
    , Convert.FuncRoutine.convert
    , selectExclude Job.Assert Convert.Assertion.convert
    , selectExclude Job.Always Convert.AlwaysKW.convert
    , Convert.Package.convert
    , Convert.StructConst.convert
    , Convert.PortDecl.convert
    , Convert.ParamNoDefault.convert
    , Convert.ResolveBindings.convert
    , Convert.UnnamedGenBlock.convert
    ]

convert :: FilePath -> [Job.Exclude] -> IOPhase
convert dumpPrefix excludes =
    step "parse"   id      >=>
    step "initial" initial >=>
    loop 1 "main"  main    >=>
    step "final"   final
    where
        final = combine $ finalPhases selectExclude
        main = combine $ mainPhases selectExclude
        initial = combine $ initialPhases selectExclude
        combine = foldr1 (.)

        selectExclude :: Selector
        selectExclude exclude phase =
            if elem exclude excludes
                then id
                else phase

        dumper :: String -> IOPhase
        dumper =
            if null dumpPrefix
                then const return
                else fileDumper dumpPrefix

        -- add debug dumping to a phase
        step :: String -> Phase -> IOPhase
        step key = (dumper key .)

        -- add convergence and debug dumping to a phase
        loop :: Int -> String -> Phase -> IOPhase
        loop idx key phase files =
            if files == files'
                then return files
                else dumper key' files' >>= loop (idx + 1) key phase
            where
                files' = phase files
                key' = key ++ "_" ++ show idx

-- pass through dumper which writes ASTs to a file
fileDumper :: String -> String -> IOPhase
fileDumper prefix key files = do
    let path = prefix ++ key ++ ".sv"
    let output = show $ concat files
    writeFile path output
    return files
