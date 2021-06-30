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
import qualified Convert.Cast
import qualified Convert.DimensionQuery
import qualified Convert.DuplicateGenvar
import qualified Convert.EmptyArgs
import qualified Convert.Enum
import qualified Convert.ForDecl
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
import qualified Convert.RemoveComments
import qualified Convert.ResolveBindings
import qualified Convert.Simplify
import qualified Convert.Stream
import qualified Convert.StringParam
import qualified Convert.Struct
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
type Selector = Job.Exclude -> Phase -> Phase

finalPhases :: Selector -> [Phase]
finalPhases _ =
    [ Convert.NamedBlock.convert
    , Convert.DuplicateGenvar.convert
    ]

mainPhases :: Selector -> [Phase]
mainPhases selectExclude =
    [ Convert.AsgnOp.convert
    , Convert.BlockDecl.convert
    , selectExclude Job.Logic Convert.Logic.convert
    , Convert.FuncRet.convert
    , Convert.FuncRoutine.convert
    , Convert.EmptyArgs.convert
    , Convert.ImplicitNet.convert
    , Convert.Inside.convert
    , Convert.IntTypes.convert
    , Convert.KWArgs.convert
    , Convert.MultiplePacked.convert
    , Convert.UnbasedUnsized.convert
    , Convert.Cast.convert
    , Convert.ParamType.convert
    , Convert.HierConst.convert
    , Convert.TypeOf.convert
    , Convert.DimensionQuery.convert
    , Convert.Simplify.convert
    , Convert.Stream.convert
    , Convert.Struct.convert
    , Convert.TFBlock.convert
    , Convert.Typedef.convert
    , Convert.UnpackedArray.convert
    , Convert.Unsigned.convert
    , Convert.Wildcard.convert
    , Convert.Enum.convert
    , Convert.ForDecl.convert
    , Convert.StringParam.convert
    , selectExclude Job.Interface Convert.Interface.convert
    , selectExclude Job.Succinct Convert.RemoveComments.convert
    ]

initialPhases :: Selector -> [Phase]
initialPhases selectExclude =
    [ Convert.Jump.convert
    , Convert.Unique.convert
    , Convert.LogOp.convert
    , Convert.Foreach.convert
    , selectExclude Job.Assert Convert.Assertion.convert
    , selectExclude Job.Always Convert.AlwaysKW.convert
    , Convert.Package.convert
    , Convert.ParamNoDefault.convert
    , Convert.ResolveBindings.convert
    , Convert.UnnamedGenBlock.convert
    ]

convert :: [Job.Exclude] -> Phase
convert excludes =
    final . loopMain . initial
    where
        final = combine $ finalPhases selectExclude
        main = combine $ mainPhases selectExclude
        initial = combine $ initialPhases selectExclude
        combine = foldr1 (.)
        loopMain :: Phase
        loopMain descriptions =
            if descriptions == descriptions'
                then descriptions
                else loopMain descriptions'
            where descriptions' = main descriptions
        selectExclude :: Selector
        selectExclude exclude phase =
            if elem exclude excludes
                then id
                else phase
