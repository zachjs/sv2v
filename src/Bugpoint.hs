{-# LANGUAGE ScopedTypeVariables #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Utility for reducing test cases that cause conversion errors or otherwise
 - produce unexpected output.
 -}

module Bugpoint (runBugpoint) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Control.Exception (catches, ErrorCall(..), Handler(..), PatternMatchFail(..))
import Control.Monad (when, (>=>))
import Data.Functor ((<&>))
import Data.List (isInfixOf)

import qualified Convert.RemoveComments
import Language.SystemVerilog.AST

runBugpoint :: [String] -> ([AST] -> IO [AST]) -> [AST] -> IO [AST]
runBugpoint expected converter =
    fmap pure . runBugpoint' expected converter' . concat
    where
        converter' :: AST -> IO AST
        converter' = fmap concat . converter . pure

runBugpoint' :: [String] -> (AST -> IO AST) -> AST -> IO AST
runBugpoint' expected converter ast = do
    ast' <- runBugpointPass expected converter ast
    if ast == ast'
        then out "done minimizing" >> return ast
        else runBugpoint' expected converter ast'

out :: String -> IO ()
out = hPutStrLn stderr . ("bugpoint: " ++)

-- run the given converter and return the conversion failure if any or the
-- converted output otherwise
extractConversionResult :: (AST -> IO AST) -> AST -> IO String
extractConversionResult converter asts =
    catches runner
        [ Handler handleErrorCall
        , Handler handlePatternMatchFail
        ]
    where
        runner = converter asts <&> show
        handleErrorCall (ErrorCall str) = return str
        handlePatternMatchFail (PatternMatchFail str) = return str

runBugpointPass :: [String] -> (AST -> IO AST) -> AST -> IO AST
runBugpointPass expected converter ast = do
    out $ "beginning pass with " ++ (show $ length $ show ast) ++ " characters"
    matches <- oracle ast
    when (not matches) $
        out ("doesn't match expected strings: " ++ show expected) >> exitFailure
    let ast' = concat $ Convert.RemoveComments.convert [ast]
    matches' <- oracle ast'
    minimizeContainer oracle minimizeDescription "<design>" id $
        if matches' then ast' else ast
    where
        oracle :: AST -> IO Bool
        oracle = fmap check . extractConversionResult converter
        check :: String -> Bool
        check = flip all expected . flip isInfixOf

type Oracle t = t -> IO Bool
type Minimizer t = Oracle t -> t -> IO t

-- given a subsequence-verifying oracle, a strategy for minimizing within
-- elements of the sequence, a name for debugging, a constructor for the
-- container, and the elements within the container, produce a minimized version
-- of the container
minimizeContainer :: forall a b. (Show a, Show b)
    => Oracle a -> Minimizer b -> String -> ([b] -> a) -> [b] -> IO a
minimizeContainer oracle minimizer name constructor =
    stepFilter 0 [] >=>
    stepRecurse [] >=>
    return . constructor
    where
        oracle' :: Oracle [b]
        oracle' = oracle . constructor

        stepFilter :: Int -> [b] -> [b] -> IO [b]
        stepFilter 0 [] pending = stepFilter (length pending) [] pending
        stepFilter 1 need [] = return need
        stepFilter width need [] =
            stepFilter (max 1 $ width `div` 4) [] need
        stepFilter width need pending = do
            matches <- oracle' $ need ++ rest
            if matches
                then out msg >> stepFilter width need rest
                else stepFilter width (need ++ curr) rest
            where
                (curr, rest) = splitAt width pending
                msg = "removed " ++ show (length curr) ++ " items from " ++ name

        stepRecurse :: [b] -> [b] -> IO [b]
        stepRecurse before [] = return before
        stepRecurse before (isolated : after) = do
            isolated' <- minimizer oracleRecurse isolated
            stepRecurse (before ++ [isolated']) after
            where oracleRecurse = (oracle' $) . (before ++) . (: after)

minimizeDescription :: Minimizer Description
minimizeDescription oracle (Package lifetime name items) =
    minimizeContainer oracle (const return) name constructor items
    where constructor = Package lifetime name
minimizeDescription oracle (Part att ext kw lif name ports items) =
    minimizeContainer oracle minimizeModuleItem name constructor items
    where constructor = Part att ext kw lif name ports
minimizeDescription _ other = return other

minimizeModuleItem :: Minimizer ModuleItem
minimizeModuleItem oracle (Generate items) =
    minimizeContainer oracle minimizeGenItem "<generate>" Generate items
minimizeModuleItem _ item = return item

minimizeGenItem :: Minimizer GenItem
minimizeGenItem _ GenNull = return GenNull
minimizeGenItem oracle item = do
    matches <- oracle GenNull
    if matches
        then out "removed generate item" >> return GenNull
        else minimizeGenItem' oracle item

minimizeGenItem' :: Minimizer GenItem
minimizeGenItem' oracle (GenModuleItem item) =
    minimizeModuleItem (oracle . GenModuleItem) item <&> GenModuleItem
minimizeGenItem' oracle (GenIf c t f) = do
    t' <- minimizeGenItem (oracle . flip (GenIf c) f) t
    f' <- minimizeGenItem (oracle . GenIf c t') f
    return $ GenIf c t' f'
minimizeGenItem' _ (GenBlock _ []) = return GenNull
minimizeGenItem' oracle (GenBlock name items) =
    minimizeContainer oracle minimizeGenItem' name constructor items
    where constructor = GenBlock name
minimizeGenItem' oracle (GenFor a b c item) =
    minimizeGenItem (oracle . constructor) item <&> constructor
    where constructor = GenFor a b c
minimizeGenItem' oracle (GenCase expr cases) =
    minimizeContainer oracle minimizeGenCase "<case>" constructor cases
    where constructor = GenCase expr
minimizeGenItem' _ GenNull = return GenNull

minimizeGenCase :: Minimizer GenCase
minimizeGenCase oracle (exprs, item) =
    minimizeGenItem' (oracle . constructor) item <&> constructor
    where constructor = (exprs,)
