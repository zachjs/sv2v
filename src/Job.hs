{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Command line arguments.
 -}

module Job where

import GitHash (giDescribe, tGitInfoCwd)
import System.IO (stderr, hPutStr)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)

data Exclude
    = Always
    | Assert
    | Interface
    | Logic
    | Succinct
    deriving (Show, Typeable, Data, Eq)

data Job = Job
    { files :: [FilePath]
    , incdir :: [FilePath]
    , define :: [String]
    , siloed :: Bool
    , skipPreprocessor :: Bool
    , exclude :: [Exclude]
    , verbose :: Bool
    } deriving (Show, Typeable, Data)

defaultJob :: Job
defaultJob = Job
    { files = def &= args &= typ "FILES"
    , incdir = nam_ "I" &= name "incdir" &= typDir
        &= help "Add directory to include search path"
        &= groupname "Preprocessing"
    , define = nam_ "D" &= name "define" &= typ "NAME[=VALUE]"
        &= help "Define a macro for preprocessing"
    , siloed = nam_ "siloed" &= help ("Lex input files separately, so"
        ++ " macros from earlier files are not defined in later files")
    , skipPreprocessor = nam_ "skip-preprocessor" &= help "Disable preprocessor"
    , exclude = nam_ "exclude" &= name "E" &= typ "CONV"
        &= help ("Exclude a particular conversion (always, assert, interface,"
            ++ " or logic)")
        &= groupname "Conversion"
    , verbose = nam "verbose" &= help "Retain certain conversion artifacts"
    }
    &= program "sv2v"
    &= summary ("sv2v " ++ giDescribe $$tGitInfoCwd)
    &= details [ "sv2v converts SystemVerilog to Verilog."
               , "More info: https://github.com/zachjs/sv2v"
               , "(C) 2019-2020 Zachary Snow, 2011-2015 Tom Hawkins" ]
    &= helpArg [explicit, name "help", groupname "Other"]
    &= versionArg [explicit, name "version"]
    &= verbosityArgs [ignore] [ignore]
    where
        -- borrowed from: https://github.com/ndmitchell/hlint
        nam xs = nam_ xs &= name [head xs]
        nam_ xs = def &= name xs &= explicit

type DeprecationPhase = [String] -> IO [String]

oneunit :: DeprecationPhase
oneunit strs = do
    let strs' = filter (not . isOneunitArg) strs
    if strs == strs'
        then return strs
        else do
            hPutStr stderr $ "Deprecation warning: --oneunit has been removed, "
                ++ "and is now on by default\n"
            return strs'
    where
        isOneunitArg :: String -> Bool
        isOneunitArg "-o" = True
        isOneunitArg "--oneunit" = True
        isOneunitArg _ = False

flagRename :: String -> String -> DeprecationPhase
flagRename before after strs = do
    let strs' = map rename strs
    if strs == strs'
        then return strs
        else do
            hPutStr stderr $ "Deprecation warning: " ++ before ++
                " has been renamed to " ++ after ++ "\n"
            return strs'
    where
        rename :: String -> String
        rename arg =
            if before == take (length before) arg
                then after ++ drop (length before) arg
                else arg

readJob :: IO Job
readJob = do
    strs <- getArgs
    strs' <- oneunit strs
        >>= flagRename "-i" "-I"
        >>= flagRename "-d" "-D"
        >>= flagRename "-e" "-E"
        >>= flagRename "-V" "--version"
        >>= flagRename "-?" "--help"
    job <- withArgs (strs') $ cmdArgs defaultJob
    return $ if verbose job
        then job { exclude = Succinct : exclude job }
        else job
