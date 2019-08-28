{-# LANGUAGE DeriveDataTypeable #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Command line arguments.
 -}

module Job where

import System.Console.CmdArgs

data Exclude
    = Always
    | Interface
    | Logic
    | Succinct
    deriving (Show, Typeable, Data, Eq)

data Job = Job
    { exclude :: [Exclude]
    , files :: [FilePath]
    , incdir :: [FilePath]
    , define :: [String]
    , oneunit :: Bool
    , verbose :: Bool
    } deriving (Show, Typeable, Data)

defaultJob :: Job
defaultJob = Job
    { exclude = [] &= typ "CONV"
        &= help "exclude a particular conversion (always, interface, or logic)"
    , files = def &= args &= typ "FILES"
    , incdir = def &= typDir &= help "add directory to include search path"
    , define = def &= typ "NAME[=VALUE]" &= help ("define a macro for"
        ++ " preprocessing")
    , oneunit = False &= help ("put all files in one compilation unit, so"
        ++ " macros from earlier files remain defined in later files")
    , verbose = False &= help "retain certain conversion artifacts"
    }
    &= program "sv2v"
    &= summary "sv2v v0.0.1, (C) 2019 Zachary Snow, 2011-2015 Tom Hawkins"
    &= details [ "sv2v converts SystemVerilog to Verilog."
               , "More info: https://github.com/zachjs/sv2v" ]

readJob :: IO Job
readJob = do
    job <- cmdArgs defaultJob
    return $ if verbose job
        then job { exclude = Succinct : exclude job }
        else job
