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
    deriving (Show, Typeable, Data, Eq)

data Job = Job
    { exclude :: [Exclude]
    , files :: [FilePath]
    , incdir :: [FilePath]
    , define :: [String]
    } deriving (Show, Typeable, Data)

defaultJob :: Job
defaultJob = Job
    { exclude = [] &= typ "CONV"
        &= help "exclude a particular conversion (always, interface, logic)"
    , files = def &= args &= typ "FILES"
    , incdir = def &= typDir &= help "add directory to include search path"
    , define = def &= typ "NAME[=VALUE]" &= help "define a macro for preprocessing"
    }
    &= program "sv2v"
    &= summary "sv2v v0.0.1, (C) Zachary Snow 2019, Tom Hawkins, 2011-2015"
    &= details [ "sv2v converts SystemVerilog to Verilog."
               , "More info: https://github.com/zachjs/sv2v" ]

readJob :: IO Job
readJob = cmdArgs defaultJob
