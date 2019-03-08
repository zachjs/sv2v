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
    , file :: FilePath
    } deriving (Show, Typeable, Data)

defaultJob :: Job
defaultJob = Job
    { exclude = [] &= typ "CONV"
        &= help
            ("conversion to exclude (always, interface, logic)"
            ++ "; can be specified multiple times")
    , file = def &= args &= typFile
    }
    &= program "sv2v"
    &= summary "sv2v v0.0.1, (C) Zachary Snow 2019, Tom Hawkins, 2011-2015"
    &= details [ "sv2v converts SystemVerilog to Verilog."
               , "More info: https://github.com/zachjs/sv2v" ]

readJob :: IO Job
readJob = cmdArgs defaultJob
