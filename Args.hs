{-# LANGUAGE DeriveDataTypeable #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Command line arguments.
 -}

module Args where

import System.Console.CmdArgs

data Target = VTR | YOSYS
    deriving (Show, Typeable, Data)

data Job = Job
    { target :: Target
    , file :: FilePath
    } deriving (Show, Typeable, Data)

defaultJob :: Job
defaultJob = Job
    { target = YOSYS &= typ "TARGET"
        &= help "target sythesizer (yosys, vtr; defaults to yosys)"
    , file = def &= args &= typFile
    }
    &= program "sv2v"
    &= summary "sv2v v0.0.1, (C) Zachary Snow 2019, Tom Hawkins, 2011-2015"
    &= details [ "sv2v converts SystemVerilog to Verilog."
               , "More info: https://github.com/zachjs/sv2v" ]

readArgs :: IO Job
readArgs = cmdArgs defaultJob
