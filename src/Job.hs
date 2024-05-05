{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Command line arguments.
 -}

module Job where

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Version (showVersion)
#if MIN_VERSION_githash(0,1,5)
import GitHash (giTag, tGitInfoCwdTry)
#endif
import qualified Paths_sv2v (version)
import System.IO (stderr, hPutStr)
import System.Console.CmdArgs
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)

data Exclude
    = Always
    | Assert
    | Interface
    | Logic
    | Succinct
    | UnbasedUnsized
    deriving (Typeable, Data, Eq)

data Write
    = Stdout
    | Adjacent
    | File FilePath
    | Directory FilePath
    deriving (Typeable, Data)

data Job = Job
    { files :: [FilePath]
    , incdir :: [FilePath]
    , libdir :: [FilePath]
    , define :: [String]
    , siloed :: Bool
    , skipPreprocessor :: Bool
    , passThrough :: Bool
    , exclude :: [Exclude]
    , verbose :: Bool
    , write :: Write
    , writeRaw :: String
    , top :: [String]
    , oversizedNumbers :: Bool
    , dumpPrefix :: FilePath
    } deriving (Typeable, Data)

version :: String
#if MIN_VERSION_githash(0,1,5)
version = either (const backup) giTag $$tGitInfoCwdTry
#else
version = backup
#endif
    where backup = showVersion Paths_sv2v.version

defaultJob :: Job
defaultJob = Job
    { files = def &= args &= typ "FILES"
    , incdir = nam_ "I" &= name "incdir" &= typDir
        &= help "Add a directory to the include search path"
        &= groupname "Preprocessing"
    , libdir = nam_ "y" &= name "libdir" &= typDir
        &= help ("Add a directory to the library search path used when looking"
            ++ " for undefined modules and interfaces")
    , define = nam_ "D" &= name "define" &= typ "NAME[=VALUE]"
        &= help "Define a macro for preprocessing"
    , siloed = nam_ "siloed" &= help ("Lex input files separately, so"
        ++ " macros from earlier files are not defined in later files")
    , skipPreprocessor = nam_ "skip-preprocessor"
        &= help "Disable preprocessing of macros, comments, etc."
    , passThrough = nam_ "pass-through" &= help "Dump input without converting"
        &= groupname "Conversion"
    , exclude = nam_ "exclude" &= name "E" &= typ "CONV"
        &= help ("Exclude a particular conversion (Always, Assert, Interface,"
            ++ " Logic, or UnbasedUnsized)")
    , verbose = nam "verbose" &= help "Retain certain conversion artifacts"
    , write = Stdout &= ignore -- parsed from the flexible flag below
    , writeRaw = "s" &= name "write" &= name "w" &= explicit
        &= typ "MODE/FILE/DIR"
        &= help ("How to write output; default is 'stdout'; use 'adjacent' to"
            ++ " create a .v file next to each input; use a path ending in .v"
            ++ " to write to a file; use a path to an existing directory to"
            ++ " create a .v within for each converted module")
    , top = def &= name "top" &= explicit &= typ "NAME"
        &= help ("Remove uninstantiated modules except the given top module;"
            ++ " can be used multiple times")
    , oversizedNumbers = nam_ "oversized-numbers"
        &= help ("Disable standard-imposed 32-bit limit on unsized number"
            ++ " literals (e.g., 'h1_ffff_ffff, 4294967296)")
        &= groupname "Other"
    , dumpPrefix = def &= name "dump-prefix" &= explicit &= typ "PATH"
        &= help ("Create intermediate output files with the given path prefix;"
            ++ " used for internal debugging")
    }
    &= program "sv2v"
    &= summary ("sv2v " ++ version)
    &= details [ "sv2v converts SystemVerilog to Verilog."
               , "More info: https://github.com/zachjs/sv2v"
               , "(C) 2019-2024 Zachary Snow, 2011-2015 Tom Hawkins" ]
    &= helpArg [explicit, name "help", help "Display this help message"]
    &= versionArg [explicit, name "version"]
    &= verbosityArgs [ignore] [ignore]
    where
        -- borrowed from: https://github.com/ndmitchell/hlint
        nam xs = nam_ xs &= name [head xs]
        nam_ xs = def &= name xs &= explicit

parseWrite :: String -> IO Write
parseWrite w | w `matches` "stdout" = return Stdout
parseWrite w | w `matches` "adjacent" = return Adjacent
parseWrite w | ".v" `isSuffixOf` w = return $ File w
parseWrite w | otherwise = do
    isDir <- doesDirectoryExist w
    when (not isDir) $ do
        hPutStr stderr $ "invalid --write " ++ show w ++ ", expected stdout,"
            ++ " adjacent, a path ending in .v, or a path to an existing"
            ++ " directory"
        exitFailure
    return $ Directory w

matches :: String -> String -> Bool
matches = isPrefixOf . map toLower

readJob :: IO Job
readJob =
    cmdArgs defaultJob
        >>= setWrite . setSuccinct

setWrite :: Job -> IO Job
setWrite job = do
    w <- parseWrite $ writeRaw job
    case (w, passThrough job) of
        (Directory{}, True) -> do
            hPutStr stderr "can't use --pass-through when writing to a dir"
            exitFailure
        _ -> return $ job { write = w }

setSuccinct :: Job -> Job
setSuccinct job | verbose job = job { exclude = Succinct : exclude job }
setSuccinct job | otherwise = job
