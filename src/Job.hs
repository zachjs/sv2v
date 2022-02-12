{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Command line arguments.
 -}

module Job where

import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Version (showVersion)
import GitHash (giDescribe, tGitInfoCwdTry)
import qualified Paths_sv2v (version)
import System.IO (stderr, hPutStr)
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
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
    deriving (Typeable, Data)

data Job = Job
    { files :: [FilePath]
    , incdir :: [FilePath]
    , define :: [String]
    , siloed :: Bool
    , skipPreprocessor :: Bool
    , passThrough :: Bool
    , exclude :: [Exclude]
    , verbose :: Bool
    , write :: Write
    , writeRaw :: String
    , oversizedNumbers :: Bool
    , dumpPrefix :: FilePath
    } deriving (Typeable, Data)

version :: String
version = either (const backup) giDescribe $$tGitInfoCwdTry
    where backup = showVersion Paths_sv2v.version

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
    , passThrough = nam_ "pass-through" &= help "Dump input without converting"
        &= groupname "Conversion"
    , exclude = nam_ "exclude" &= name "E" &= typ "CONV"
        &= help ("Exclude a particular conversion (Always, Assert, Interface,"
            ++ " Logic, or UnbasedUnsized)")
    , verbose = nam "verbose" &= help "Retain certain conversion artifacts"
    , write = Stdout &= ignore -- parsed from the flexible flag below
    , writeRaw = "s" &= name "write" &= name "w" &= explicit &= typ "MODE/FILE"
        &= help ("How to write output; default is 'stdout'; use 'adjacent' to"
            ++ " create a .v file next to each input; use a path ending in .v"
            ++ " to write to a file")
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
               , "(C) 2019-2022 Zachary Snow, 2011-2015 Tom Hawkins" ]
    &= helpArg [explicit, name "help"]
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
    hPutStr stderr $ "invalid --write " ++ show w
        ++ ", expected stdout, adjacent, or a path ending in .v"
    exitFailure

matches :: String -> String -> Bool
matches = isPrefixOf . map toLower

type DeprecationPhase = [String] -> IO [String]

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
    strs' <- return strs
        >>= flagRename "-i" "-I"
        >>= flagRename "-d" "-D"
        >>= flagRename "-e" "-E"
    withArgs strs' $ cmdArgs defaultJob
        >>= setWrite . setSuccinct
    where
        setWrite :: Job -> IO Job
        setWrite job = do
            w <- parseWrite $ writeRaw job
            return $ job { write = w }
        setSuccinct :: Job -> Job
        setSuccinct job | verbose job = job { exclude = Succinct : exclude job }
        setSuccinct job | otherwise = job
