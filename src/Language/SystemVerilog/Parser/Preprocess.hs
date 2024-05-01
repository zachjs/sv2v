{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog Preprocessor
 -
 - This preprocessor handles all preprocessor directives and produces an output
 - stream that is tagged with the effective source position of resulting
 - characters.
 -}
module Language.SystemVerilog.Parser.Preprocess
    ( preprocess
    , annotate
    , Env
    , Contents
    ) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char (ord)
import Data.List (tails, isPrefixOf, findIndex, intercalate)
import Data.Maybe (isJust, fromJust)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.Directory (findFile)
import System.FilePath (dropFileName)
import System.IO (hGetContents, hSetEncoding, openFile, stdin, IOMode(ReadMode))
import qualified Data.Map.Strict as Map

import Language.SystemVerilog.Parser.Tokens (Position(..))

type Env = Map.Map String (String, [(String, Maybe String)])
type Contents = [(Char, Position)]

type PPS = StateT PP (ExceptT String IO)

data PP = PP
    { ppInput        :: String -- current input string
    , ppOutput       :: [(Char, Position)] -- preprocessor output (in reverse)
    , ppPosition     :: Position -- current file position
    , ppFilePath     :: FilePath -- currently active filename
    , ppEnv          :: Env -- active macro definitions
    , ppCondStack    :: [Level] -- if-else cascade state
    , ppIncludePaths :: [FilePath] -- folders to search for includes
    , ppMacroStack   :: [[(String, String)]] -- arguments for in-progress macro expansions
    , ppIncludeStack :: [(FilePath, Env)] -- in-progress includes for loop detection
    }

-- if-else cascade level state and error information
data Level = Level
    { cfDesc :: String -- text of the directive, e.g., "ifdef FOO"
    , cfPos :: Position -- location where this level started
    , cfCond :: Cond -- whether or not this level is as has been satisfied
    }

-- keeps track of the state of an if-else cascade level
data Cond
    = CurrentlyTrue -- an active if/elsif/else branch (condition is met)
    | PreviouslyTrue -- an inactive else/elsif block due to an earlier if/elsif
    | NeverTrue -- an inactive if/elsif block; a subsequent else will be met
    deriving Eq

-- update a Cond for an `else block, where this block is active if and only if
-- no previous block was active
elseCond :: Cond -> Cond
elseCond NeverTrue = CurrentlyTrue
elseCond _ = NeverTrue

-- generate a Cond for an `if/`elsif that is not part of a PreviouslyTrue chain
ifCond :: Bool -> Cond
ifCond True = CurrentlyTrue
ifCond False = NeverTrue

-- update a Cond for an `elsif block. The boolean argument is whether the
-- `elsif block's test is true.
elsifCond :: Bool -> Cond -> Cond
elsifCond defined c =
    case c of
        NeverTrue -> ifCond defined
        _ -> PreviouslyTrue

-- preprocessor entrypoint
preprocess :: [String] -> Env -> FilePath -> ExceptT String IO (Env, Contents)
preprocess includePaths env path = do
    contents <- liftIO $ loadFile path
    let initialState = PP
            { ppInput        = contents
            , ppOutput       = []
            , ppPosition     = Position path 1 1
            , ppFilePath     = path
            , ppEnv          = env
            , ppCondStack    = []
            , ppIncludePaths = includePaths
            , ppMacroStack   = []
            , ppIncludeStack = [(path, env)]
            }
    finalState <- execStateT (preprocessInput >> checkConds) initialState
    let env' = ppEnv finalState
    let output = reverse $ ppOutput finalState
    return (env', output)

checkConds :: PPS ()
checkConds = do
    condStack <- getCondStack
    when (not $ null condStack) $ do
        let level = head condStack
        lexicalError $ "unfinished conditional directive `" ++ cfDesc level ++
            " started at " ++ show (cfPos level)

-- position annotator entrypoint used for files that don't need any
-- preprocessing
annotate :: FilePath -> ExceptT String IO Contents
annotate path = do
    contents <- liftIO $ loadFile path
    let positions = scanl advance (Position path 1 1) contents
    return $ zip contents positions

-- read in the given file
loadFile :: FilePath -> IO String
loadFile path = do
    handle <-
        if path == "-"
            then return stdin
            else openFile path ReadMode
    hSetEncoding handle $ mkUTF8 TransliterateCodingFailure
    contents <- hGetContents handle
    return $ normalize contents

-- removes carriage returns before newlines
normalize :: String -> String
normalize ('\r' : '\n' : rest) = '\n' : (normalize rest)
normalize (ch : chs) = ch : (normalize chs)
normalize [] = []

-- find the given file for inclusion
includeSearch :: FilePath -> PPS FilePath
includeSearch file = do
    base <- getFilePath
    includePaths <- gets ppIncludePaths
    let directories = dropFileName base : includePaths
    result <- liftIO $ findFile directories file
    case result of
        Just path -> return path
        Nothing -> lexicalError $ "Could not find file " ++ show file ++
                        ", included from " ++ show base

lexicalError :: String -> PPS a
lexicalError msg = do
    pos <- getPosition
    lift $ throwError $ show pos ++ ": Lexical error: " ++ msg

-- input accessors
setInput :: String -> PPS ()
setInput x = modify $ \s -> s { ppInput = x }
getInput :: PPS String
getInput = gets ppInput
-- output accessors
setOutput :: [(Char, Position)] -> PPS ()
setOutput x = modify $ \s -> s { ppOutput = x }
getOutput :: PPS [(Char, Position)]
getOutput = gets ppOutput
-- position accessors
getPosition :: PPS Position
getPosition = gets ppPosition
setPosition :: Position -> PPS ()
setPosition x = modify $ \s -> s { ppPosition = x }
-- file path accessors
getFilePath :: PPS FilePath
getFilePath = gets ppFilePath
setFilePath :: String -> PPS ()
setFilePath x = modify $ \s -> s { ppFilePath = x }
-- environment accessors
getEnv :: PPS Env
getEnv = gets ppEnv
setEnv :: Env -> PPS ()
setEnv x = modify $ \s -> s { ppEnv = x }
-- cond stack accessors
getCondStack :: PPS [Level]
getCondStack = gets ppCondStack
setCondStack :: [Level] -> PPS ()
setCondStack x = modify $ \s -> s { ppCondStack = x }
-- macro stack accessors
getMacroStack :: PPS [[(String, String)]]
getMacroStack = gets ppMacroStack
setMacroStack :: [[(String, String)]] -> PPS ()
setMacroStack x = modify $ \s -> s { ppMacroStack = x }
-- combined input and position accessors
setBuffer :: (String, Position) -> PPS ()
setBuffer (x, p) = do
    setInput x
    setPosition p
getBuffer :: PPS (String, Position)
getBuffer = do
    x <- getInput
    p <- getPosition
    return (x, p)

-- mark the start of an include for include loop detection
pushIncludeStack :: FilePath -> PPS ()
pushIncludeStack path = do
    stack <- gets ppIncludeStack
    env <- gets ppEnv
    let entry = (path, env)
    let stack' = entry : stack
    if elem entry stack then do
        let first : rest = reverse $ map fst stack'
        lexicalError $ "include loop: " ++ show first ++ " includes "
            ++ intercalate ", which includes " (map show rest)
    else
        modify $ \s -> s { ppIncludeStack = stack' }

-- mark the end of an include for include loop detection
popIncludeStack :: PPS ()
popIncludeStack = do
    stack <- gets ppIncludeStack
    let stack' = tail stack
    modify $ \s -> s { ppIncludeStack = stack' }

-- Push a condition onto the top of the preprocessor condition stack
pushCondStack :: String -> String -> Position -> Cond -> PPS ()
pushCondStack typ name pos cond =
    getCondStack >>= setCondStack . (level :)
    where
        level = Level { cfDesc = desc, cfPos = pos, cfCond = cond }
        desc = typ ++ if null name then "" else " " ++ name

-- Pop the top from the preprocessor condition stack
popCondStack :: String -> PPS Level
popCondStack directive = do
    cs <- getCondStack
    case cs of
        [] -> lexicalError $
                "`" ++ directive ++ " directive outside of an `if/`endif block"
        c : cs' -> setCondStack cs' >> return c

isIdentChar :: Char -> Bool
isIdentChar ch =
    ('a' <= ch && ch <= 'z') ||
    ('A' <= ch && ch <= 'Z') ||
    ('0' <= ch && ch <= '9') ||
    (ch == '_') || (ch == '$')

-- reads an identifier from the front of the input
takeIdentifier :: PPS String
takeIdentifier = do
    str <- getInput
    macroStack <- getMacroStack
    if null macroStack
        then do
            let (ident, rest) = span isIdentChar str
            advancePositions ident
            setInput rest
            return ident
        else takeIdentifierFollow True
takeIdentifierFollow :: Bool -> PPS String
takeIdentifierFollow firstPass = do
    str <- getInput
    case str of
        '`' : '`' : '`' : _ -> do
            '`' <- takeChar
            '`' <- takeChar
            process $ handleDirective True
        '`' : '`' : _ -> do
            '`' <- takeChar
            '`' <- takeChar
            process consumeWithSubstitution
        _ -> if firstPass
                then process consumeWithSubstitution
                else return ""
    where
        process :: (PPS ()) -> PPS String
        process action = do
            -- save the state
            outputOrig <- getOutput
            condStackOrig <- getCondStack
            -- process this chunk of the identifier
            setOutput []
            setCondStack []
            () <- action
            outputIdent <- getOutput
            -- restore the previous state
            setOutput outputOrig
            setCondStack condStackOrig
            -- move on to the next chunk
            let ident = reverse $ map fst outputIdent
            identFollow <- takeIdentifierFollow False
            return $ ident ++ identFollow

-- read tokens after the name until the first (un-escaped) newline
takeUntilNewline :: PPS String
takeUntilNewline = do
    str <- getInput
    case str of
        []                 -> return ""
        '\n' :        _    -> do
            return ""
        '/' : '/' : _ -> do
            remainder <- takeThrough '\n'
            case last $ init remainder of
                '\\' -> takeUntilNewline >>= return . (' ' :)
                _ -> return ""
        '\\' : '\n' : rest -> do
            advancePosition '\\'
            advancePosition '\n'
            setInput rest
            takeUntilNewline >>= return . (' ' :)
        ch   :        rest -> do
            advancePosition ch
            setInput rest
            takeUntilNewline >>= return . (ch :)

-- select characters up to and including the given character
takeThrough :: Char -> PPS String
takeThrough goal = do
    str <- getInput
    if null str
        then lexicalError $ "unexpected end of input, looking for " ++ show goal
        else do
            ch <- takeChar
            if ch == goal
                then return [ch]
                else do
                    rest <- takeThrough goal
                    return $ ch : rest

-- pop one character from the input stream
takeChar :: PPS Char
takeChar = do
    str <- getInput
    (ch, chs) <-
        if null str
            then lexicalError "unexpected end of input"
            else return (head str, tail str)
    advancePosition ch
    setInput chs
    return ch

-- removes and returns a quoted string such as <foo.bar> or "foo.bar"
takeQuotedString :: PPS String
takeQuotedString = do
    dropSpaces
    ch <- takeChar
    end <-
        case ch of
            '"' -> return '"'
            '<' -> return '>'
            _ -> lexicalError $ "bad beginning of include arg: " ++ (show ch)
    rest <- takeThrough end
    let res = ch : rest
    return res

-- removes and returns a decimal number
takeNumber :: PPS Int
takeNumber = do
    dropSpaces
    leadCh <- peekChar
    if '0' <= leadCh && leadCh <= '9'
        then step 0
        else lexicalError $ "expected number, but found unexpected char: "
                ++ show leadCh
    where
        step number = do
            ch <- peekChar
            if ch == ' ' || ch == '\n' then
                return number
            else if '0' <= ch && ch <= '9' then do
                _ <- takeChar
                let digit = ord ch - ord '0'
                step $ number * 10 + digit
            else
                lexicalError $ "unexpected char while reading number: "
                    ++ show ch

peekChar :: PPS Char
peekChar = do
    str <- getInput
    if null str
        then lexicalError "unexpected end of input"
        else return $ head str

takeMacroDefinition :: PPS (String, [(String, Maybe String)])
takeMacroDefinition = do
    leadCh <- peekChar
    if leadCh /= '('
        then do
            dropSpaces
            body <- takeUntilNewline
            return (body, [])
        else do
            args <- takeMacroArguments
            dropSpaces
            body <- takeUntilNewline
            argsWithDefaults <- mapM splitArg args
            return (body, argsWithDefaults)
    where
        splitArg :: String -> PPS (String, Maybe String)
        splitArg [] = lexicalError "macro definition missing argument name"
        splitArg str =
            if null name then
                lexicalError $ "invalid macro definition argument: " ++ show str
            else if null rest then
                return (name, Nothing)
            else if leadCh /= '=' then
                lexicalError $ "bad char after argument name: " ++ show leadCh
            else
                return (name, Just value)
            where
                (name, rest) = span isIdentChar str
                leadCh : after = dropWhile isWhitespaceChar rest
                value = dropWhile isWhitespaceChar after

-- commas and right parens are forbidden outside matched pairs of: (), [], {},
-- "", except to delimit arguments or end the list of arguments; see 22.5.1
takeMacroArguments :: PPS [String]
takeMacroArguments = do
    dropWhitespace
    leadCh <- takeChar
    if leadCh == '('
        then argLoop >>= mapM preprocessString
        else lexicalError $ "expected beginning of macro arguments, but found "
                ++ show leadCh
    where
        argLoop :: PPS [String]
        argLoop = do
            dropWhitespace
            (argRev, isEnd) <- loop "" []
            let arg = trimAndRev argRev
            if isEnd
                then return [arg]
                else do
                    rest <- argLoop
                    return $ arg : rest
        loop :: String -> [Char] -> PPS (String, Bool)
        loop curr stack = do
            ch <- takeChar
            case (stack, ch) of
                ([     ], ',') -> return (curr, False)
                ([     ], ')') -> return (curr, True)

                -- simple quoted strings, allowing escaped quotes
                ('\\': s, _  ) -> loop (ch : curr) s
                ('"' : s, '"') -> loop (ch : curr) s
                ('"' : _,'\\') -> loop (ch : curr) ('\\': stack)
                ('"' : _, _  ) -> loop (ch : curr) stack
                (      _, '"') -> loop (ch : curr) ('"' : stack)

                ('[' : s, ']') -> loop (ch : curr) s
                (      s, '[') -> loop (ch : curr) ('[' : s)
                ('(' : s, ')') -> loop (ch : curr) s
                (      s, '(') -> loop (ch : curr) ('(' : s)
                ('{' : s, '}') -> loop (ch : curr) s
                (      s, '{') -> loop (ch : curr) ('{' : s)

                (      s, '/') -> do
                    next <- peekChar
                    case next of
                        '/' -> takeChar >> dropLineComment >> loop curr s
                        '*' -> takeChar >> dropBlockComment >> loop curr s
                        _ -> loop ('/' : curr) s

                (      s,'\n') -> loop (' ' : curr) s
                (      s, _  ) -> loop (ch  : curr) s

        trimAndRev = -- drop surrounding whitespace and reverse string
            dropWhile isWhitespaceChar . reverse . dropWhile isWhitespaceChar

        dropLineComment :: PPS ()
        dropLineComment = do
            ch <- takeChar
            when (ch /= '\n') dropLineComment

        dropBlockComment :: PPS ()
        dropBlockComment = do
            ch1 <- takeChar
            ch2 <- peekChar
            if ch1 == '*' && ch2 == '/'
                then takeChar >> return ()
                else dropBlockComment

defaultMacroArgs :: [Maybe String] -> [String] -> PPS [String]
defaultMacroArgs [] [] = return []
defaultMacroArgs [] _ = lexicalError "too many macro arguments given"
defaultMacroArgs defaults [] = do
    if all isJust defaults
        then return $ map fromJust defaults
        else lexicalError "too few macro arguments given"
defaultMacroArgs (f : fs) (a : as) = do
    let arg = if a == "" && isJust f
            then fromJust f
            else a
    args <- defaultMacroArgs fs as
    return $ arg : args

-- drop spaces in the input until a non-space is reached or EOF
dropSpaces :: PPS ()
dropSpaces = do
    str <- getInput
    if null str then
        return ()
    else do
        let ch : rest = str
        if ch == '\t' || ch == ' ' then do
            advancePosition ch
            setInput rest
            dropSpaces
        else
            return ()

isWhitespaceChar :: Char -> Bool
isWhitespaceChar ch = elem ch [' ', '\t', '\n']

-- drop all leading whitespace in the input
dropWhitespace :: PPS ()
dropWhitespace = do
    str <- getInput
    case str of
        ch : chs ->
            when (isWhitespaceChar ch) $ do
                advancePosition ch
                setInput chs
                dropWhitespace
        [] -> return ()

-- directives that must always be processed even if the current code block is
-- being excluded; we have to process conditions so we can match them up with
-- their ending tag, even if they're being skipped
unskippableDirectives :: [String]
unskippableDirectives = ["else", "elsif", "endif", "ifdef", "ifndef"]

-- list of all of the supported directive names; used to prevent defining macros
-- with illegal names
directives :: [String]
directives =
    [ "timescale"
    , "celldefine"
    , "endcelldefine"
    , "unconnected_drive"
    , "nounconnected_drive"
    , "default_nettype"
    , "pragma"
    , "resetall"
    , "begin_keywords"
    , "end_keywords"
    , "__FILE__"
    , "__LINE__"
    , "line"
    , "include"
    , "ifdef"
    , "ifndef"
    , "else"
    , "elsif"
    , "endif"
    , "define"
    , "undef"
    , "undefineall"
    ]

-- primary preprocessor loop
preprocessInput :: PPS ()
preprocessInput = do
    str <- getInput
    macroStack <- getMacroStack
    case str of
        '/' : '/' : _ -> removeThrough "\n"
        '/' : '*' : _ ->
            -- prevent treating `/*/` as self-closing
            takeChar >> takeChar >>
            removeThrough "*/"
        '`' : '"' : _ -> handleBacktickString
        '"' : _ -> handleString
        '/' : '`' : '`' : '*' : _ ->
            if null macroStack
                then consume
                else removeThrough "*``/"
        '`' : '`' : _ -> do
            if null macroStack
                then do
                    consume
                    consume
                else do
                    '`' <- takeChar
                    '`' <- takeChar
                    return ()
        '`' : _ -> handleDirective False
        _ : _ -> do
            condStack <- map cfCond <$> getCondStack
            if null macroStack && all (== CurrentlyTrue) condStack
                then consumeMany
                else consumeWithSubstitution
        [] -> return ()
    if str == []
        then return ()
        else preprocessInput

-- if we are expanding a macro, and the leading tokens form an identifier, then
-- attempt to replace that identifier with the arguments of this macro, if
-- applicable; otherwise, just consume the top character
consumeWithSubstitution :: PPS ()
consumeWithSubstitution = do
    str <- getInput
    macroStack <- getMacroStack
    if null macroStack then
        consume
    else do
        let (ident, rest) = span isIdentChar str
        if null ident then
            consume
        else do
            pos <- getPosition
            let args = head macroStack
            let chars = case lookup ident args of
                            Nothing -> ident
                            Just val -> val
            pushChars chars pos
            advancePositions ident
            setInput rest

-- consume takes the lead input character and pushes it into the output,
-- advancing the position state and removing the lead character from the input
consume :: PPS ()
consume = do
    ch : chs <- getInput
    pos <- getPosition
    advancePosition ch
    setInput chs
    pushChar ch pos

-- consumeMany processes chars in a batch until a potential delimiter is reached
consumeMany :: PPS ()
consumeMany = do
    consume -- always consume first character
    (str, pos) <- getBuffer
    let (content, rest) = break (flip elem stopChars) str
    let positions = scanl advance pos content
    output <- getOutput
    setOutput $ (reverse $ zip content positions) ++ output
    setBuffer (rest, last positions)
    where stopChars = ['`', '"', '/']

-- preprocess a leading string literal; this routine is largely necessary to
-- avoid doing any macro or directive related manipulations within standard
-- string literals; it also handles escaped newlines in the string
handleString :: PPS ()
handleString = do
    consume
    loop
    where
        -- processes the remainder of a standard string literal
        loop :: PPS ()
        loop = do
            input <- getInput
            case input of
                '"' : _ -> do
                    consume
                    -- end of loop!
                '\\' : '\n' : _ -> do
                    '\\' <- takeChar
                    '\n' <- takeChar
                    loop
                '\\' : '\\' : _ -> do
                    consume
                    consume
                    loop
                '\\' : '"' : _ -> do
                    consume
                    consume
                    loop
                _ : _ -> do
                    consume
                    loop
                [] -> lexicalError "unterminated string literal"

-- preprocess a "backtick string", which begins and ends with a backtick
-- followed by a slash (`"), and within which macros can be invoked as normal;
-- otherwise, normal string literal rules apply, except that unescaped quotes
-- are forbidden, and backticks must be escaped using a backslash to avoid being
-- interpreted as a macro or marking the end of a string
handleBacktickString :: PPS ()
handleBacktickString = do
    '`' <- takeChar
    consume
    loop
    where
        -- processes the remainder of a leading backtick string, up to and
        -- including the ending `"
        loop :: PPS ()
        loop = do
            input <- getInput
            macroStack <- getMacroStack
            case input of
                '`' : '"' : _ -> do
                    '`' <- takeChar
                    consume -- ending quote
                    -- end of loop!
                '\\' : '`' : _ -> do
                    '\\' <- takeChar
                    consume -- now un-escaped backtick
                    loop
                '\\' : '\\' : _ -> do
                    consume
                    consume
                    loop
                '\\' : '"' : _ -> do
                    consume
                    consume
                    loop
                '\\' : '\n' : _ -> do
                    '\\' <- takeChar
                    '\n' <- takeChar
                    loop
                '`' : '\\' : '`' : '"' : _ -> do
                    '`' <- takeChar
                    consume
                    '`' <- takeChar
                    consume
                    if null macroStack
                        then lexicalError "`\\`\" is not allowed outside of macros"
                        else loop
                '`' : '`' : _ -> do
                    '`' <- takeChar
                    '`' <- takeChar
                    loop
                '`' : _ -> do
                    handleDirective True
                    loop
                '"' : _ ->
                    if null macroStack
                        then lexicalError "unescaped quote in backtick string"
                        else consume -- end of loop!
                _ : _ -> do
                    consumeWithSubstitution
                    loop
                [] -> lexicalError "unterminated backtick string"

handleDirective :: Bool -> PPS ()
handleDirective macrosOnly = do
    directivePos <- getPosition
    '`' <- takeChar
    directive <- takeIdentifier

    -- helper for directives which are not operated on
    let passThrough = do
            pushChar '`' directivePos
            pushChars directive directivePos

    env <- getEnv
    condStack <- map cfCond <$> getCondStack
    if any (/= CurrentlyTrue) condStack
        && not (elem directive unskippableDirectives) then
        return ()
    else if macrosOnly && elem directive directives then
        lexicalError "compiler directives are forbidden inside strings"
    else case directive of

        "timescale" -> removeThrough "\n"

        "celldefine" -> passThrough
        "endcelldefine" -> passThrough

        "unconnected_drive" -> passThrough
        "nounconnected_drive" -> passThrough

        "default_nettype" -> passThrough
        "pragma" -> do
            leadCh <- peekChar
            if leadCh == '\n'
                then lexicalError "pragma directive cannot be empty"
                else removeThrough "\n"
        "resetall" -> passThrough

        "begin_keywords" -> passThrough
        "end_keywords" -> passThrough

        "__FILE__" -> do
            currFile <- getFilePath
            insertChars directivePos (show currFile)
        "__LINE__" -> do
            Position _ currLine _ <- getPosition
            insertChars directivePos (show currLine)

        "line" -> do
            lineLookahead
            lineNumber <- takeNumber
            quotedFilename <- takeQuotedString
            levelNumber <- takeNumber
            let filename = init $ tail quotedFilename
            setFilePath filename
            let newPos = Position filename lineNumber 0
            setPosition newPos
            when (levelNumber < 0 || 2 < levelNumber) $
                lexicalError "line directive invalid level number"

        "include" -> do
            lineLookahead
            quotedFilename <- takeQuotedString
            fileFollow <- getFilePath
            bufFollow <- getBuffer
            -- find and load the included file
            let filename = init $ tail quotedFilename
            includePath <- includeSearch filename
            pushIncludeStack includePath
            includeContent <- liftIO $ loadFile includePath
            -- pre-process the included file
            setFilePath includePath
            setBuffer (includeContent, Position includePath 1 1)
            preprocessInput
            -- resume processing the original file
            popIncludeStack
            setFilePath fileFollow
            setBuffer bufFollow

        "ifdef" -> do
            dropSpaces
            name <- takeIdentifier
            pushCondStack "ifdef" name directivePos $
                ifCond $ Map.member name env
        "ifndef" -> do
            dropSpaces
            name <- takeIdentifier
            pushCondStack "ifndef" name directivePos $
                ifCond $ Map.notMember name env
        "else" -> do
            c <- cfCond <$> popCondStack "else"
            pushCondStack "else" "" directivePos (elseCond c)
        "elsif" -> do
            dropSpaces
            name <- takeIdentifier
            c <- cfCond <$> popCondStack "elsif"
            pushCondStack "elsif" name directivePos $
                elsifCond (Map.member name env) c
        "endif" -> do
            _ <- popCondStack "endif"
            return ()

        "define" -> do
            dropSpaces
            name <- do
                str <- takeIdentifier
                if elem str directives
                    then lexicalError $ "illegal macro name: " ++ str
                    else return str
            defn <- do
                str <- getInput
                if null str
                    then return ("", [])
                    else takeMacroDefinition
            setEnv $ Map.insert name defn env
        "undef" -> do
            dropSpaces
            name <- takeIdentifier
            setEnv $ Map.delete name env
        "undefineall" -> do
            setEnv Map.empty

        _ -> do
            case Map.lookup directive env of
                Nothing -> lexicalError $ "Undefined macro: " ++ directive
                Just (body, formalArgs) -> do
                    (names, args) <- if null formalArgs
                        then return ([], [])
                        else do
                            actualArgs <- takeMacroArguments
                            defaultedArgs <- defaultMacroArgs (map snd formalArgs) actualArgs
                            return (map fst formalArgs, defaultedArgs)
                    -- save our current state
                    currFile <- getFilePath
                    macroStack <- getMacroStack
                    bufFollow <- getBuffer
                    -- lex the macro expansion, preserving the file and line
                    let Position _ l c = snd bufFollow
                    let loc = "macro expansion of " ++ directive ++ " at " ++ currFile
                    let pos = Position loc l (c - length directive - 1)
                    setMacroStack $ (zip names args) : macroStack
                    setBuffer (body, pos)
                    preprocessInput
                    "" <- getInput
                    -- return to the rest of the input
                    setMacroStack macroStack
                    setBuffer bufFollow

-- inserts the given string into the output at the given position
insertChars :: Position -> String -> PPS ()
insertChars pos str = do
    bufFollow <- getBuffer
    setBuffer (str, pos)
    preprocessInput
    setBuffer bufFollow

-- pre-pre-processes the current line, such that macros can be used in
-- directives
lineLookahead :: PPS ()
lineLookahead = do
    line <- takeUntilNewline
    -- save the state
    outputOrig <- getOutput
    condStackOrig <- getCondStack
    inputOrig <- getInput
    -- process the line
    setOutput []
    setCondStack []
    setInput line
    preprocessInput
    outputAfter <- getOutput
    -- add in the new characters
    let newChars = reverse $ map fst outputAfter
    setInput $ newChars ++ inputOrig
    -- restore the previous state
    setOutput outputOrig
    setCondStack condStackOrig

-- run the given string through the current preprocessor state, but out of band
preprocessString :: String -> PPS String
preprocessString str = do
    -- save the state
    outputOrig <- getOutput
    condStackOrig <- getCondStack
    bufferOrig <- getBuffer
    -- process the line
    setOutput []
    setCondStack []
    setInput str
    preprocessInput
    outputAfter <- getOutput
    -- restore the previous state
    setBuffer bufferOrig
    setOutput outputOrig
    setCondStack condStackOrig
    -- get the result characters
    return $ reverse $ map fst outputAfter

-- update the position in the preprocessor state according to the movement of
-- the given character
advancePosition :: Char -> PPS ()
advancePosition '\n' = do
    Position f l _ <- getPosition
    setPosition $ Position f (l + 1) 1
advancePosition _ = do
    Position f l c <- getPosition
    setPosition $ Position f l (c + 1)

-- advances position for multiple characters
advancePositions :: String -> PPS ()
advancePositions = mapM_ advancePosition

-- update the given position based on the movement of the given character
advance :: Position -> Char -> Position
advance (Position f l _) '\n' = Position f (l + 1) 1
advance (Position f l c) _    = Position f l (c + 1)

-- adds a character (and its position) to the output state
pushChar :: Char -> Position -> PPS ()
pushChar c p = do
    condStack <- map cfCond <$> getCondStack
    when (all (== CurrentlyTrue) condStack) $ do
        output <- getOutput
        setOutput $ (c, p) : output

-- adds a sequence of characters all at the same given position
pushChars :: String -> Position -> PPS ()
pushChars s p = mapM_ (flip pushChar p) s

-- search for a pattern in the input and remove remove characters up to and
-- including the first occurrence of the pattern
removeThrough :: String -> PPS ()
removeThrough pat = do
    str <- getInput
    case findIndex (isPrefixOf pat) (tails str) of
        Nothing ->
            if pat == "\n"
                then setInput ""
                else lexicalError $ "Reached EOF while looking for: "
                        ++ show pat
        Just patternIdx -> do
            let chars = patternIdx + length pat
            let (dropped, rest) = splitAt chars str
            advancePositions dropped
            when (pat == "\n") $ do
                pos <- getPosition
                pushChar '\n' pos
            setInput rest
