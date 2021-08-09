{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog number literals
 -}

module Language.SystemVerilog.AST.Number
    ( Number (..)
    , Base (..)
    , Bit (..)
    , parseNumber
    , numberBitLength
    , numberIsSigned
    , numberIsSized
    , numberToInteger
    , numberCast
    , bitToVK
    ) where

import Data.Bits ((.&.), shiftL, xor)
import Data.Char (digitToInt, intToDigit, toLower)
import Data.List (elemIndex)
import Text.Read (readMaybe)

-- normalize the number first, making everything lowercase and removing
-- visual niceties like spaces and underscores
parseNumber :: String -> Number
parseNumber = parseNumber' . map toLower . filter (not . isPad)
    where isPad = flip elem "_ \n\t"

parseNumber' :: String -> Number
parseNumber' "'0" = UnbasedUnsized Bit0
parseNumber' "'1" = UnbasedUnsized Bit1
parseNumber' "'x" = UnbasedUnsized BitX
parseNumber' "'z" = UnbasedUnsized BitZ
parseNumber' str =
    -- simple decimal number
    if maybeIdx == Nothing then
        let n = readDecimal str
        in Decimal (negate $ decimalSize True n) True n
    -- non-decimal based integral number
    else if maybeBase /= Nothing then
        let (values, kinds) = parseBasedDigits (baseSize base) digitsExtended
        in Based size signed base values kinds
    -- decimal X or Z literal
    else if numDigits == 1 && elem leadDigit xzDigits then
        let (vals, knds) = parseBasedDigits 2 $ replicate (abs size) leadDigit
        in Based size signed Binary vals knds
    -- explicitly-based decimal number
    else
        let num = readDecimal digits
        in if rawSize == 0
            then Decimal (negate $ decimalSize signed num) signed num
            else Decimal size signed num
    where
        -- pull out the components of the literals
        maybeIdx = elemIndex '\'' str
        Just idx = maybeIdx
        signBasedAndDigits = drop (idx + 1) str
        (signed, baseAndDigits) = takeSign signBasedAndDigits
        (maybeBase, digits) = takeBase baseAndDigits

        -- high-order X or Z is extended up to the size of the literal
        leadDigit = head digits
        numDigits = length digits
        digitsExtended =
            if elem leadDigit xzDigits
                then replicate (sizeDigits - numDigits) leadDigit ++ digits
                else digits

        -- determine the number of digits needed based on the size
        sizeDigits = ((abs size) `div` bitsPerDigit) + sizeExtraDigit
        sizeExtraDigit =
            if (abs size) `mod` bitsPerDigit == 0
                then 0
                else 1

        -- determine the explicit size of the literal in bites
        Just base = maybeBase
        rawSize =
            if idx == 0
                then 0
                else readDecimal $ take idx str
        size =
            if rawSize /= 0 then
                rawSize
            else if maybeBase /= Nothing then
                negate $ bitsPerDigit * numDigits
            else
                -32
        bitsPerDigit = bits $ baseSize base - 1

-- read a simple unsigned decimal number
readDecimal :: Read a => String -> a
readDecimal str =
    case readMaybe str of
        Nothing -> error $ "could not parse decimal " ++ show str
        Just n -> n

-- returns the number of bits necessary to represent a number; it gives an extra
-- bit for signed numbers so that the literal doesn't sign extend unnecessarily
decimalSize :: Bool -> Integer -> Int
decimalSize True = max 32 . fromIntegral . bits . (* 2)
decimalSize False = max 32 . fromIntegral . bits

-- remove the leading sign specified, if it is present
takeSign :: String -> (Bool, String)
takeSign ('s' : rest) = (True, rest)
takeSign rest = (False, rest)

-- pop the leading base specified from a based coda
takeBase :: String -> (Maybe Base, String)
takeBase ('d' : rest) = (Nothing, rest)
takeBase ('b' : rest) = (Just Binary, rest)
takeBase ('o' : rest) = (Just Octal, rest)
takeBase ('h' : rest) = (Just Hex, rest)
takeBase rest = error $ "cannot parse based coda " ++ show rest

-- convert the digits of a based number to its corresponding value and kind bits
parseBasedDigits :: Integer -> String -> (Integer, Integer)
parseBasedDigits base str =
    (values, kinds)
    where
        values = parseDigits parseValueDigit str
        kinds = parseDigits parseKindDigit str

        parseDigits :: (Char -> Integer) -> String -> Integer
        parseDigits f = foldl sumStep 0 . map f
        sumStep :: Integer -> Integer -> Integer
        sumStep total digit = total * base + digit

        parseValueDigit :: Char -> Integer
        parseValueDigit x =
            if elem x xDigits then
                0
            else if elem x zDigits then
                base - 1
            else
                fromIntegral $ digitToInt x

        parseKindDigit :: Char -> Integer
        parseKindDigit x =
            if elem x xzDigits
                then base - 1
                else 0

xDigits :: [Char]
xDigits = ['x']
zDigits :: [Char]
zDigits = ['z', '?']
xzDigits :: [Char]
xzDigits = xDigits ++ zDigits

data Bit
    = Bit0
    | Bit1
    | BitX
    | BitZ
    deriving Eq

instance Show Bit where
    show Bit0 = "0"
    show Bit1 = "1"
    show BitX = "x"
    show BitZ = "z"

-- convet an unbased unsized bit to its (values, kinds) pair
bitToVK :: Bit -> (Integer, Integer)
bitToVK Bit0 = (0, 0)
bitToVK Bit1 = (1, 0)
bitToVK BitX = (0, 1)
bitToVK BitZ = (1, 1)

data Base
    = Binary
    | Octal
    | Hex
    deriving (Eq, Ord)

instance Show Base where
    show Binary = "b"
    show Octal  = "o"
    show Hex    = "h"

data Number
    = UnbasedUnsized Bit
    | Decimal Int Bool Integer
    | Based   Int Bool Base Integer Integer
    deriving Eq

baseSize :: Integral a => Base -> a
baseSize Binary = 2
baseSize Octal  = 8
baseSize Hex    = 16

-- get the number of bits in a number
numberBitLength :: Number -> Integer
numberBitLength UnbasedUnsized{} = 1
numberBitLength (Decimal size _ _) = fromIntegral $ abs size
numberBitLength (Based size _ _ _ _) =
    fromIntegral $
    if size < 0
        then max 32 $ negate size
        else size

-- get whether or not a number is signed
numberIsSized :: Number -> Bool
numberIsSized UnbasedUnsized{} = False
numberIsSized (Decimal size _ _) = size > 0
numberIsSized (Based size _ _ _ _) = size > 0

-- get whether or not a number is signed
numberIsSigned :: Number -> Bool
numberIsSigned UnbasedUnsized{} = False
numberIsSigned (Decimal _ signed _) = signed
numberIsSigned (Based _ signed _ _ _) = signed

-- get the integer value of a number, provided it has not X or Z bits
numberToInteger :: Number -> Maybe Integer
numberToInteger (UnbasedUnsized Bit1) = Just 1
numberToInteger (UnbasedUnsized Bit0) = Just 0
numberToInteger UnbasedUnsized{} = Nothing
numberToInteger (Decimal sz sg num)
    | not sg || num .&. pow == 0 = Just num
    | otherwise                  = Just $ negate $ num `xor` mask + 1
    where
        pow = 2 ^ (abs sz - 1)
        mask = pow + pow - 1
numberToInteger (Based sz sg _ num 0) =
    numberToInteger $ Decimal sz sg num
numberToInteger Based{} = Nothing

-- return the number of bits in a number (i.e. ilog2)
bits :: Integral a => a -> a
bits 0 = 0
bits n = 1 + bits (quot n 2)

-- number to string conversion
instance Show Number where
    show (UnbasedUnsized bit) =
        '\'' : show bit
    show (Decimal (-32) True value) =
        if value < 0
            then error $ "illegal decimal: " ++ show value
            else show value
    show (Decimal size signed value) =
        if size == 0
            then error $ "illegal decimal literal: "
                ++ show (size, signed, value)
            else sizeStr ++ '\'' : signedStr ++ 'd' : valueStr
        where
            sizeStr = if size > 0 then show size else ""
            signedStr = if signed then "s" else ""
            valueStr = show value
    show (Based size signed base value kinds) =
        if size == 0 || value < 0 || kinds < 0
            then error $ "illegal based literal: "
                ++ show (size, signed, base, value, kinds)
            else sizeStr ++ '\'' : signedStr ++ baseCh : valueStr
        where
            sizeStr = if size > 0 then show size else ""
            signedStr = if signed then "s" else ""
            [baseCh] = show base
            valueStr = showBasedDigits signed (baseSize base) size value kinds

showBasedDigits :: Bool -> Int -> Int -> Integer -> Integer -> String
showBasedDigits signed base size values kinds =
    if numDigits > sizeDigits then
        error $ "invalid based literal digits: "
            ++ show (base, size, values, kinds, numDigits, sizeDigits)
    else if size < -32 || (size < 0 && signed) then
        padList '0' sizeDigits digits
    else if leadingXZ && size < 0 && sizeDigits == numDigits then
        removeExtraPadding digits
    else if leadingXZ || (256 >= size && size > 0) then
        padList '0' sizeDigits digits
    else
        digits
    where
        valChunks = chunk (fromIntegral base) values
        kndChunks = chunk (fromIntegral base) kinds
        numDigits = max (length valChunks) (length kndChunks)

        digits = zipWith combineChunks
            (padList 0 numDigits valChunks)
            (padList 0 numDigits kndChunks)
        leadingXZ = elem (head digits) xzDigits

        removeExtraPadding :: String -> String
        removeExtraPadding ('x' : 'x' : chs) = removeExtraPadding ('x' : chs)
        removeExtraPadding ('z' : 'z' : chs) = removeExtraPadding ('z' : chs)
        removeExtraPadding chs = chs

        -- determine the number of digits needed based on the explicit size
        sizeDigits = ((abs size) `div` bitsPerDigit) + sizeExtraDigit
        sizeExtraDigit =
            if (abs size) `mod` bitsPerDigit == 0
                then 0
                else 1
        bitsPerDigit = bits $ base - 1

        -- combine a value and kind digit into their corresponding character
        combineChunks :: Int -> Int -> Char
        combineChunks value kind =
            if kind == 0 then
                intToDigit value
            else if kind /= base - 1 then
                invalid
            else if value == 0 then
                'x'
            else if value == base - 1 then
                'z'
            else
                invalid
            where
                invalid = error $ "based bits inconsistent: "
                    ++ show (base, values, kinds, value, kind)

-- pad the left side of a list with `padding` to be at least `size` elements
padList :: a -> Int -> [a] -> [a]
padList padding size values =
    replicate (size - length values) padding ++ values

-- split an integer into chunks of `base` bits
chunk :: Integer -> Integer -> [Int]
chunk base n0 =
    reverse $ chunkStep (quotRem n0 base)
    where
        chunkStep (n, d) =
            case n of
                0 -> [d']
                _ -> d' : chunkStep (quotRem n base)
            where d' = fromIntegral d

-- number concatenation
instance Semigroup Number where
    (n1 @ Based{}) <> (n2 @ Based{}) =
        Based size signed base values kinds
        where
            size = size1 + size2
            signed = False
            base = selectBase (min base1 base2) values kinds
            trim = flip mod . (2 ^)
            values = trim size2 values2 + shiftL (trim size1 values1) size2
            kinds = trim size2 kinds2 + shiftL (trim size1 kinds1) size2
            size1 = fromIntegral $ numberBitLength n1
            size2 = fromIntegral $ numberBitLength n2
            Based _ _ base1 values1 kinds1 = n1
            Based _ _ base2 values2 kinds2 = n2
    n1 <> n2 =
        toBased n1 <> toBased n2
        where
            toBased (n @ Based{}) = n
            toBased (Decimal size signed num) =
                Based size signed Hex num 0
            toBased (UnbasedUnsized bit) =
                uncurry (Based 1 False Binary) (bitToVK bit)

-- size cast raw bits with optional sign extension
rawCast :: Bool -> Int -> Int -> Integer -> Integer
rawCast signed inSize outSize val =
    if outSize <= inSize then
        val `mod` (2 ^ outSize)
    else if signed && val >= 2 ^ (inSize - 1) then
        valTrim + 2 ^ outSize - 2 ^ inSize
    else
        valTrim
    where valTrim = val `mod` (2 ^ inSize)

-- check if the based number is valid under the given base
checkBase :: Integer -> Integer -> Integer -> Bool
checkBase _ _ 0 = True
checkBase base v k =
    -- kind bits in this chunk must all be the same
    (rK == 0 || rK == base - 1) &&
    -- if the X/Z, it must be all X or all Z
    (rK == 0 || rV == 0 || rV == base - 1) &&
    -- check the next chunk
    checkBase base qV qK
    where
        (qV, rV) = v `divMod` base
        (qK, rK) = k `divMod` base

-- select the maximal valid base
selectBase :: Base -> Integer -> Integer -> Base
selectBase Binary _ _ = Binary
selectBase Octal v k =
    if checkBase 8 v k
        then Octal
        else Binary
selectBase Hex v k =
    if checkBase 16 v k
        then Hex
        else selectBase Octal v k

-- utility for size and/or sign casting a number
numberCast :: Bool -> Int -> Number -> Number
numberCast outSigned outSize (Decimal inSizeRaw inSigned inVal) =
    Decimal outSize outSigned outVal
    where
        inSize = abs inSizeRaw
        outVal = rawCast inSigned inSize outSize inVal
numberCast outSigned outSize (Based inSizeRaw inSigned inBase inVal inKnd) =
    Based outSize outSigned outBase outVal outKnd
    where
        inSize = abs inSizeRaw
        -- sign extend signed inputs, or unsized literals with a leading X/Z
        doExtend = inSigned || inKnd >= 2 ^ (inSize - 1) && inSizeRaw < 0
        outVal = rawCast doExtend inSize outSize inVal
        outKnd = rawCast doExtend inSize outSize inKnd
        -- note that we could try patching the upper bits of the result to allow
        -- the use of a higher base as in 5'(6'ozx), but this should be rare
        outBase = selectBase inBase outVal outKnd
numberCast signed size (UnbasedUnsized bit) =
    numberCast signed size $
    uncurry (Based 1 True Binary) $
        case bit of
            Bit0 -> (0, 0)
            Bit1 -> (1, 0)
            BitX -> (0, 1)
            BitZ -> (1, 1)
