{-# LANGUAGE TupleSections #-}
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

{-# NOINLINE parseNumber #-}
parseNumber :: Bool -> String -> (Number, String)
parseNumber oversizedNumbers =
    (parseNormalized oversizedNumbers) . normalizeNumber

-- normalize the number first, making everything lowercase and removing
-- visual niceties like spaces and underscores
normalizeNumber :: String -> String
normalizeNumber = map toLower . filter (not . isPad)
    where isPad = flip elem "_ \n\t"

-- truncate the given decimal number literal, if necessary
validateDecimal :: Bool -> String -> Int -> Bool -> Integer -> (Number, String)
validateDecimal oversizedNumbers str sz sg v
    | sz < -32 && oversizedNumbers =
        valid $ Decimal sz sg v
    | sz < -32 =
        addTruncateMessage str $
        if sg && v' > widthMask 31
            -- avoid zero-pad on signed decimals
            then Based (-32) sg Hex v' 0
            else Decimal (-32) sg v'
    | sz > 0 && v > widthMask sz =
        addTruncateMessage str $
            Decimal sz sg v'
    | otherwise =
        valid $ Decimal sz sg v
    where
        v' = v .&. widthMask truncWidth
        truncWidth = if sz < -32 then -32 else sz

-- produce a warning message describing the applied truncation
addTruncateMessage :: String -> Number -> (Number, String)
addTruncateMessage orig trunc = (trunc, msg)
    where
        width = show $ numberBitLength trunc
        bit = if width == "1" then "bit" else "bits"
        msg = "Number literal " ++ orig ++ " exceeds " ++ width ++ " " ++ bit
            ++ "; truncating to " ++ show trunc ++ "."

-- when extending a wide unsized number, we use the bit width if the digits
-- cover exactly that many bits, and add an extra 0 padding bit otherwise
extendUnsizedBased :: Int -> Number -> Number
extendUnsizedBased sizeByDigits n
    | size > -32 || sizeByBits < 32 = n
    | sizeByBits == sizeByDigits    = useWidth sizeByBits
    | otherwise                     = useWidth $ sizeByBits + 1
    where
        Based size sg base vals knds = n
        sizeByBits = fromIntegral $ max (bits vals) (bits knds)
        useWidth sz = Based (negate sz) sg base vals knds

-- truncate the given based number literal, if necessary
validateBased :: String -> Int -> Int -> Number -> (Number, String)
validateBased orig sizeIfPadded sizeByDigits n
    -- more digits than the size would allow for, regardless of their values
    | sizeIfPadded < sizeByDigits            = truncated
    -- unsized literal with fewer than 32 bits
    | 0 > size && size > -32                 = validated
    -- no padding bits are present
    | abs size >= sizeIfPadded               = validated
    -- check the padding bits in the leading digit, if there are any
    | all (isLegalPad sizethBit) paddingBits = validated
    -- some of the padding bits aren't legal
    | otherwise                              = truncated
    where
        Based size sg base vals knds = n
        n' = Based size sg base' vals' knds'

        validated = valid n
        truncated = addTruncateMessage orig n'

        -- checking padding bits
        sizethBit = getBit $ abs size - 1
        paddingBits = map getBit [abs size..sizeIfPadded - 1]
        getBit = getVKBit vals knds

        -- truncated the number, and selected a valid new base
        vals' = vals .&. widthMask size
        knds' = knds .&. widthMask size
        base' = if size == -32 && baseSelect == Octal
            then Binary
            else baseSelect
        baseSelect = selectBase base vals' knds'

-- if the MSB post-truncation is X or Z, then any padding bits must match; if
-- the MSB post-truncation is 0 or 1, then non-zero padding bits are forbidden
isLegalPad :: Bit -> Bit -> Bool
isLegalPad Bit1 = (== Bit0)
isLegalPad bit = (== bit)

parseNormalized :: Bool -> String -> (Number, String)
parseNormalized _ "'0" = valid $ UnbasedUnsized Bit0
parseNormalized _ "'1" = valid $ UnbasedUnsized Bit1
parseNormalized _ "'x" = valid $ UnbasedUnsized BitX
parseNormalized _ "'z" = valid $ UnbasedUnsized BitZ
parseNormalized oversizedNumbers str =
    -- simple decimal number
    if maybeIdx == Nothing then
        let num = readDecimal str
            sz = negate (decimalSize True num)
        in decimal sz True num
    -- non-decimal based integral number
    else if maybeBase /= Nothing then
        let (values, kinds) = parseBasedDigits (baseSize base) digitsExtended
            number = Based size signed base values kinds
            sizeIfPadded = sizeDigits * bitsPerDigit
            sizeByDigits = length digitsExtended * bitsPerDigit
        in if oversizedNumbers && size < 0
            then valid $ extendUnsizedBased sizeByDigits number
            else validateBased str sizeIfPadded sizeByDigits number
    -- decimal X or Z literal
    else if numDigits == 1 && leadDigitIsXZ then
        let vals = if elem leadDigit zDigits then widthMask size else 0
            knds = widthMask size
        in valid $ Based size signed Binary vals knds
    -- explicitly-based decimal number
    else
        let num = readDecimal digits
            sz = if rawSize == 0 then negate (decimalSize signed num) else size
        in decimal sz signed num
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
        leadDigitIsXZ = elem leadDigit xzDigits
        digitsExtended =
            if leadDigitIsXZ
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
            else if maybeBase == Nothing || leadDigitIsXZ then
                -32
            else
                negate $ min 32 $ bitsPerDigit * numDigits
        bitsPerDigit = bits $ baseSize base - 1

        -- shortcut for decimal outputs
        decimal :: Int -> Bool -> Integer -> (Number, String)
        decimal = validateDecimal oversizedNumbers str

-- shorthand denoting a valid number literal
valid :: Number -> (Number, String)
valid = (, "")

-- mask with the lowest N bits set
widthMask :: Int -> Integer
widthMask pow = 2 ^ (abs pow) - 1

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

-- get the logical bit value at the given index in a (values, kinds) pair
getVKBit :: Integer -> Integer -> Int -> Bit
getVKBit v k i =
    case (v .&. select, k .&. select) of
        (0, 0) -> Bit0
        (_, 0) -> Bit1
        (0, _) -> BitX
        (_, _) -> BitZ
    where select = 2 ^ i

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
    n1@Based{} <> n2@Based{} =
        Based size signed base values kinds
        where
            size = size1 + size2
            signed = False
            base = selectBase (max base1 base2) values kinds
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
            toBased n@Based{} = n
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
