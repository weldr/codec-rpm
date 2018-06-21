-- |
-- Module: Codec.RPM.Parse
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: stable
-- Portability: portable
-- 
-- Functions and types for working with version numbers, as understood by RPM.

module Codec.RPM.Version(
    -- * Types
    DepOrdering(..),
    DepRequirement(..),
    EVR(..),
    -- * Functions
    parseEVR,
    parseDepRequirement,
    satisfies,
    vercmp)
 where

import           Data.Char(digitToInt, isAsciiLower, isAsciiUpper, isDigit, isSpace)
import           Data.Maybe(fromMaybe)
import           Data.Monoid((<>))
import qualified Data.Ord as Ord
import qualified Data.Text as T
import           Data.Word(Word32)
import           Text.Parsec

import Prelude hiding(EQ, GT, LT)

-- | The versioning information portion of a package's name - epoch, version, release.
data EVR = EVR {
    -- | The epoch of a package.  This is sort of a super version number, used when a package with
    -- an earlier version number must upgrade a package with a later version number.  The package
    -- with a larger epoch will always in version comparisons.  Most packages do not have an epoch.
    epoch :: Maybe Word32,
    -- | The version number provided by the package's upstream, represented as 'Data.Text.Text'.
    version :: T.Text,
    -- | The release number, represented as 'Data.Text.Text'.  The release value is added on by a
    -- distribution and allows them to make multiple releases of the same upstream version, fixing
    -- bugs and applying distribution-specific tweaks.
    release :: T.Text }
 deriving(Show)

-- for Ord and Eq, an epoch of Nothing is the same as an epoch of 0.
-- for Eq, version and release strings need to go through vercmp, since they can be equivalent
-- without being the same String.
instance Eq EVR where
    (==) evr1 evr2 = evr1 `compare` evr2 == Ord.EQ

instance Ord EVR where
    compare evr1 evr2 = fromMaybe 0 (epoch evr1) `compare` fromMaybe 0 (epoch evr2) <>
                        version evr1 `vercmp` version evr2 <>
                        release evr1 `vercmp` release evr2

-- | Like 'Ordering', but with support for less-than-or-equal and greater-than-or-equal.
data DepOrdering = LT | LTE | EQ | GTE | GT
 deriving(Eq, Ord, Show)

-- | RPM supports the concept of dependencies between packages.  Collectively, these dependencies
-- are commonly referred to as PRCO - Provides, Requires, Conflicts, and Obsoletes.  These
-- dependencies can optionally include version information.  These relationships can be examined
-- with various RPM inspection tools or can be found in the spec files that define how a package
-- is built.  Examples include:
--
-- @
-- Requires: python-six
-- Requires: python3-blivet >= 1:1.0
-- Obsoletes: booty <= 0.107-1
-- @
--
-- This data type expresses a single dependency relationship.  The example dependencies above
-- would be represented like so:
--
-- @
-- DepRequirement "python-six" Nothing
-- DepRequirement "python3-blivet" (Just (GTE, EVR (Just 1) "1.0" ""))
-- DepRequirement "booty" (Just (LTE, EVR Nothing "0.107" "1"))
-- @
--
-- It is not in the scope of this type to know what kind of relationship a 'DepRequirement'
-- describes.
--
-- This type derives 'Ord' so that it can be easily be used with collection types, but the
-- derived ordering will not make sense for the purpose of comparing requirements. Use
-- 'satisfies' to determine if requirements match one another.
data DepRequirement = DepRequirement T.Text (Maybe (DepOrdering, EVR))
 deriving (Eq, Ord, Show)

-- | Compare two version numbers and return an 'Ordering'.
vercmp :: T.Text -> T.Text -> Ordering
vercmp a b = let
    -- strip out all non-version characters
    -- keep in mind the strings may be empty after this
    a' = dropSeparators a
    b' = dropSeparators b

    -- rpm compares strings by digit and non-digit components, so grab the first
    -- component of one type
    fn = if isDigit (T.head a') then isDigit else isAsciiAlpha
    (prefixA, suffixA) = T.span fn a'
    (prefixB, suffixB) = T.span fn b'
 in
       -- Nothing left means the versions are equal
    if | T.null a' && T.null b'                             -> Ord.EQ
       -- tilde ls less than everything, including an empty string
       | ("~" `T.isPrefixOf` a') && ("~" `T.isPrefixOf` b') -> vercmp (T.tail a') (T.tail b')
       | ("~" `T.isPrefixOf` a')                            -> Ord.LT
       | ("~" `T.isPrefixOf` b')                            -> Ord.GT
       -- otherwise, if one of the strings is null, the other is greater
       | (T.null a')                                        -> Ord.LT
       | (T.null b')                                        -> Ord.GT
       -- Now we have two non-null strings, starting with a non-tilde version character
       -- If one prefix is a number and the other is a string, the one that is a number
       -- is greater.
       | isDigit (T.head a') && (not . isDigit) (T.head b') -> Ord.GT
       | (not . isDigit) (T.head a') && isDigit (T.head b') -> Ord.LT
       | isDigit (T.head a')                                -> (prefixA `compareAsInts` prefixB) <> (suffixA `vercmp` suffixB)
       | otherwise                                          -> (prefixA `compare` prefixB) <> (suffixA `vercmp` suffixB)
 where
    compareAsInts :: T.Text -> T.Text -> Ordering
    -- the version numbers can overflow Int, so strip leading 0's and do a string compare,
    -- longest string wins
    compareAsInts x y =
        let x' = T.dropWhile (== '0') x
            y' = T.dropWhile (== '0') y
        in 
            if T.length x' > T.length y' then Ord.GT
            else x' `compare` y'

    -- isAlpha returns any unicode alpha, but we just want ASCII characters
    isAsciiAlpha :: Char -> Bool
    isAsciiAlpha x = isAsciiLower x || isAsciiUpper x

    -- RPM only cares about ascii digits, ascii alpha, and ~
    isVersionChar :: Char -> Bool
    isVersionChar x = isDigit x || isAsciiAlpha x || x == '~'

    dropSeparators :: T.Text -> T.Text
    dropSeparators = T.dropWhile (not . isVersionChar)

{-# ANN satisfies ("HLint: ignore Redundant if" :: String) #-}
-- | Determine if a candidate package satisfies the dependency relationship required by some other
-- package.
satisfies :: DepRequirement         -- ^ The package in question, represented as a 'DepRequirement'.
          -> DepRequirement         -- ^ The requirement.
          -> Bool
satisfies (DepRequirement name1 ver1) (DepRequirement name2 ver2) =
    -- names have to match
    if name1 /= name2 then False
    else satisfiesVersion ver1 ver2
 where
    -- If either half has no version expression, it's a match
    satisfiesVersion Nothing _ = True
    satisfiesVersion _ Nothing = True

    -- There is a special case for matching versions with no release component.
    -- If one side is equal to (or >=, or <=) a version with no release component, it will match any non-empty
    -- release on the other side, regardless of operator.
    -- For example: x >= 1.0 `satisfies` x < 1.0-47.
    -- If *both* sides have no release, the regular rules apply, so x >= 1.0 does not satisfy x < 1.0

    satisfiesVersion (Just (o1, v1)) (Just (o2, v2))
        | T.null (release v1) && (not . T.null) (release v2) && compareEV v1 v2 && isEq o1 = True
        | T.null (release v2) && (not . T.null) (release v1) && compareEV v1 v2 && isEq o2 = True
        | otherwise =
            case compare v1 v2 of
                -- e1 < e2, true if >[=] e1 || <[=] e2
                Ord.LT -> isGt o1 || isLt o2
                -- e1 > e2, true if <[=] e1 || >[=] e2
                Ord.GT -> isLt o1 || isGt o2
                -- e1 == e2, true if both sides are the same direction
                Ord.EQ -> (isLt o1 && isLt o2) || (isEq o1 && isEq o2) || (isGt o1 && isGt o2)

    isEq EQ  = True
    isEq GTE = True
    isEq LTE = True
    isEq _   = False

    isLt LT  = True
    isLt LTE = True
    isLt _   = False

    isGt GT  = True
    isGt GTE = True
    isGt _   = False

    compareEV v1 v2 = fromMaybe 0 (epoch v1) == fromMaybe 0 (epoch v2) && version v1 == version v2

-- parsers for version strings
-- the EVR Parsec is shared by the EVR and DepRequirement parsers
parseEVRParsec :: Parsec T.Text () EVR
parseEVRParsec = do
    e <- optionMaybe $ try parseEpoch
    v <- many1 versionChar
    r <- try parseRelease <|> return ""
    eof

    return EVR{epoch=e, version=T.pack v, release=T.pack r}
 where
    parseEpoch :: Parsec T.Text () Word32
    parseEpoch = do
        e <- many1 digit
        _ <- char ':'

        -- parse the digit string as an Integer until it ends or overflows Word32
        parseInteger 0 e
     where
        maxW32 = toInteger (maxBound :: Word32)

        parseInteger :: Integer -> String -> Parsec T.Text () Word32
        parseInteger acc []     = return $ fromInteger acc
        parseInteger acc (x:xs) = let
            newAcc = (acc * (10 :: Integer)) + toInteger (digitToInt x)
         in
            if newAcc > maxW32 then parserFail ""
            else parseInteger newAcc xs

    parseRelease = do
        _ <- char '-'
        many1 versionChar

    versionChar = digit <|> upper <|> lower <|> oneOf "._+%{}~"

-- | Convert a 'Data.Text.Text' representation into an 'EVR' or a 'ParseError' if something goes wrong.
parseEVR :: T.Text -> Either ParseError EVR
parseEVR = parse parseEVRParsec ""

-- | Convert a 'Data.Text.Text' representation into a 'DepRequirement' or a 'ParseError' if something
-- goes wrong.
parseDepRequirement :: T.Text -> Either ParseError DepRequirement
parseDepRequirement input = parse parseDepRequirement' "" input
 where
    parseDepRequirement' = do
        reqname <- many $ satisfy (not . isSpace)
        spaces
        reqver <- optionMaybe $ try parseDepVersion

        -- If anything went wrong in parsing the version (invalid operator, malformed EVR), treat the entire
        -- string as a name. This way RPMs with bad version strings in Requires, which of course exist, will
        -- match against the full string.
        case reqver of
            Just _  -> return $ DepRequirement (T.pack reqname) reqver
            Nothing -> return $ DepRequirement input Nothing

    -- check lte and gte first, since they overlap lt and gt
    parseOperator :: Parsec T.Text () DepOrdering
    parseOperator = lte <|> gte <|> eq <|> lt <|> gt

    eq  = try (string "=")  >> return EQ
    lt  = try (string "<")  >> return LT
    gt  = try (string ">")  >> return GT
    lte = try (string "<=") >> return LTE
    gte = try (string ">=") >> return GTE

    parseDepVersion :: Parsec T.Text () (DepOrdering, EVR)
    parseDepVersion = do
        oper <- parseOperator
        spaces
        evr <- parseEVRParsec
        eof

        return (oper, evr)
