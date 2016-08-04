{-# LANGUAGE MultiParamTypeClasses , FlexibleInstances #-}

{- |

module:         Data.CaseInsensitive.Eq
description:    Simple, fast case-insensitive matching of various string types.

This, like most other case-insensitive matching algorithms, is an
over-simplification. For some complex Q&A about properties like case, see
<http://unicode.org/faq/casemap_charprop.html>. A round-trip case-conversion
does not necessarily result in the original character(s) in some scripts. Also,
string matching is not necessarily the same as character matching. The
char-at-a-time approach taken here is not technically correct, but works for
the most scripts, and definitely for ASCII and the ISO-8859-1 8-bit Latin 1
character set.

Usage is simple:

> main = putStrLn $ if "HREF" ^== "href" then "match" else "no match"
 
-}

module Data.CaseInsensitive.Eq
    ( CaseInsensitiveEq(..)
    , (^==)
    , (^/=)
    )
where

import           Data.Char
import           Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

class CaseInsensitiveEq a where
    caseInsensitiveEq :: a -> a -> Bool

    caseInsensitiveMatch :: [a] -> [a] -> Bool
    caseInsensitiveMatch = list_eq

-- | An equality operator for 'CaseInsensitiveEq'.
(^==) :: (CaseInsensitiveEq a) => a -> a -> Bool
(^==) = caseInsensitiveEq


-- | An inequality operator for 'CaseInsensitiveEq'.
(^/=) :: (CaseInsensitiveEq a) => a -> a -> Bool
(^/=) a = not . (^==) a

------------------------------------------------------------------------------
--
-- instances

instance CaseInsensitiveEq Char where
    caseInsensitiveEq = char_eq

instance (CaseInsensitiveEq a) => CaseInsensitiveEq [a] where
    caseInsensitiveEq = caseInsensitiveMatch

instance CaseInsensitiveEq Word8 where
    caseInsensitiveEq = char8_eq

instance CaseInsensitiveEq BS.ByteString where
    caseInsensitiveEq = bs_strict_eq

instance CaseInsensitiveEq BSL.ByteString where
    caseInsensitiveEq = bs_lazy_eq

instance CaseInsensitiveEq T.Text where
    caseInsensitiveEq = t_strict_eq

instance CaseInsensitiveEq TL.Text where
    caseInsensitiveEq = t_lazy_eq

instance (CaseInsensitiveEq a, CaseInsensitiveEq b) => CaseInsensitiveEq (a,b) where
    caseInsensitiveEq (a,b) (a',b')
        | caseInsensitiveEq a a' = True
        | otherwise = caseInsensitiveEq b b'

instance (CaseInsensitiveEq a, CaseInsensitiveEq b, CaseInsensitiveEq c) => CaseInsensitiveEq (a,b,c) where
    caseInsensitiveEq (a,b,c) (a',b',c')
        | caseInsensitiveEq a a' = True
        | otherwise = caseInsensitiveEq (b,c) (b',c')

instance (CaseInsensitiveEq a, CaseInsensitiveEq b, CaseInsensitiveEq c, CaseInsensitiveEq d) => CaseInsensitiveEq (a,b,c,d) where
    caseInsensitiveEq (a,b,c,d) (a',b',c',d')
        | caseInsensitiveEq a a' = True
        | otherwise = caseInsensitiveEq (b,c,d) (b',c',d')



-- | Unicode character matching. The real complexity of this masked by our
--   one-char-at-a-time approach. If all alphabetic characters were either
--   upper or lower case with a one-to-one mapping, both toUpper tests would
--   not be needed. However, there are some non-transitive and one-to-many
--   cases for Unicode characters. This is a pragmatic approach that is
--   better than using only 'toLower', which fails for some characters.
{-# INLINE char_eq #-}
char_eq :: Char -> Char -> Bool
char_eq a b
    | a == b = True
    | a == toLower b = True
    | b == toLower a = True
    | a == toUpper b = True
    | b == toUpper a = True
    | otherwise = False

-- | ISO-8859-1 character matching. The code here looks wonky, but this is an
--   attempt to find the answer in as few steps as necessary.
{-# INLINE char8_eq #-}
char8_eq :: Word8 -> Word8 -> Bool
char8_eq a b
    | a == b = True                     --
    | a < 65 || b < 65 = False          -- [0..64]
    | a < 91 && b < 123 = a == b - 32   -- [65..122]
    | b < 91 && a < 123 = b == a - 32   --
    | a < 192 || b < 192 = False        -- [123..192]
    | a == 215 || b == 215 = False      -- not 215
    | a < 223 = a == b - 32             -- [193..214,216..254]
    | b < 223 = a - 32 == b
    | otherwise = False


-- A two-step process for comparing ByteStrings, since length is O(1).
bs_strict_eq :: BS.ByteString -> BS.ByteString -> Bool
bs_strict_eq a b
    | BS.length a /= BS.length b = False -- length is O(1)
    | otherwise = continue_bs_strict_eq a b

-- All of the following follow the same pattern, matching a character at a time.
-- If anyone can find a faster way to do this, please send it in!

continue_bs_strict_eq a b
    | BS.null a = True -- assertion: BS.null b == True after the length check
    | char8_eq (BS.head a) (BS.head b) = continue_bs_strict_eq (BS.tail a) (BS.tail b)
    | otherwise = False

list_eq :: (CaseInsensitiveEq a) => [a] -> [a] -> Bool
list_eq a b
    | null a || null b = null a && null b
    | caseInsensitiveEq (head a) (head b) = list_eq (tail a) (tail b)
    | otherwise = False

bs_lazy_eq :: BSL.ByteString -> BSL.ByteString -> Bool
bs_lazy_eq a b
    | BSL.null a || BSL.null b = BSL.null a && BSL.null b
    | char8_eq (BSL.head a) (BSL.head b) = bs_lazy_eq (BSL.tail a) (BSL.tail b)
    | otherwise = False

t_strict_eq :: T.Text -> T.Text -> Bool
t_strict_eq a b
    | T.null a || T.null b = T.null a && T.null b
    | char_eq (T.head a) (T.head b) = t_strict_eq (T.tail a) (T.tail b)
    | otherwise = False

t_lazy_eq :: TL.Text -> TL.Text -> Bool
t_lazy_eq a b
    | TL.null a || TL.null b = TL.null a && TL.null b
    | char_eq (TL.head a) (TL.head b) = t_lazy_eq (TL.tail a) (TL.tail b)
    | otherwise = False

