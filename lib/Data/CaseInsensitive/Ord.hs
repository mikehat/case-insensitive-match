
{- |
module: Data.CaseInsensitive.Ord
description: Case-insensitive comparison of strings

Case folding of Unicode characters is a bit of a mine field, and making ordinal
comparisons of characters is sketchy as well. For example, how do we compare
alphabetic and non-alphabetic characters, or a control character and a symbol?
Unicode characters don't always have a one-to-one mapping between upper- and
lower-case, so one-at-a-time character comparisons don't necessarily make
string comparisons valid. To make things worse, some applications might have a
preference for ordering \"Apple\" ahead of \"apple\" while others would prefer them
to be 'EQ' so long as \"BANANA\" compares 'GT' to either of them.

Herein is an attempt to combine both Unicode case folding and non-alphabetic
character comparisons in one module. @Be( a)?ware@ of the details and try to
think critically of your specific needs.

That /slippery disclaimer/ aside, this should exhibit most people's sense of
\'normal\' behavior when comparing and ordering strings in a case-insensitive
manner. In particular, when comparing strings with all-alphanumeric characters
or strings with matching non-alphanumeric characters in /identical/ positions
this module will yield satisfying results.

__Reasonable comparisons__:

>   "James" and "Mary"

Two first names, no non-alphanumerics.

>   "http://www.haskell.org/" and "HTTP://WWW.EXAMPLE.COM/"

All non-alphanumerics in matching positions until a comparison can be made.
This would be much more satisfying if the URI were parsed, even to the point of
breaking the hostname into components.

>   ("Appleseed","Johnny") and ("Bunyon","Paul")

Comparing last-name-to-last-name and then first-to-first.


__Questionable comparisons__:

>   "Franklin, Benjamin" and "Hitler, Adolph"
>   "Smith, Snuffy" and "Smithers, Waylon"
>   "me@example.com" and "YOU@EXAMPLE.COM"

Politics and morality aside, the @last, first@ format will inevitably result in
comparisons between a comma and an alphabetic character. The result, while
intuitively correct in this case, is so because a comma is \'less than\' a
letter for arbitrary reasons (OK, the ASCII creators probably thought deeply
about this and maybe it's not simply arbitrary). Beware when someone decides to
sort your @User@ object on @last_first@ because of some local or locale-based
assumption and the results look a little backwards to normal humans.

> "Https://www.example.com" and "http://www.haskell.org/"

Do all HTTP URIs come before all HTTPS ones, or should the hostname take
precedence?  Again, think carefully about why you are making case-insensitive
comparisons.

-}

module Data.CaseInsensitive.Ord
    ( CaseInsensitiveOrd(..)
    , (^>)
    , (^<)
    , (^>=)
    , (^<=)
    , caseInsensitiveComparing
    )
where

import           Data.Ord
import           Data.Char
import           Data.Word

import           Data.CaseInsensitive.Eq

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | A class for ordering strings in a case-insensitive manner.
class CaseInsensitiveOrd a where
    caseInsensitiveCompare :: a -> a -> Ordering

-- | /greater than/ for case-insensitive strings
(^>) :: (CaseInsensitiveOrd a) => a -> a -> Bool
a ^> b = caseInsensitiveCompare a b == GT

-- | /less than/ for case-insensitive strings
(^<) :: (CaseInsensitiveOrd a) => a -> a -> Bool
a ^< b = caseInsensitiveCompare a b == LT

-- | /greater than or equal to/ for case-insensitive strings
(^>=) :: (CaseInsensitiveOrd a) => a -> a -> Bool
a ^>= b = caseInsensitiveCompare a b /= LT

-- | /less than or equal to/ for case-insensitive strings
(^<=) :: (CaseInsensitiveOrd a) => a -> a -> Bool
a ^<= b = caseInsensitiveCompare a b /= GT

-- | 'comparing' for 'CaseInsensitiveOrd'
caseInsensitiveComparing :: (CaseInsensitiveOrd a) => (b -> a) -> b -> b -> Ordering
caseInsensitiveComparing f a b = caseInsensitiveCompare (f a) (f b)



instance CaseInsensitiveOrd Char where
    caseInsensitiveCompare = compare_char

instance CaseInsensitiveOrd Word8 where
    caseInsensitiveCompare = compare_char8

instance (CaseInsensitiveOrd a) => CaseInsensitiveOrd [a] where
    caseInsensitiveCompare = compare_list

instance CaseInsensitiveOrd BS.ByteString where
    caseInsensitiveCompare = compare_bs

instance CaseInsensitiveOrd BSL.ByteString where
    caseInsensitiveCompare = compare_bsl

instance CaseInsensitiveOrd T.Text where
    caseInsensitiveCompare = compare_t

instance CaseInsensitiveOrd TL.Text where
    caseInsensitiveCompare = compare_tl

instance (CaseInsensitiveOrd a,CaseInsensitiveOrd b) => CaseInsensitiveOrd (a,b) where
    caseInsensitiveCompare (a,b) (a',b') =
        case caseInsensitiveCompare a a' of
            EQ -> caseInsensitiveCompare b b'
            ord -> ord

instance (CaseInsensitiveOrd a,CaseInsensitiveOrd b,CaseInsensitiveOrd c) => CaseInsensitiveOrd (a,b,c) where
    caseInsensitiveCompare (a,b,c) (a',b',c') =
        case caseInsensitiveCompare a a' of
            EQ -> caseInsensitiveCompare (b,c) (b',c')
            ord -> ord

instance (CaseInsensitiveOrd a,CaseInsensitiveOrd b,CaseInsensitiveOrd c,CaseInsensitiveOrd d) => CaseInsensitiveOrd (a,b,c,d) where
    caseInsensitiveCompare (a,b,c,d) (a',b',c',d') =
        case caseInsensitiveCompare a a' of
            EQ -> caseInsensitiveCompare (b,c,d) (b',c',d')
            ord -> ord


-- | Compare Char
{-# INLINE compare_char #-}
compare_char :: Char -> Char -> Ordering
compare_char a b
    | caseInsensitiveEq a b = EQ
    | otherwise = compare (toUpper a) (toUpper b)

-- | Compare 8-bit ISO-8859-1 words.
{-# INLINE compare_char8 #-}
compare_char8 :: Word8 -> Word8 -> Ordering
compare_char8 a b
    | a == b = EQ
    | a < 65 || b < 65 = compare a b
    | a < 97 && b < 97 = compare a b
    | a < 91 && b < 123 = compare a (b - 32)
    | b < 91 && a < 123 = compare (a - 32) b
    | a < 216 && b < 216 = compare a b
    | a == 247 = GT
    | b == 247 = LT
    | a == 215 = compare a (b - 32)
    | b == 215 = compare (a - 32) b
    | a < 224 = compare a (b - 32)
    | b < 224 = compare (a - 32) b
    | otherwise = compare a b


compare_list :: (CaseInsensitiveOrd a) => [a] -> [a] -> Ordering
compare_list a b
    | null a && null b = EQ
    | null a = LT
    | null b = GT
    | otherwise =
        case caseInsensitiveCompare (head a) (head b) of
            EQ -> compare_list (tail a) (tail b)
            ord -> ord

compare_bs :: BS.ByteString -> BS.ByteString -> Ordering
compare_bs a b
    | BS.null a && BS.null b = EQ
    | BS.null a = LT
    | BS.null b = GT
    | otherwise =
        case caseInsensitiveCompare (BS.head a) (BS.head b) of
            EQ -> compare_bs (BS.tail a) (BS.tail b)
            ord -> ord

compare_bsl :: BSL.ByteString -> BSL.ByteString -> Ordering
compare_bsl a b
    | BSL.null a && BSL.null b = EQ
    | BSL.null a = LT
    | BSL.null b = GT
    | otherwise =
        case caseInsensitiveCompare (BSL.head a) (BSL.head b) of
            EQ -> compare_bsl (BSL.tail a) (BSL.tail b)
            ord -> ord

compare_t :: T.Text -> T.Text -> Ordering
compare_t a b
    | T.null a && T.null b = EQ
    | T.null a = LT
    | T.null b = GT
    | otherwise =
        case caseInsensitiveCompare (T.head a) (T.head b) of
            EQ -> compare_t (T.tail a) (T.tail b)
            ord -> ord

compare_tl :: TL.Text -> TL.Text -> Ordering
compare_tl a b
    | TL.null a && TL.null b = EQ
    | TL.null a = LT
    | TL.null b = GT
    | otherwise =
        case caseInsensitiveCompare (TL.head a) (TL.head b) of
            EQ -> compare_tl (TL.tail a) (TL.tail b)
            ord -> ord

