{-# LANGUAGE PackageImports , TupleSections , MultiParamTypeClasses , FlexibleInstances #-}

{- |

Run comparative benchmarks against other Haskell implementations.

The only other common implementation in Hackage is @case-insensitive@. That
algorithm and a simple one using @map toLower@ (and similar implementations)
are included in these benchmarks.

The random strings generated are limited to the printable ASCII characters.
The ratios of alphabetic and upper-case characters in the strings are part of
the randomization of the strings. The ratios chosen are shown below. The
character makeup of the test strings appears to have only slight effect on any
performance differences between the algorithms tested.

The lengths of the test strings are a little more impactful on the benches, so
two options are explored: short (random length) and long. The short strings are
likely what will be matched in the real world. The case folding routines
probably have similar performance, so the differences in string handling in
general and in allocation in particular exaggerate the difference in benchmark
performance. Even the lazily evaluated string in the @case-insensitive@ library
is noticeably slower because a second string is allocated.

The biggest difference in speed is for string comparisons that fail. When
matching short strings in a larger stream, the non-match is probably the
dominant case. The 'CaseInsensitiveEq' algorithm is by far the fastest.

Benchmarks available form a four-dimensional matrix accessible in the
tree-like, path-based style of @Criterion@'s defaultMain command line
handling.

> bench-others -m glob ByteString.Lazy/short/*/*

The axes and values are:

 - string type: data type of strings to compare

    -- @String@
    -- @ByteString@
    -- @ByteString.Lazy@
    -- @Text@
    -- @Text.Lazy@

 - string length:
 
    -- @short@    - 1000 different random-length (2-12 chars) strings
    -- @long@     - 20 different 500-char strings

 - string case: comparing the original with ...

    -- @id@        - identical strings
    -- @toLower@   - a lower-case version of the original
    -- @toUpper@   - an upper-case version of the original
    -- @flipCase@  - all alphabetic characters in the opposite case
    -- @similar@   - identical at the beginning, then different
    -- @different@ - different string from the first character

 - match algorithm:

    -- @FoldCase@           - compare two case-folded strings using @base@ library
    -- @CaseInsensitiveEq@  - compare using this library
    -- @CIeq@               - compare using the @case-insensitive@ package
    -- @CIeq'@              - a smarter use of @case-insensitive@, assuming one string is a literal in client code
    -- @FoldCaseOrd@        - ordering or two case-folded strings using the @base@ library
    -- @CaseInsensitiveOrd@ - ordering using this library

Note on @CIeq'@: There is some overhead to using 'CI.mk". In most cases, one of
the two strings being compared will be a string literal in the client source
code. We can assume that 'CI.mk' will be called for the literal at most once,
if not never, during execution. The @CIeq'@ comparison algorithm (sometimes the
fastest) models this case by evaluating one half of the comparison before the
benchmark begins. This might also be informative for the @FoldCase@ algorithm,
but it's not explored here.

-}

module Main ( main ) where

import           Test.RandomStrings
import           Test.RandomStrings.FlipCase
import           Criterion.Main

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Data.Ratio ( (%) )
import           Data.Word
import           Data.Char
import           Data.String ( IsString(..) )

import qualified "case-insensitive" Data.CaseInsensitive as CI
import           Data.CaseInsensitive.Eq
import           Data.CaseInsensitive.Ord




------------------------------------------------------------------------------
-- 
-- | 'main' first generates some random test strings and then organizes bench
--   groups in a tree organized by
--
--   @/ types / length / case / method@
--
main = do
    strings <- mapM mk_strings
        [ StringLengthShort
        , StringLengthLong
        ]

    defaultMain 
        [ bgroup "String"           $ map (mk_length ""       ) strings
        , bgroup "ByteString"       $ map (mk_length BS.empty ) strings
        , bgroup "ByteString.Lazy"  $ map (mk_length BSL.empty) strings
        , bgroup "Text"             $ map (mk_length T.empty  ) strings
        , bgroup "Text.Lazy"        $ map (mk_length TL.empty ) strings
        ]



------------------------------------------------------------------------------
-- 
-- generating random test strings
--

-- | Test strings are all printable characters in the 7-bit ASCII range. The
--   mix is 9/10 alphabetic and the alphabetic characters are 1/6 upper case.
--   If the mix of character classes made much difference in benchmark
--   results, I'd add these as command line parameters. To try a different
--   mix, modify it here.
string_generator = randomString' randomASCII (9%10) (1%6)

data StringLengthOption = StringLengthLong | StringLengthShort deriving Eq

mk_strings :: StringLengthOption -> IO (StringLengthOption,[String])
mk_strings StringLengthShort = randomStringsLen string_generator (2,12) 1000 >>= \s -> return (StringLengthShort,s)
mk_strings StringLengthLong  = randomStrings (string_generator 500) 20 >>= \s -> return (StringLengthLong,s)


------------------------------------------------------------------------------
--
-- | CaseOption describes the change made to the original string to make a
--   testable pair of strings. Most create a pair that should be equivalent
--   in case-insensitive comparisons.
data CaseOption = CaseId | CaseToLower | CaseToUpper | CaseFlipCase | CaseSimilar | CaseDifferent

-- | Creating the pairs used by the comparison algorithms. The extra 'Bool' is
--   what the result of a case-insensitive comparison should be.
mk_pair :: CaseOption -> String -> ((String,String),Bool)
mk_pair CaseId a = ((a,a),True)
mk_pair CaseToLower a = ((a,map toLower a),True)
mk_pair CaseToUpper a = ((a,map toUpper a),True)
mk_pair CaseFlipCase a = ((a,map flipCase a),True)
mk_pair CaseSimilar a = ((a, mk_similar a), False)
mk_pair CaseDifferent a = ((a,mk_different a),False)


-- | Make a different String after a 'random' number of chars.
--   Guaranteed to be different from the original.
mk_similar [] = []
mk_similar as@(a:_) = (take n as') ++ (map succ as'')
    where
        n = (fromEnum a) `mod` (length as)
        (as',as'') = splitAt n as

-- | Make a completely different string than the original.
mk_different = map succ


pack_pair :: (IsString a) => a -> ((String,String),b) -> ((a,a),b)
pack_pair _ ((a,b),c) = ((fromString a,fromString b),c)

ci_mk_pair :: (CI.FoldCase a) => ((a,a),b) -> ((CI.CI a,a),b)
ci_mk_pair ((a,b),c) = ((CI.mk a,b),c)

-- | make a bench group for a StringLengthOption
mk_length :: (BenchType a) => a -> (StringLengthOption,[String]) -> Benchmark
mk_length s (len_opt,strings) = bgroup (bench_name len_opt) $
    map (mk_case s strings)
        [ CaseId        -- the original string
        , CaseToLower   -- a lower-case version of the original
        , CaseToUpper   -- an upper-case version of the original
        , CaseFlipCase  -- a version with inverted case for alphabetic chars
        , CaseSimilar   -- a version that is initially equivalent, but not fully so
        , CaseDifferent -- a completely different string from the first character
        ]

------------------------------------------------------------------------------
--
-- The leaf nodes evaluate different comparison algorithms on the pairs of
-- randomly generated string. These are the actual benchmark tests, and the
-- pairs of strings being generated are fully constructed before the tests
-- because they are the final parameter of 'nf'.


-- | make a bench group for a CaseOption
mk_case :: (BenchType a) => a -> [String] -> CaseOption -> Benchmark
mk_case s strings case_opt = bgroup (bench_name case_opt)
    [ bench "FoldCase"           $ nf (map $ \((a,b),tf) -> (((fold_case_bench a) == (fold_case_bench b)) == tf || assert_fail)) $ (map (pack_pair s) pairs)
    , bench "CaseInsensitiveEq"  $ nf (map $ \((a,b),tf) -> ((caseInsensitiveEq a b                     ) == tf || assert_fail)) $ (map (pack_pair s) pairs)
    , bench "CIeq"               $ nf (map $ \((a,b),tf) -> (((CI.mk a) == (CI.mk b)                    ) == tf || assert_fail)) $ (map (pack_pair s) pairs)
    , bench "CIeq'"              $ nf (map $ \((a,b),tf) -> ((a == (CI.mk b)                            ) == tf || assert_fail)) $ (map ci_mk_pair $ map (pack_pair s) pairs)
    , bench "FoldCaseOrd"        $ nf (map $ \((a,b),tf) -> ((compare (fold_case_bench a) (fold_case_bench b) == EQ) == tf || assert_fail)) $ (map (pack_pair s) pairs)
    , bench "CaseInsensitiveOrd" $ nf (map $ \((a,b),tf) -> ((caseInsensitiveCompare a b                      == EQ) == tf || assert_fail)) $ (map (pack_pair s) pairs)
    ]
    where
        pairs = map (mk_pair case_opt) strings
        assert_fail = error "internal: assertion failed"


------------------------------------------------------------------------------
--
-- classes


-- | A class for string types to be benchmarked. Both 'CaseInsensitiveEq' and
--   'CI.FoldCase' are ready to be tested as-is, but the case-folding algorithm
--   that only relies on the @base@ package requires an implementation of
--   case-folding for each string type.
class (CI.FoldCase a, CaseInsensitiveEq a, Eq a, CaseInsensitiveOrd a, Ord a, IsString a) => BenchType a where
    fold_case_bench :: a -> a

instance BenchType String where
    fold_case_bench = map toLower

instance BenchType BS.ByteString where
    fold_case_bench = BS8.map toLower

instance BenchType BSL.ByteString where
    fold_case_bench = BSL8.map toLower

instance BenchType T.Text where
    fold_case_bench = T.map toLower

instance BenchType TL.Text where
    fold_case_bench = TL.map toLower



-- | A helper class for naming branches of the bench tree.
class NamedBench a where bench_name :: a -> String

instance NamedBench String where bench_name = const "String"
instance NamedBench BS.ByteString where bench_name = const "ByteString"
instance NamedBench BSL.ByteString where bench_name = const "ByteString.Lazy"
instance NamedBench T.Text where bench_name = const "Text"
instance NamedBench TL.Text where bench_name = const "Text.Lazy"

instance NamedBench StringLengthOption where
    bench_name StringLengthLong = "Long"
    bench_name StringLengthShort = "Short"

instance NamedBench CaseOption where
    bench_name CaseId = "id"
    bench_name CaseToLower = "toLower"
    bench_name CaseToUpper = "toUpper"
    bench_name CaseFlipCase = "flipCase"
    bench_name CaseSimilar = "similar"
    bench_name CaseDifferent = "different"

