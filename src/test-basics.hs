
{-

-}

module Main ( main ) where


import           Data.Char
import           Data.Word
import           Data.Maybe ( catMaybes )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Control.Monad.State
import           System.Exit

import           Data.CaseInsensitive


main = do
    success <- flip execStateT True $ do
        doTest "all Char Eq"                            test_AllCharEqChar
        doTest "all Word8 Eq"                           test_AllCharEqWord8
        doTest "all Char Eq String"                     test_AllCharEqString
        doTest "all Char using caseInsensitiveMatch"    test_AllCharMatchChar
        doTest "all Word8 Eq Strict ByteString"         test_AllCharEqBS
        doTest "all Word8 Eq Lazy ByteString"           test_AllCharEqBSL
        doTest "unequal String cases"                   test_NotEqualString
        doTest "unequal ByteString cases"               test_NotEqualBS
        doTest "unequal Lazy ByteString cases"          test_NotEqualBSL
        doTest "unequal Text cases"                     test_NotEqualT
        doTest "unequal Lazy Text cases"                test_NotEqualTL
        doTest "equal String cases"                     test_EqualString
        doTest "equal ByteString cases"                 test_EqualBS
        doTest "equal Lazy ByteString cases"            test_EqualBSL
        doTest "equal Text cases"                       test_EqualT
        doTest "equal Lazy Text cases"                  test_EqualTL
        doTest "equal ByteStrirg tuple"                 test_EqualTupleBS
        doTest "compare all Char EQ"                    test_CompareAllCharEQChar
        doTest "compare all Char8 EQ"                   test_CompareAllCharEQWord8
        doTest "compare all String EQ"                  test_CompareAllCharEQString
        doTest "compare all ByteString EQ"              test_CompareAllByteStringEQ
        doTest "compare unequal String cases"           test_OrderString
        doTest "compare unequal ByteString cases"       test_OrderByteString
        doTest "compare unequal Text cases"             test_OrderText
        doTest "compare equal String cases"             test_CompareEQString
        doTest "compare equal ByteString cases"         test_CompareEQByteString
        doTest "compare equal Text cases"               test_CompareEQText
        doTest "compare equal ByteString tuple"         test_CompareEQTupleBS
        doTest "compare unequal ByteString tuple"       test_CompareGTTupleBS

    if success then exitSuccess else exitFailure

-- | All tests are of the simple pass/fail variety. This is meant
--   at a certain level to emulate QuickCheck.
doTest :: String -> Bool -> StateT Bool IO ()
doTest name test = do
    liftIO $ putStr $ take 40 $ concat [ name , ":" , repeat ' ' ]
    if test
        then do
            liftIO $ putStrLn "+++ OK, passed"
        else do
            liftIO $ putStrLn "--- Failed!"
            put False


------------------------------------------------------------------------------
-- 
-- Testing all character values for each instance. 
--
--

-- Test the case-folding behavior of one-at-a-time character matching.  A
-- highly technical implementation would not match Unicode Char and String
-- using the same algorithm. The instance for Char is pragmatic enough to pass,
-- but this is a test that the case-insensitive package fails.

test_AllCharEqChar :: Bool
test_AllCharEqChar =
    all id
        [ all (uncurry caseInsensitiveEq) $ zip all_chars all_chars
        , all (uncurry caseInsensitiveEq) $ zip all_chars all_uppers
        , all (uncurry caseInsensitiveEq) $ zip all_chars all_lowers
        , not $ all_chars == all_uppers -- validating toUpper, actually
        , not $ all_chars == all_lowers
        ]
    where
        all_chars = [minBound..maxBound] :: [Char]
        all_uppers = map toUpper all_chars
        all_lowers = map toLower all_chars

test_AllCharEqWord8 :: Bool
test_AllCharEqWord8 =
    all id
        [ all (uncurry caseInsensitiveEq) $ zip all_chars all_chars
        , all (uncurry caseInsensitiveEq) $ zip all_chars all_uppers
        , all (uncurry caseInsensitiveEq) $ zip all_chars all_lowers
        , not $ all_chars == all_uppers -- validating fold_8 toUpper, actually
        , not $ all_chars == all_lowers
        ]
    where
        all_chars = [minBound..maxBound] :: [Word8]
        all_uppers = map (fold_8 toUpper) all_chars
        all_lowers = map (fold_8 toLower) all_chars

test_AllCharMatchChar :: Bool
test_AllCharMatchChar =
    all id
        [ all_chars `caseInsensitiveMatch` all_chars
        , all_chars `caseInsensitiveMatch` all_uppers
        , all_chars `caseInsensitiveMatch` all_lowers
        , not $ all_chars == all_uppers -- validating map toUpper, actually
        , not $ all_chars == all_lowers
        ]
    where
        all_chars = [minBound..maxBound] :: [Char]
        all_uppers = map toUpper all_chars
        all_lowers = map toUpper all_chars

test_AllCharEqString :: Bool
test_AllCharEqString =
    all id
        [ all_chars `caseInsensitiveEq` all_chars
        , all_chars `caseInsensitiveEq` all_uppers
        , all_chars `caseInsensitiveEq` all_lowers
        , not $ all_chars == all_uppers -- validating map toUpper, actually
        , not $ all_chars == all_lowers
        ]
    where
        all_chars = [minBound..maxBound] :: [Char]
        all_uppers = map toUpper all_chars
        all_lowers = map toUpper all_chars

test_AllCharEqBS :: Bool
test_AllCharEqBS =
    all id
        [ all_chars `caseInsensitiveEq` all_chars
        , all_chars `caseInsensitiveEq` all_uppers
        , all_chars `caseInsensitiveEq` all_lowers
        , not $ all_chars == all_uppers -- validating fold_8 toUpper, actually
        , not $ all_chars == all_lowers
        ]
    where
        all_chars = BS.pack $ [minBound..maxBound]
        all_uppers = BS.map (fold_8 toUpper) all_chars
        all_lowers = BS.map (fold_8 toLower) all_chars

test_AllCharEqBSL :: Bool
test_AllCharEqBSL =
    all id
        [ all_chars `caseInsensitiveEq` all_chars
        , all_chars `caseInsensitiveEq` all_uppers
        , all_chars `caseInsensitiveEq` all_lowers
        , not $ all_chars == all_uppers -- validating fold_8 toUpper, actually
        , not $ all_chars == all_lowers
        ]
    where
        all_chars = BSL.pack $ [minBound..maxBound]
        all_uppers = BSL.map (fold_8 toUpper) all_chars
        all_lowers = BSL.map (fold_8 toLower) all_chars

------------------------------------------------------------------------------
--
-- Ord
--

test_CompareAllCharEQChar :: Bool
test_CompareAllCharEQChar =
    all id
        [ all ((==EQ) . uncurry caseInsensitiveCompare) $ zip all_chars all_chars
        , all ((==EQ) . uncurry caseInsensitiveCompare) $ zip all_chars all_uppers
        , all ((==EQ) . uncurry caseInsensitiveCompare) $ zip all_chars all_lowers
        ]
    where
        all_chars = [minBound..maxBound] :: [Char]
        all_uppers = map toUpper all_chars
        all_lowers = map toLower all_chars


test_CompareAllCharEQWord8 :: Bool
test_CompareAllCharEQWord8 =
    all id
        [ all ((==EQ) . uncurry caseInsensitiveCompare) $ zip all_chars all_chars
        , all ((==EQ) . uncurry caseInsensitiveCompare) $ zip all_chars all_uppers
        , all ((==EQ) . uncurry caseInsensitiveCompare) $ zip all_chars all_lowers
        ]
    where
        all_chars = [minBound..maxBound] :: [Word8]
        all_uppers = map (fold_8 toUpper) all_chars
        all_lowers = map (fold_8 toLower) all_chars

test_CompareAllCharEQString :: Bool
test_CompareAllCharEQString =
    all id
        [ caseInsensitiveCompare all_chars all_chars == EQ
        , caseInsensitiveCompare all_chars all_uppers == EQ
        , caseInsensitiveCompare all_chars all_lowers == EQ
        ]
    where
        all_chars = [minBound..maxBound] :: [Char]
        all_uppers = map toUpper all_chars
        all_lowers = map toLower all_chars

test_CompareAllByteStringEQ :: Bool
test_CompareAllByteStringEQ =
    all id
        [ caseInsensitiveCompare all_chars all_chars == EQ
        , caseInsensitiveCompare all_chars all_uppers == EQ
        , caseInsensitiveCompare all_chars all_lowers == EQ
        ]
    where
        all_chars = BS.pack $ [minBound..maxBound]
        all_uppers = BS.map (fold_8 toUpper) all_chars
        all_lowers = BS.map (fold_8 toLower) all_chars



------------------------------------------------------------------------------
--
-- A few test cases with strings that should not match.
--

neq_cases =
    [ (( "apples" , "oranges" ) , LT)
    , (( "apples" , "apple"   ) , GT)
    , (( "123"    , ""        ) , GT)
    , (( ""       , "abc"     ) , LT)
    , (( a_to_z   , z_to_a    ) , LT)
    ]
    where
        a_to_z = ['a'..'z']
        z_to_a = reverse a_to_z 


neq_pairs = map fst neq_cases

neq_cases_bs = map (\((a,b),c) -> ((BS.pack a,BS.pack b),c)) $ cases_to_word8 neq_cases
neq_pairs_bs  = map (\(a,b) -> (BS.pack a ,BS.pack b )) $ pairs_to_word8 neq_pairs

neq_cases_bsl = map (\((a,b),c) -> ((BSL.pack a,BSL.pack b),c)) $ cases_to_word8 neq_cases
neq_pairs_bsl = map (\(a,b) -> (BSL.pack a,BSL.pack b)) $ pairs_to_word8 neq_pairs

neq_cases_t = map (\((a,b),c) -> ((T.pack a,T.pack b),c)) $ neq_cases
neq_pairs_t  = map (\(a,b) -> (T.pack a ,T.pack b )) $ neq_pairs

neq_cases_tl = map (\((a,b),c) -> ((TL.pack a,TL.pack b),c)) $ neq_cases
neq_pairs_tl = map (\(a,b) -> (TL.pack a,TL.pack b)) $ neq_pairs

test_NotEqualString :: Bool
test_NotEqualString = all not $ map (uncurry caseInsensitiveEq) neq_pairs

test_NotEqualBS :: Bool
test_NotEqualBS = all not $ map (uncurry caseInsensitiveEq) neq_pairs_bs

test_NotEqualBSL :: Bool
test_NotEqualBSL = all not $ map (uncurry caseInsensitiveEq) neq_pairs_bsl

test_NotEqualT :: Bool
test_NotEqualT = all not $ map (uncurry caseInsensitiveEq) neq_pairs_t

test_NotEqualTL :: Bool
test_NotEqualTL = all not $ map (uncurry caseInsensitiveEq) neq_pairs_tl

test_OrderString :: Bool
test_OrderString = all id $ map (\((a,b),c) -> caseInsensitiveCompare a b == c) neq_cases

test_OrderByteString :: Bool
test_OrderByteString = all id $ map (\((a,b),c) -> caseInsensitiveCompare a b == c) neq_cases_bs

test_OrderText :: Bool
test_OrderText = all id $ map (\((a,b),c) -> caseInsensitiveCompare a b == c) neq_cases_t

------------------------------------------------------------------------------
--
-- A few test cases with strings that should match.
--

case_eq_pairs =
    [ ( "apples"        , "apples"       )
    , ( "example.com"   , "EXAMPLE.com"  )
    , ( "HTML"          , "html"         )
    , ( "Content-Type"  , "CONTENT-TYPE" )
    , ( "C:\\Windows"   , "c:\\windows"  )
    ]

case_eq_pairs_bs  = map (\(a,b) -> (BS.pack a ,BS.pack b )) $ pairs_to_word8 case_eq_pairs
case_eq_pairs_bsl = map (\(a,b) -> (BSL.pack a,BSL.pack b)) $ pairs_to_word8 case_eq_pairs

case_eq_pairs_t  = map (\(a,b) -> (T.pack a ,T.pack b )) $ case_eq_pairs
case_eq_pairs_tl = map (\(a,b) -> (TL.pack a,TL.pack b)) $ case_eq_pairs

test_EqualString :: Bool
test_EqualString = all id $ map (uncurry caseInsensitiveEq) case_eq_pairs

test_EqualBS :: Bool
test_EqualBS = all id $ map (uncurry caseInsensitiveEq) case_eq_pairs_bs

test_EqualBSL :: Bool
test_EqualBSL = all id $ map (uncurry caseInsensitiveEq) case_eq_pairs_bsl

test_EqualT :: Bool
test_EqualT = all id $ map (uncurry caseInsensitiveEq) case_eq_pairs_t

test_EqualTL :: Bool
test_EqualTL = all id $ map (uncurry caseInsensitiveEq) case_eq_pairs_tl

test_CompareEQString :: Bool
test_CompareEQString = all id $ map ((==EQ) . uncurry caseInsensitiveCompare) case_eq_pairs

test_CompareEQByteString :: Bool
test_CompareEQByteString = all id $ map ((==EQ) . uncurry caseInsensitiveCompare) case_eq_pairs_bs

test_CompareEQText :: Bool
test_CompareEQText = all id $ map ((==EQ) . uncurry caseInsensitiveCompare) case_eq_pairs_t

------------------------------------------------------------------------------
--
-- tuple instances - testing all recursively
--

test_EqualTupleBS :: Bool
test_EqualTupleBS =
    ("apple","oranges","me@example.com","Content-Type")
    ^==
    ("APPLE","Oranges","me@EXAMPLE.COM","content-type")

test_CompareEQTupleBS :: Bool
test_CompareEQTupleBS =
    ( caseInsensitiveCompare
        ("apple","oranges","me@example.com","Content-Type")
        ("APPLE","Oranges","me@EXAMPLE.COM","content-type")
    ) == EQ


test_CompareGTTupleBS :: Bool
test_CompareGTTupleBS =
    ( caseInsensitiveCompare
        ("apple","oranges","me@example.com",("Content-Type","text/html"))
        ("APPLE","Oranges","me@EXAMPLE.COM",("content-language","en-us"))
    ) == GT


------------------------------------------------------------------------------
--
-- utility functions for creating Word8 strings
--

char_to_word8 :: Char -> Maybe Word8
char_to_word8 c
    | fromEnum c > 256 = Nothing
    | otherwise = Just $ fromIntegral $ fromEnum c

string_to_word8s :: String -> [Word8]
string_to_word8s = catMaybes . map char_to_word8

cases_to_word8 = map (\((a,b),c) -> ((string_to_word8s a,string_to_word8s b),c))
pairs_to_word8 = map (\(a,b) -> (string_to_word8s a , string_to_word8s b))


-- Some of the lower-256 actually have upper-case outside that range. Here,
-- we treat those as if they didn't have any case at all.
fold_8 :: (Char -> Char) -> Word8 -> Word8
fold_8 f c
    | ( fromEnum $ f $ toEnum $ fromIntegral c) > 255 = c
    | otherwise = fromIntegral $ fromEnum $ f $ toEnum $ fromIntegral c
