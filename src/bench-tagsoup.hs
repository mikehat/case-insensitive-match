{-# LANGUAGE OverloadedStrings , PackageImports #-}


{- | Benchmarking TagSoup to extract a/@href from a web page. The results are
     not at all shocking, but do show differences between packages. The easier
     syntax and slight speed improvement of the @case-insensitive-match@
     package. The time differences seem consistent, showing that when parsing
     an html page there is significant time doing case-insensitive string
     comparison.

     Note: I'm not sure why Criterion shows a faster run the first time tags
     are parsed. I added an @init@ bench to make the others look normal.  The
     same thing usually happens no matter the matching algorithm. You can try
     running only one bench at a time and comparing the higher-speed results.

-}
module Main ( main ) where

import           Criterion.Main
import           Data.String

import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString ( ByteString )

import qualified "case-insensitive" Data.CaseInsensitive as CI
import           Data.CaseInsensitive.Eq

main = do
    html <- BS.getContents
    let tags = parseTags html
    if True -- set to False to actually see the list of HREFs
        then
            defaultMain
                [ bgroup "match-only"
                    [ bench "init"              $ nf (findLinks (^==) ) tags
                    , bench "=="                $ nf (findLinks (==)  ) tags
                    , bench "CaseInsensitiveEq" $ nf (findLinks (^==) ) tags
                    , bench "CIeq"              $ nf (findLinks ciEq  ) tags
                    , bench "CIeq'"             $ nf (findLinks ciEq' ) tags
                    ]
                , bgroup "parse-and-match"
                    [ bench "init"              $ nf (findLinks (^==) . parseTags ) html
                    , bench "=="                $ nf (findLinks (==)  . parseTags ) html
                    , bench "CaseInsensitiveEq" $ nf (findLinks (^==) . parseTags ) html
                    , bench "CIeq"              $ nf (findLinks ciEq  . parseTags ) html
                    , bench "CIeq'"             $ nf (findLinks ciEq' . parseTags ) html
                    ]
                ]
        else
            mapM_ BS8.putStrLn $ findLinks (^==) tags

-- see bench-others for the reasoning behind these two algorithms
ciEq a b = (CI.mk a) == (CI.mk b)
ciEq' a b = a == (CI.mk b)

findLinks :: (IsString a) => (a -> ByteString -> Bool) -> [Tag ByteString] -> [ByteString]
findLinks eq [] = []
findLinks eq (tag:tags)
    | tagOpen (eq "a") (const True) tag = get_tag_href eq tag : findLinks eq tags
    | otherwise = findLinks eq tags

get_tag_href eq (TagOpen _ attrs) = get_href eq attrs
get_tag_href _ _ = error "internal: tagOpen failed!"

get_href _ [] = BS.empty
get_href eq ((n,v):attrs)
    | eq "href" n = v
    | otherwise = get_href eq attrs

