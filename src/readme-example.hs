{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.List
import           Data.CaseInsensitive
import qualified Data.ByteString.Char8 as BS

main = do
    stdin <- BS.getContents
    let sorted_names = map join_name $ sortBy caseInsensitiveCompare $ map split_name $ BS.lines stdin
    mapM_ BS.putStrLn sorted_names


split_name name = (last,BS.drop 2 first)
    where (last,first) = BS.span (/= ',') name

join_name (last,first) = BS.concat [ last , ", " , first ]

