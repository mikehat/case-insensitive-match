
### case-insensitive-match 0.1.1.1

Here is a simplified library for matching and comparing strings in a
case-insensitive manner. The only dependencies are `base`, `bytestring` and
`text`.

Usage is simple

    -- normal string comparison
    "href"    /=  "HREF"
    "apples"  /=  "oranges"
    "Smith"   <   "Jones

    -- case-insensitive comparison
    "href"    ^== "HREF"
    "appples" ^/= "oranges"
    "jones"   ^<  "Smith"

    -- sorting some data structurue
    get_names p = (last_name p,first_name p)
    sortBy (caseInsensitiveComparing get_names) people

#### Work in progress

- revisit Word8 comparison functions
- explore some ByteString.Internal insanity
- add a benchmark to evaluate internal changes

#### Benchmarks

The benchmarks are pretty comprehensive, offering comparisons with other
algorithms, including the `case-insensitive` package and simple case folding
using the `base` package. Before simply running the `bench-others` executable,
check the source code or you'll end up with a long series of 360 benchmark
tests. You'll want something like `bench-others -m glob ByteString/Short/*/*`,
which runs _only_ 36 benchmarks. The heirarchy is
`<data-type>/<string-length>/<match-type>/<algorithm>`. As usual, performance
comparisons depend heavily on use-cases, but for matching shorter strings that
are often unequal this algorithm is clearly fastest.

There is also a real-world bench test that compares different algorithms while
looking for links in an HTML file with Text.HTML.TagSoup. This bench involves
a lot of work other than string comparison, so the differences between
algorithms is slim, but usually measurable. Build and run:

    $ cabal build bench-tagsoup
    ...

    $ curl -s 'https://hackage.haskell.org/packages/names' > sample/hackage-names.html
    $ dist/build/bench-tagsoup/bench-tagsoup < sample/hackage-names.html
    ...


#### Testing

It would be quite involved to build a perfectly comprehensive testing module,
but the `test-basics` executable is tests multiple cases against all supported
data types.


#### Sample

Here is a sample:

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


Try it with:

    $ cabal build readme-sample
    ...
    
    $ dist/build/readme-sample/readme-sample < sample/declaration-signers.txt


