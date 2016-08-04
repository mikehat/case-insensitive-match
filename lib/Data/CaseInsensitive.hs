

{- |
module:         Data.CaseInsensitive
description:    All the funtionality of the "Data.CaseInsensitive.Eq" and
                "Data.CaseInsensitive.Ord" modules.

This module creates an impolite naming conflict with the @case-insensitive@
package. If using @cabal@ to build, it's not a problem unless asked for in the
@build-depends@ directive. It is probably best to import only the
"Data.CaseInsensitive.Eq" module if that is the only functionality needed.

-}

module Data.CaseInsensitive
    ( module Data.CaseInsensitive.Eq
    , module Data.CaseInsensitive.Ord
    )
where

import Data.CaseInsensitive.Eq
import Data.CaseInsensitive.Ord

