{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}

module Baz.Qux
  ( quux
  ) where

import "base-unicode-symbols" Prelude.Unicode
import "base" Foreign.C.Types (CInt)


quux ∷ CInt → CInt
quux = (⋅ 100)
