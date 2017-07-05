{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}

module Baz.Qux
  ( quux
  ) where

import "base-unicode-symbols" Prelude.Unicode


quux ∷ Int → Int
quux = (⋅ 100)
