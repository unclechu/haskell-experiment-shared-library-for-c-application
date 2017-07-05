{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module Bar where

import "base-unicode-symbols" Prelude.Unicode
import qualified "base" Foreign.C.Types as C

import Baz.Qux (quux)

foreign export ccall barForeign ∷ C.CInt → C.CInt

bar ∷ Int → Int
bar = (⋅ 4) ∘ quux

barForeign ∷ C.CInt → C.CInt
barForeign = fromIntegral ∘ bar ∘ fromIntegral
