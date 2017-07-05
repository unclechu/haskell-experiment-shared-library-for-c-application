{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module Bar where

import "base-unicode-symbols" Prelude.Unicode
import qualified "base" Foreign.C.Types as C

import Baz.Qux (quux)

foreign export ccall barForeign ∷ C.CInt → C.CInt

barForeign ∷ C.CInt → C.CInt
barForeign = (⋅ 4) ∘ quux
