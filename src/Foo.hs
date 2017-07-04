{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module Foo where

import qualified "base" Foreign.C.Types as C

foreign export ccall fooForeign ∷ C.CInt → C.CInt

foo ∷ Int → Int
foo = (* 2)

fooForeign ∷ C.CInt → C.CInt
fooForeign = fromIntegral . foo . fromIntegral
