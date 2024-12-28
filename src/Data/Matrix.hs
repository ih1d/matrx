{-# LANGUAGE MagicHash #-}
-- |
-- Module      : Data.Matrix
-- Copyright   : (c) Isaac H. Lopez Diaz 2024
-- License     : BSD-style
--
-- Maintainer  : isaac.lopez@upr.edu
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Native matrices

module Data.Matrix where

import Data.Primitive.ByteArray

type NDArray a = MutableByteArray# (MutableByteArray a)

data Matrix a = Matrix {-# UNPACK #-} !Int 
                       {-# UNPACK #-} !Int 
                                      (NDArray a)
