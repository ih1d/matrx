{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Matrix.Internal
-- Copyright   : (c) Isaac H. Lopez Diaz 2024
-- License     : BSD-style
--
-- Maintainer  : isaac.lopez@upr.edu
-- Stability   : experimental
-- Portability : non-portable

module Data.Matrix.Internal where

import Data.Matrix.Class
import Data.Primitive.ByteArray

data Matrix a = Matrix Int Int [ByteArray]

instance MatrixPure Matrix a where
    dims (Matrix m n _) = (m,n)
