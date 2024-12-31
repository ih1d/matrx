{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Matrix
-- Copyright   : (c) Isaac H. Lopez Diaz 2024
-- License     : BSD-style
--
-- Maintainer  : isaac.lopez@upr.edu
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Matrix interface
--

module Data.IMatrix (
    IMatrix(..),
    length,
    empty,
    fromList,
    toList,
) where

import Prelude hiding (length)
import Data.MMatrix

class IMatrix mx a where
    mlength :: mx a -> Int

length :: IMatrix mx a => mx a -> Int
{-# INLINE length #-}
length mx = mlength mx

empty :: IMatrix mx a => mx a
{-# INLINE empty #-}
empty = fromList []

fromList :: IMatrix mx a => [a] -> mx a
{-# INLINE fromList #-}
fromList = 
