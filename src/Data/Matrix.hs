{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Data.Matrix where

import GHC.Base 
import GHC.ST
import Data.MMatrix

data Matrix a = Matrix {-# UNPACK #-} !Int -- | dim
                       {-# UNPACK #-} !Int -- | row size
                       {-# UNPACK #-} !Int -- | col size
                       (MutableByteArray# a)

instance MMatrixPure Matrix a where
    dims (Matrix _ r c _) = (r,c) 
    length (Matrix d _ _ _) = d
    rowLength (Matrix _ r _ _) = r
    colLength (Matrix _ _ c _) = c
    unsafeRowSlice (Matrix d r c _) i j
        | i > r || j > c || i * j > d = error "Data.MMAtrix.rowSlice: index out of bounds" 
        | otherwise = undefined 
    unsafeColSlice (Matrix d r c _) i j
        | i > r || j > c || i * j > d = error "Data.MMAtrix.colSlice: index out of bounds" 
        | otherwise = undefined

instance MMatrix Matrix (ST s) a where
    {-# INLINE unsafeNew #-}
    unsafeNew = unsafeNew'

unsafeNew' :: Int -> Int -> ST s (Matrix a)
unsafeNew' (I# r#) (I# c#) = 
    ST (\s# -> 
        case newByteArray# (r# *# c#) s# of
            (# s1#, arr# #) -> undefined)
    
