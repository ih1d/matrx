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
-- Matrix mutable interface
--

module Data.MMatrix where

import Control.Exception (assert)
import Prelude hiding (length)

class MMatrixPure mx a where
    -- | dimensions of matrix
    dims :: mx a -> (Int, Int)
    -- | length of the array of the matrix
    length :: mx a -> Int
    -- | length of rows
    rowLength :: mx a -> Int
    -- | length of columns
    colLength :: mx a -> Int
    -- | slice rows from starting idx to some idx
    unsafeRowSlice :: mx a -> Int -> Int -> mx a
    -- | slice cols from starting idx to some idx
    unsafeColSlice :: mx a -> Int -> Int -> mx a

class (Monad m, MMatrixPure mx a) => MMatrix mx m a where
    -- | Create a new matrix of a given dimension
    unsafeNew :: Int -> Int -> m (mx a)

    -- | Return element at given position
    unsafeRead :: mx a -> Int -> m a

    -- | Replace element at given position
    unsafeWrite :: mx a -> Int -> a -> m ()

-- | Test that index given is valid
inBounds :: MMatrixPure mx a => mx a -> Int -> Bool
inBounds mx i = i >= 0 && i < length mx

-- | Return row part of the matrix
rowSlice :: MMatrixPure mx a => mx a -> Int -> Int -> mx a
rowSlice mx i n = assert (i >= 0 && n >= 0 && i <= n && i+n <= length mx) (unsafeRowSlice mx i n)

-- | Return column part of the matrix
colSlice :: MMatrixPure mx a => mx a -> Int -> Int -> mx a
colSlice mx i n = assert (i >= 0 && n >= 0 && i <= n && i+n <= length mx) (unsafeColSlice mx i n)

new :: MMatrix mx m a => Int -> Int -> m (mx a)
new r c = assert (r >= 0 && c >= 0) $ unsafeNew r c

read :: MMatrix mx m a => mx a -> Int -> m a
read mx i = assert (inBounds mx i) $ unsafeRead mx i

write :: MMatrix mx m a => mx a -> Int -> a -> m ()
write mx i a = assert (inBounds mx i) $ unsafeWrite mx i a
