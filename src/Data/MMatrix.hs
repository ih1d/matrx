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

module Data.MMatrix (
    MMatrixPure(..),
    MMatrix(..),
    new, read, write,
    unlist,
    rowSlice, colSlice,
) where

import Control.Exception (assert)
import Prelude hiding (read)

class MMatrixPure mx a where
    -- | dimensions of matrix
    dims :: mx a -> (Int, Int)
    -- | size of the array of the matrix
    size :: mx a -> Int
    -- | size of rows
    rowLength :: mx a -> Int
    -- | size of columns
    colLength :: mx a -> Int

class (Monoid a, Monad m, MMatrixPure mx a) => MMatrix mx m a where
    -- | Create a new matrix of a given dimension
    unsafeNew :: Int -> Int -> m (mx a)
    -- | Return element at given position
    unsafeRead :: mx a -> Int -> m a
    -- | Replace element at given position
    unsafeWrite :: mx a -> Int -> a -> m ()
    -- | slice rows from starting idx to some idx
    unsafeRowSlice :: mx a -> Int -> Int -> m (mx a)
    -- | slice cols from starting idx to some idx
    unsafeColSlice :: mx a -> Int -> Int -> m (mx a)


-- | Test that index given is valid
inBounds :: MMatrixPure mx a => mx a -> Int -> Bool
inBounds mx i = i >= 0 && i < size mx

new :: MMatrix mx m a => Int -> Int -> m (mx a)
new r c = assert (r >= 0 && c >= 0) $ unsafeNew r c

read :: MMatrix mx m a => mx a -> Int -> m a
read mx i = assert (inBounds mx i) $ unsafeRead mx i

write :: MMatrix mx m a => mx a -> Int -> a -> m ()
write mx i a = assert (inBounds mx i) $ unsafeWrite mx i a

-- | Return row part of the matrix
rowSlice :: MMatrix mx m a => mx a -> Int -> Int -> m (mx a)
rowSlice mx i n = assert (i >= 0 && n >= 0 && i <= n && i+n <= size mx) (unsafeRowSlice mx i n)

-- | Return column part of the matrix
colSlice :: MMatrix mx m a => mx a -> Int -> Int -> m (mx a)
colSlice mx i n = assert (i >= 0 && n >= 0 && i <= n && i+n <= size mx) (unsafeColSlice mx i n)

-- | Create a new matrix from a list
unlist :: (Monoid a, MMatrix mx m a) => Int -> Int -> [a] -> m (mx a)
unlist r c xs = new r c >>= loop (zip xs [0..])
    where
        loop :: (Monoid a, MMatrix mx m a) => [(a,Int)] -> mx a -> m (mx a)
        loop [] arr = pure arr
        loop ((x, i):xs) arr = write arr i x >> loop xs arr
