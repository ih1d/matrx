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

module Data.IMatrix where

class IMatrix m a where
    rowSlice :: m a -> Int -> m a
    colSlice :: m a -> Int -> m a
    index :: m a -> Int -> Int -> a
    shape :: m a -> (Int, Int)
    size :: m a -> Int
