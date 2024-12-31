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
    IMatrix(..)
) where

class IMatrix m a where
    unsafeRowSlice :: m a -> Int -> m a
    unsafeColSlice :: m a -> Int -> m a
