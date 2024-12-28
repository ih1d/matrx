{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Data.Matrix.Internal
-- Copyright   : (c) Isaac H. Lopez Diaz 2024
-- License     : BSD-style
--
-- Maintainer  : isaac.lopez@upr.edu
-- Stability   : experimental
-- Portability : non-portable

module Data.Matrix.Class where

-- | Operations on pure mutable matrices
class MatrixPure mat a where
    dims :: mat a -> (Int, Int)
    rowSlice :: Int -> mat a -> mat a 
    colSlice :: Int -> mat a -> mat a
