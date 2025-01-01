{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Data.Matrix
-- Copyright   : (c) Isaac H. Lopez Diaz 2024
-- License     : BSD-style
--
-- Maintainer  : isaac.lopez@upr.edu
-- Stability   : experimental
-- Portability : non-portable
-- 

module Data.Matrix (
    module Data.IMatrix,
    Matrix(..),
) where

import GHC.Base (Array#, unsafeFreezeArray#)
import Data.IMatrix
import Data.Matrix.Mutable qualified as Mut
import Prelude hiding (length)
import Control.Monad.ST (runST)
import GHC.ST

-- Matrix data structure
data Matrix a = Matrix {-# UNPACK #-} !Int -- | dim
                       {-# UNPACK #-} !Int -- | row size
                       {-# UNPACK #-} !Int -- | col size
                       (Array# a)

instance Monoid a => IMatrix Matrix a where
    {-# INLINE mnew #-}
    mnew init = runST (do
                       Mut.Matrix d r c marr# <- init
                       ST (\s# -> case unsafeFreezeArray# marr# s# of
                               (# s2#, arr# #) -> (# s2#, Matrix d r c arr# #)))

    msize (Matrix d _ _ _) = d 
    mshape (Matrix _ r c _) = (r,c) 
