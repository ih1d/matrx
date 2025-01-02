{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}

{- |
Module      : Data.Unboxed
Copyright   : (c) Isaac H. Lopez Diaz 2024
License     : BSD-style

Maintainer  : isaac.lopez@upr.edu
Stability   : experimental
Portability : non-portable

Mutable Matrices
-}
module Data.Matrix.Mutable where

import Data.MMatrix
import GHC.Base (Int (..), MutableArray#, newArray#, readArray#, writeArray#, (*#))
import GHC.ST

data Matrix s a
    = Matrix
        {-# UNPACK #-} !Int
        -- | dim
        {-# UNPACK #-} !Int
        -- | row size
        {-# UNPACK #-} !Int
        -- | col size
        (MutableArray# s a)

instance MMatrixPure (Matrix s) a where
    dims (Matrix _ r c _) = (r, c)
    size (Matrix d _ _ _) = d
    rowLength (Matrix _ r _ _) = r
    colLength (Matrix _ _ c _) = c

instance MMatrix (Matrix s) (ST s) a where
    {-# INLINE unsafeNew #-}
    unsafeNew = unsafeNew'

    {-# INLINE unsafeRead #-}
    unsafeRead (Matrix _ _ _ arr#) (I# i#) = ST (readArray# arr# i#)

    {-# INLINE unsafeWrite #-}
    unsafeWrite (Matrix d r c arr#) (I# i#) a =
        ST
            ( \s# ->
                case writeArray# arr# i# a s# of
                    s2# -> (# s2#, () #)
            )

    {-# INLINE unsafeRowSlice #-}
    unsafeRowSlice = unsafeRowSlice'

    {-# INLINE unsafeColSlice #-}
    unsafeColSlice = undefined

unsafeNew' :: Int -> Int -> a -> ST s (Matrix s a)
{-# INLINE unsafeNew' #-}
unsafeNew' (I# r#) (I# c#) a =
    ST
        ( \s# ->
            case newArray# (r# *# c#) a s# of
                (# s'#, arr# #) -> (# s'#, Matrix (I# (r# *# c#)) (I# r#) (I# c#) arr# #)
        )


unsafeRowSlice :: Matrix a -> Int -> Int -> ST s (Matrix s a)
{-# INLINE unsafeRowSlice #-} 
unsafeRowSlice' mat i n = undefined