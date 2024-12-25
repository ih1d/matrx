{- Module      : Data.Matrix
-- Copyright   : (c) Isaac H. Lopez Diaz 2024
-- License     : BSD-style
--
-- Maintainer  : isaac.lopez@upr.edu
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Native matrices
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Matrix where

-- Matrix data structure
data Matrix a = Matrix 
    { row :: Int 
    , col :: Int 
    , elems :: Eq a => [[a]]
    }

instance Semigroup (Matrix a) where
    (<>) = undefined

instance Monoid (Matrix a) where
    mempty = Matrix 0 0 []
    mappend = undefined

{- Matrix Creation -}
-- | empty matrix construction
empty :: Matrix a
empty = Matrix 0 0 []

-- | Given a row and a column with a list of
-- elements a matrix data structure is returned.
-- If the list is empty the zero matrix is returned
-- Otherwise matrix is built
matrix :: Int -> Int -> [a] -> Matrix a
matrix r c [] = Matrix r c []
matrix r c m
    | r*c /= length m = error "Matrix indices do not match list length"
    | otherwise = 
        let r1 = take c m
         in Matrix r c ([r1] ++ loop c (drop c m))
        where
            loop _ [] = []
            loop n xs = [take n xs] ++ loop n (drop c xs) 


-- | transposition
-- transposes the matrices elements
transpose :: Matrix a -> Matrix a
transpose (Matrix r c elems) = 
    if r == 1 
        then (Matrix c r elems)
        else (Matrix c r (transposeIter elems []))
    where 
        transposeIter xs res =
            let hds = map head xs
                tls = map tail xs
             in 
                if concat tls == [] 
                    then res ++ [hds]
                    else transposeIter tls (res ++ [hds])
