{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Data.Matrix
-- Copyright   : (c) Isaac H. Lopez Diaz 2024
-- License     : BSD-style
--
-- Maintainer  : isaac.lopez@upr.edu
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Matrix interface for pure matrices
--

module Data.IMatrix (
    IMatrix(..),
    -- creation
    empty, zeros,
    -- basic ops
    size, shape,
    -- to/from list
    fromList, toList
) where

import Data.MMatrix (MMatrix)

class IMatrix mx a where
    mnew :: (forall mmx m. MMatrix mmx m a => m (mmx a)) -> mx a
    msize :: mx a -> Int
    mshape :: mx a -> (Int, Int)

size :: IMatrix mx a => mx a -> Int
size = msize

shape :: IMatrix mx a => mx a -> (Int, Int)
shape = mshape

-- * Matrix Creation 

-- | empty matrix construction
empty :: IMatrix mx a => mx a
empty = fromList [] 

-- | zeros creates a matrix of given dimensions with mempty
zeros :: (Monoid a, IMatrix mx a) => Int -> Int -> mx a
zeros r c = fromList $ replicate (r*c) mempty

fromList :: IMatrix mx a => [a] -> mx a
fromList [] = undefined 

{- | Given a row and a column with a list of
-- elements a matrix data structure is returned.
-- If the list is empty the zero matrix is returned
-- Otherwise matrix is built
matrix :: Int -> Int -> [a] -> Matrix a
matrix r c [] = Matrix (r*c) r c []
matrix r c m
    | r*c /= length m = error "Matrix indices do not match list length"
    | otherwise = 
        let r1 = take c m
         in Matrix (r*c) r c ([r1] ++ loop c (drop c m))
        where
            loop _ [] = []
            loop n xs = [take n xs] ++ loop n (drop c xs) 

-- | transposition
-- transposes the matrices elements
transpose :: IMatrix mx a => mx a
transpose m = 
    let (r, c) = rank m
     in 
        if r == 1 
            then 
            else (Matrix d c r (transposeIter elems []))
    where 
        transposeIter xs res =
            let hds = map head xs
                tls = map tail xs
             in 
                if concat tls == [] 
                    then res ++ [hds]
                    else transposeIter tls (res ++ [hds])

-- vector dot product
dot :: (Eq a, Num a) => Matrix a -> Matrix a -> a
dot (Matrix _ r1 c1 v1) (Matrix _ r2 c2 v2) =
    if 
        | c1 == 1 && c2 == 1 -> 
            if r1 /= r2
                then error "Vector dot product expects vectors of the same dimension" 
                else loop r1 v1 v2 0 0
        | r1 == 1 && r2 == 1 -> 
            if c1 /= c2
                then error "Vector dot product expects vectors of the same dimension" 
                else loop c1 v1 v2 0 0
        | otherwise -> error "Vector has higher dimension to perform dot product"
    where
        loop n x y i c =
            if i == n
                then c
                else
                    let xi = (x !! 0) !! i
                        yi = (y !! 0) !! i
                        c' = c + xi * yi
                     in loop n x y (i+1) c'

saxpy :: Num a => Matrix a  -> Matrix a -> a -> Matrix a
saxpy (Matrix d1 r1 c1 v1) (Matrix _ r2 c2 v2) alpha =
    if
        | r1 == 1 && r2 == 1 -> 
            if c1 /= c2
                then error "Vector dot product expects vectors of the same dimension" 
                else
                    let result = loop v1 v2 c1 alpha 0 []
                     in matrix r1 c1 result
        | c1 == 1 && c2 == 1-> 
            if r1 /= r2
                then error "Vector dot product expects vectors of the same dimension" 
                else
                    let result = loop v1 v1 r1 alpha 0 []
                     in matrix r1 c1 result
        | otherwise -> error "Vector scalar multiplication has higher dimension"
    where
        loop x y n a i res =
            if i == n 
                then res
                else
                    let xi = (x !! 0) !! i
                        yi = (y !! 0) !! i
                        yi' = yi + a * xi
                     in loop x y n a (i+1) (res ++ [yi'])

-- | Matrix addition
add :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
add (Matrix d m n m1) (Matrix _ v w m2) =
    if m /= v || n /= w 
        then error "Matrix addition requires both matrices to have same dimension"
        else
            let result = loop m1 m2 m 0 []
             in Matrix d m n result
    where
        loop a b n i res =
            if i == n
                then res
                else
                    let ai = a !! i
                        bi = b !! i
                        c = zipWith (+) ai bi
                     in loop a b n (i+1) (res ++ [c])
-- | Matrix addition
sub :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
sub (Matrix d m n m1) (Matrix _ v w m2) =
    if m /= v || n /= w 
        then error "Matrix addition requires both matrices to have same dimension"
        else
            let result = loop m1 m2 m 0 []
             in Matrix d m n result
    where
        loop a b n i res =
            if i == n
                then res
                else
                    let ai = a !! i
                        bi = b !! i
                        c = zipWith (-) ai bi
                     in loop a b n (i+1) (res ++ [c])

-- | Matrix multiplication
mul :: (Eq a, Num a) => Matrix a -> Matrix a -> Matrix a
mul (Matrix _ r1 c1 m1) (Matrix _ r2 c2 m2) =
    if c1 /= r2
        then error "Matrix multiplication requires both matrices to have same dimension"
        else do
                let result = loop m1 m2 r1 0 0 []
                matrix r1 c2 result
    where
        loop a b m i j res =
            if i == m
                then res
                else
                    if j == m
                        then loop a b m (i+1) 0 res
                        else
                            let ai = a !! i
                                bj = map (!!j) b
                                c = sum $ zipWith (*) ai bj
                             in loop a b m i (j+1) (res ++ [c])


-- | n \\ Matrix gives rows
(\\) :: (Eq a, Num a) => Int -> Matrix a -> Matrix a
n \\ (Matrix d r _ m) = 
    if n > r 
        then error "Row slice is greater than row size"
        else matrix r 1 (m !! n)

-- | Matrix // n gives columns
(//) :: (Eq a, Num a) => Matrix a -> Int -> Matrix a
(Matrix d _ c m) // n =
    if n > c
        then error "Column slice is greater than column size"
        else matrix 1 c (map (!!n) m)
-}
