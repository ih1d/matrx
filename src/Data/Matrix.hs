{-# LANGUAGE RankNTypes #-}

module Data.Matrix where

-- Matrix data structure
-- needs a better api
data Matrix a = Matrix 
    { row :: Int 
    , col :: Int 
    , elems :: Eq a => [[a]]
    }

-- | empty matrix construction
empty :: Matrix a
empty = Matrix 0 0 []

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

-- | dot product on matrices
dot :: Matrix a -> Matrix a -> a
dot (Matrix r1 c1 m1) (Matrix r2 c2 m2) = undefined

saxpy :: Matrix a -> Matrix a -> a -> Matrix a
saxpy m1 m2 a = undefined
{-    case (col m1) /= (col m2) of
        True -> error "saxpy needs to have same columns"
        False -> saxpyIter m1 m2 a 0 []
    where
        saxpyIter x y alpha i ls =
            if i == (col y) + 1
                then Matrix (row y) (col y) ls
                else 
                    let xi = ((elems x) !! i) * alpha
                        yi = (elems y) !! i
                        yi' = yi + xi
                     in saxpyIter x y alpha (i+1) (yi' : ls)
-}

addition :: Matrix a -> Matrix a -> Matrix a
addition _ _ = undefined

multiplication :: Matrix a -> Matrix a -> Matrix a
multiplication _ _ = undefined

scalarMult :: a -> Matrix a -> Matrix a
scalarMult _ _ = undefined

pointwiseMult :: Matrix a -> Matrix a -> Matrix a
pointwiseMult _ _ = undefined

pointwiseDiv :: Matrix a -> Matrix a -> Matrix a
pointwiseDiv _ _ = undefined
