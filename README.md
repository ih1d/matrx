# Purely Functional Matrices*
Native Haskell matrix library.

<sup><sub>* This will be moved to tensors eventually</sup></sub>

## No concern for alternative syntax
```hs
m1 :: Matrix a
m1 = matrix 2 2 [1,2,3,4]
[[1 2] [3 4]]
```
We use lists as main data structure for building so we do not rely on memorizing
some syntax that will not satisfy every user. 

## Basic operations
transposition, addition, scalar-matrix multiplication, and matrix-matrix
multiplication, row and column partitioning.

## Integration with *vector* library
It is important that the library works well with already well-defined
libraries such as *vector*. That is why no need of transformation between data
structures is required. Native transformations are provided:
```hs
fromVector :: Vector a -> Matrix a 
toVector :: Matrix a -> Vector a
```
