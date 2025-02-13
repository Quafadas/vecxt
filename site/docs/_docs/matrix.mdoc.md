---
title: Matrix Examples
---
# Matrix Examples

Some examples. You shouldn't use `toString()` to find out about matricies. Mdoc calls it on each line anyway - not much i can do about that.

```scala mdoc:to-string

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val nestedArr = NArray(
  NArray[Double](1.0, 2.0, 3.5),
  NArray[Double](3.0, 4.0, 5.0),
  NArray[Double](6.0, 7.0, 8.0)
)

val matrix = Matrix.fromRows(nestedArr)
val matrix2 = Matrix.fromColumns(nestedArr)

matrix.shape

matrix.printMat

matrix2.printMat

matrix.col(1).printArr

matrix.row(2).printArr

// Note that indexing is done via a tuple.
matrix((1, 2))

```
More matrix operations...

```scala mdoc:to-string

import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val mat1 = Matrix(NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
val mat2 = Matrix(NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
val result = mat1.matmul(mat2)

result.printMat

// @ is a reserved character, so we can't just copy numpy syntax... experimental
val result2 = mat1 @@ mat2

result2.printMat

// opperator precedence...
val result3 = Matrix.eye[Double](2) + mat1 @@ mat2

result3.printMat

val mat3 = mat2.transpose + mat1
mat3.printMat

mat1.exp.printMat

mat1.log.printMat

```

## Slicing

Index via a `Int`, `NArray[Int]` or a `Range` to slice a matrix. The `::` operator is used to select all elements in a dimension.

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val mat = Matrix.fromRows(
  NArray(
    NArray[Double](1.0, 2.0, 3.0),
    NArray[Double](4.0, 5.0, 6.0),
    NArray[Double](7.0, 8.0, 9.0)
  )
)
mat(::, ::).printMat
mat(1, ::).printMat
mat(::, 1).printMat
mat(1, 1).printMat
mat(0 to 1, 0 to 1).printMat
mat(NArray.from[Int](Array(0, 2)), 0 to 1).printMat


```