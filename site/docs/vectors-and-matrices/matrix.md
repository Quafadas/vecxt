# Matrix Examples

Some examples. You shouldn't use `toString()` to find out about matricies. Mdoc calls it on each line anyway - not much i can do about that.

```scala mdoc:to-string

import vecxt.all.*


val nestedArr = Array(
  Array[Double](1.0, 2.0, 3.5),
  Array[Double](3.0, 4.0, 5.0),
  Array[Double](6.0, 7.0, 8.0)
)

val matInt = Matrix.fromRows(
  Array[Int](1,2),
  Array[Int](3,4)
)

val matrix = Matrix.fromRowsArray(nestedArr)
val matrix2 = Matrix.fromColumnsArray(nestedArr)

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


val mat1 = Matrix.fromRows(
  Array(1.0, 4.0, 2.0),
  Array(5.0, 3.0, 6.0)
)

println(mat1.printMat)

val mat2 = Matrix.fromRows(
  Array(7.0, 9.0),
  Array(8.0, 11.0),
  Array(10, 12.0)
)

println(mat2.printMat)

val result = mat1.matmul(mat2)

result.printMat

// @ is a reserved character, so we can't just copy numpy syntax... experimental
val result2 = mat1 @@ mat2

result2.printMat

// matmul / @@ always allocate a fresh, correctly shaped, dense column-major output matrix for you.

// opperator precedence...
val result3 = Matrix.eye[Double](2) + mat1 @@ mat2

result3.printMat

// TODO
// val mat3 = mat2.transpose + mat1
// mat3.printMat
// (mat2.transpose - mat1).printMat


// TODO: Check performance of vectorised version on JVM
mat1.exp.printMat

// TODO: Check performance of vectorised version on JVM
mat1.log.printMat

(mat1.sum(Dimension.Rows).printMat)
(mat1.max(Dimension.Cols).printMat)
(mat1.min(Dimension.Rows).printMat)
(mat1.product(Dimension.Cols).printMat)

(mat1.mapRowsToScalar(_.sum).printMat)
(mat1.mapRows(r => r / r.sum).printMat)

(mat1.mapColsToScalar(_.sum).printMat)
(mat1.mapCols(r => r / r.sum).printMat)

mat1.horzcat(mat1).printMat
mat2.vertcat(mat2).printMat

mat1.hadamard(mat1).printMat

```

### `matmulInPlace!`

`matmul`/`@@` allocate their own output matrix, so they always hand BLAS something safe to write into. If you call
the lower-level `matmulInPlace!` directly with your own output matrix `c` — for example to accumulate into an
existing matrix via `beta`, or to avoid an allocation on a hot path — `c` must already be shaped exactly
`(m.rows, b.cols)` and be dense column-major. `matmulInPlace!` throws `MatrixDimensionMismatch` if the shape is
wrong, and `UnsupportedLayoutException` if `c` has any other layout (including a fully dense row-major matrix of
the right shape) — otherwise it would silently write (or, whenever `beta != 0`, also read) through the wrong
strides.

## Slicing

Index via a `Int`, `Array[Int]` or a `Range` to slice a matrix. The `::` operator is used to select all elements in a dimension.

```scala mdoc:to-string
import vecxt.all.*


val mat = Matrix.fromRows(
  Array[Double](1.0, 2.0, 3.0),
  Array[Double](4.0, 5.0, 6.0),
  Array[Double](7.0, 8.0, 9.0)
)
mat(::, ::).printMat
mat(Array(1), ::).printMat
mat(::, Array(1)).printMat
mat(Array(1), Array(1)).printMat
mat(0 to 1, 0 to 1).printMat
mat(Array(0, 2), 0 to 1).printMat

```

## Indexing

```scala mdoc:to-string
import vecxt.all.*


val mat4 = Matrix.fromRows(
  Array[Double](1.0, 2.0, 3.0),
  Array[Double](4.0, 5.0, 6.0),
  Array[Double](7.0, 8.0, 9.0)
)

mat4((1,1))

mat4(Array((1,1), (2,2))).printMat

```