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

val matInt = Matrix.fromRows(
  NArray[Int](1,2),
  NArray[Int](3,4)
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
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val mat1 = Matrix.fromRows(
  NArray(1.0, 4.0, 2.0),
  NArray(5.0, 3.0, 6.0)
)

println(mat1.printMat)

val mat2 = Matrix.fromRows(
  NArray(7.0, 9.0),
  NArray(8.0, 11.0),
  NArray(10, 12.0)
)

println(mat2.printMat)

val result = mat1.matmul(mat2)

result.printMat

// @ is a reserved character, so we can't just copy numpy syntax... experimental
val result2 = mat1 @@ mat2

result2.printMat

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

```

## Slicing

Index via a `Int`, `NArray[Int]` or a `Range` to slice a matrix. The `::` operator is used to select all elements in a dimension.

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val mat = Matrix.fromRows(
    NArray[Double](1.0, 2.0, 3.0),
    NArray[Double](4.0, 5.0, 6.0),
    NArray[Double](7.0, 8.0, 9.0)
)
mat(::, ::).printMat
mat(Array(1), ::).printMat
mat(::, Array(1)).printMat
mat(Array(1), Array(1)).printMat
mat(0 to 1, 0 to 1).printMat
mat(NArray.from[Int](Array(0, 2)), 0 to 1).printMat

```

## Hadamard Product (Element-wise Multiplication)

The Hadamard product (also known as element-wise multiplication) multiplies corresponding elements of two matrices. In vecxt, the `hadamard` method supports matrices with different memory layouts, including non-contiguous views and transposed matrices.

### Basic Usage

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val hadMat1 = Matrix.fromRows(
  NArray[Double](1.0, 2.0, 3.0),
  NArray[Double](4.0, 5.0, 6.0)
)

val hadMat2 = Matrix.fromRows(
  NArray[Double](2.0, 3.0, 4.0),
  NArray[Double](5.0, 6.0, 7.0)
)

// Element-wise multiplication
val hadResult = hadMat1.hadamard(hadMat2)
hadResult.printMat

```

### Working with Matrix Views

The Hadamard product works seamlessly with non-contiguous matrix views (slices), automatically handling the different memory layouts:

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

// Create base matrices
val hadBase1 = Matrix[Double](NArray.tabulate[Double](9)(i => (i + 1).toDouble), 3, 3)
val hadBase2 = Matrix[Double](NArray.tabulate[Double](9)(i => (i + 10).toDouble), 3, 3)

hadBase1.printMat
hadBase2.printMat

// Create views by selecting specific columns
val hadView1 = hadBase1(::, NArray(1, 2))  // columns 1 and 2
val hadView2 = hadBase2(::, NArray(1, 2))  // columns 1 and 2

hadView1.printMat
hadView2.printMat

// Hadamard product works on views
val hadViewResult = hadView1.hadamard(hadView2)
hadViewResult.printMat

```

### Mixed Layouts

You can use the Hadamard product with matrices that have different layouts (e.g., one simple, one sliced):

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val hadSimple = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 3, 2)
val hadBaseMixed = Matrix[Double](NArray.tabulate[Double](9)(i => (i + 10).toDouble), 3, 3)

hadSimple.printMat

hadBaseMixed.printMat

// Select specific columns from base
val hadViewMixed = hadBaseMixed(::, NArray(0, 2))

hadViewMixed.printMat

// Hadamard product with mixed layouts
val hadMixedResult = hadSimple.hadamard(hadViewMixed)
hadMixedResult.printMat

```

### Transposed Matrices

The Hadamard product also handles transposed matrices (which have row-major layout):

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val hadTransMat1 = Matrix[Double](NArray(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3)
val hadTransMat2 = Matrix[Double](NArray(10.0, 20.0, 30.0, 40.0, 50.0, 60.0), 3, 2)

hadTransMat1.printMat
hadTransMat2.printMat

// Transpose mat2 to match mat1's shape
val hadTransMat2T = hadTransMat2.transpose

hadTransMat2T.printMat

// Hadamard product with transposed matrix
val hadTransResult = hadTransMat1.hadamard(hadTransMat2T)
hadTransResult.printMat

```

### Performance Notes

The Hadamard product implementation is optimized for different scenarios:

- **Fast path**: When both matrices have the same dense memory layout (both column-major or both row-major), the operation uses SIMD-optimized array multiplication for maximum performance
- **Different layouts**: When matrices have different layouts, vecxt intelligently materializes only one matrix to match the other's layout, then performs in-place multiplication
- **Cross-platform**: The SIMD optimizations work on JVM (using Java's Vector API), while JS and Native use efficient while loops

This means you can freely work with sliced views and transposed matrices without worrying about performance penalties - vecxt handles the complexity for you while maintaining correctness as the #1 priority.

## Indexing

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*

val mat4 = Matrix.fromRows(
    NArray[Double](1.0, 2.0, 3.0),
    NArray[Double](4.0, 5.0, 6.0),
    NArray[Double](7.0, 8.0, 9.0)
)

mat4((1,1))

mat4(Array((1,1), (2,2))).printMat

```