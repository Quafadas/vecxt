---
title: Matrix Examples
---
# Matrix Examples

Some basic examples. Mdoc calls `toString()` on each line, which actually, we don't want, but can't prevent for an opaque type. It is an annoyance which I believe to be justified - to be clear, you shouldn't use `toString()` to find out about matricies.

```scala mdoc:to-string

import vecxt.Matrix.*
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

matrix.print

matrix2.print

matrix.col(1).print

matrix.row(2).print

matrix.elementAt(1, 2)

```
There are only a small number of operations currently supported on matricies, but this sets out a paradigm. If it holds up, then adding more is a detail grind, rather than a risky time investment...

```scala mdoc:to-string

import vecxt.Matrix.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*
import vecxt.extensions.*

val mat1 = Matrix(NArray(1.0, 4.0, 2.0, 5.0, 3.0, 6.0), (2, 3))
val mat2 = Matrix(NArray(7.0, 9.0, 11.0, 8.0, 10, 12.0), (3, 2))
val result = mat1.matmul(mat2)

result.print

// @ is a reserved character, so we can't just copy numpy syntax... experimental
val result2 = mat1 @@ mat2

result2.print

val mat3 = mat2.transpose + mat1

```

## Slicing

Index via a `Int`, `NArray[Int]` or a `Range` to slice a matrix. The `::` operator is used to select all elements in a dimension.

```scala mdoc:to-string
import vecxt.Matrix.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import narr.*
import vecxt.extensions.*

val mat = Matrix.fromRows(
  NArray(
    NArray[Double](1.0, 2.0, 3.0),
    NArray[Double](4.0, 5.0, 6.0),
    NArray[Double](7.0, 8.0, 9.0)
  )
)
mat(::, ::).print
mat(1, ::).print
mat(::, 1).print
mat(1, 1).print
mat(0 to 1, 0 to 1).print
mat(NArray.from[Int](Array(0, 2)), 0 to 1).print


```