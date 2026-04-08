# NDArray Examples

`NDArray[A]` is a strided N-dimensional array. It generalises `Matrix[A]` to arbitrary rank while sharing the same column-major memory model and zero-copy view semantics.

## Construction

```scala mdoc:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

// 1D from a flat Array
val v = NDArray.fromArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
v.ndim
v.numel
v.shape.mkString("[", ",", "]")
v.strides.mkString("[", ",", "]")

// 2D: shape [rows=2, cols=3], column-major strides [1, 2]
val m = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
m.ndim
m.shape.mkString("[", ",", "]")   // [2,3]
m.strides.mkString("[", ",", "]") // [1,2] — col-major: stride(0)=1, stride(1)=rows

// 3D: shape [2, 3, 4]
val t = NDArray(Array.tabulate(24)(_.toDouble), Array(2, 3, 4))
t.ndim
t.shape.mkString("[", ",", "]")   // [2,3,4]
t.strides.mkString("[", ",", "]") // [1,2,6] — col-major

// Zeros, ones, fill
val z = NDArray.zeros[Double](Array(2, 3))
val o = NDArray.ones[Double](Array(3))
val f = NDArray.fill(Array(2, 2), 7.0)

z.layout
```

## Element Access

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

// col-major 2×3: data stored column by column
// data = [col0row0, col0row1, col1row0, col1row1, col2row0, col2row1]
//      = [  1.0,     2.0,     3.0,     4.0,     5.0,     6.0   ]
val m = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))

m(0, 0) // row 0, col 0 → 1.0
m(1, 0) // row 1, col 0 → 2.0  (column-major: next row in same column)
m(0, 1) // row 0, col 1 → 3.0
m(1, 2) // row 1, col 2 → 6.0

// 3D element access: strides [1,2,6], so (1,2,3) → 1*1 + 2*2 + 3*6 = 23
val t = NDArray(Array.tabulate(24)(_.toDouble), Array(2, 3, 4))
t(1, 2, 3)

// Update
val a = NDArray.fromArray(Array(10.0, 20.0, 30.0))
a.update(1, 99.0)
a.toArray.mkString("[", ",", "]") // [10.0, 99.0, 30.0]
```

## Slicing and Views

Slices are **zero-copy** — they share the backing array.

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val m = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
m.layout

// slice(dim, start, end): keep rows 1 and 2 (indices 1 until 3)
val s = m.slice(0, 1, 3)
s.shape.mkString("[", ",", "]")  // [2,4]
s.data eq m.data                 // true — same backing array

// Multi-dimensional slicing with :: and Range
val subm = m(::, 1 until 3)
subm.shape.mkString("[", ",", "]") // [3,2]

// Gather non-contiguous rows via Array[Int]
val gathered = m(Array(0, 2), ::)
gathered.shape.mkString("[", ",", "]") // [2,4]
gathered(0, 0) == m(0, 0)
gathered(1, 0) == m(2, 0)
```

## Transpose

Transpose is **zero-copy** — it permutes strides without touching data.

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val m = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
m.shape.mkString("[", ",", "]")   // [2,3]
m.strides.mkString("[", ",", "]") // [1,2]

val t = m.T
t.shape.mkString("[", ",", "]")   // [3,2]
t.strides.mkString("[", ",", "]") // [2,1]
t.data eq m.data                  // true — same backing array

// Element equivalence: m(i,j) == m.T(j,i)
m(0, 1) == t(1, 0)
m(1, 2) == t(2, 1)

// N-D permutation: shape [2,3,4] → permute(2,0,1) → shape [4,2,3]
val cube = NDArray(Array.tabulate(24)(_.toDouble), Array(2, 3, 4))
val perm = cube.transpose(Array(2, 0, 1))
perm.shape.mkString("[", ",", "]") // [4,2,3]
```

## Reshape, Squeeze, Flatten

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

// reshape: zero-copy for contiguous arrays
val m = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
val r = m.reshape(Array(4, 3))
r.shape.mkString("[", ",", "]")   // [4,3]
r.data eq m.data                  // true — same backing array

// flatten: 1D view
val flat = m.flatten
flat.shape.mkString("[", ",", "]") // [12]

// unsqueeze: add a size-1 dimension
val v = NDArray.fromArray(Array(1.0, 2.0, 3.0))
val row = v.unsqueeze(0) // shape [1, 3] — row vector
val col = v.unsqueeze(1) // shape [3, 1] — column vector
row.shape.mkString("[", ",", "]") // [1,3]
col.shape.mkString("[", ",", "]") // [3,1]

// squeeze: remove all size-1 dimensions
val squeezed = row.squeeze
squeezed.shape.mkString("[", ",", "]") // [3]

// squeeze specific dimension
val arr3d = NDArray(Array.tabulate(6)(_.toDouble), Array(1, 2, 3))
arr3d.squeeze.shape.mkString("[", ",", "]") // [2,3]
```

## Element-wise Arithmetic

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
val b = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(2, 2))

// Binary ops — same shape required
(a + b).toArray.mkString("[", ",", "]")
(b - a).toArray.mkString("[", ",", "]")
(a * b).toArray.mkString("[", ",", "]")
(b / a).toArray.mkString("[", ",", "]")

// Scalar ops
(a + 100.0).toArray.mkString("[", ",", "]")
(a * 2.0).toArray.mkString("[", ",", "]")
(2.0 * a).toArray.mkString("[", ",", "]")
(10.0 / a).toArray.mkString("[", ",", "]")

// Unary ops
a.neg.toArray.mkString("[", ",", "]")
a.abs.toArray.mkString("[", ",", "]")
a.sqrt.toArray.mkString("[", ",", "]")
a.exp.toArray.mkString("[", ",", "]")
NDArray(Array(1.0, Math.E), Array(2)).log.toArray.mkString("[", ",", "]")
NDArray(Array(0.0, 1.0, -1.0), Array(3)).tanh.toArray.mkString("[", ",", "]")
NDArray(Array(0.0), Array(1)).sigmoid.toArray.mkString("[", ",", "]") // [0.5]

// In-place ops (array must be contiguous)
val c = NDArray(Array(1.0, 2.0, 3.0), Array(3))
c += NDArray(Array(9.0, 8.0, 7.0), Array(3))
c.toArray.mkString("[", ",", "]") // [10.0,10.0,10.0]

val d = NDArray(Array(10.0, 20.0, 30.0), Array(3))
d *= 2.0
d.toArray.mkString("[", ",", "]") // [20.0,40.0,60.0]
```

## Comparison Operations

Comparison ops return `NDArray[Boolean]` with the same shape.

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val a = NDArray(Array(1.0, 5.0, 3.0, 7.0, 2.0, 6.0), Array(2, 3))

// Scalar comparisons — returns NDArray[Boolean]
(a > 4.0).toArray.mkString("[", ",", "]")   // [false,true,false,true,false,true]
(a <= 3.0).toArray.mkString("[", ",", "]")  // [true,false,true,false,true,false]
(a =:= 5.0).toArray.mkString("[", ",", "]") // [false,true,false,false,false,false]

// Array comparisons
val b = NDArray(Array(2.0, 4.0, 3.0, 6.0, 2.0, 8.0), Array(2, 3))
(a > b).toArray.mkString("[", ",", "]")   // [false,true,false,true,false,false]
(a =:= b).toArray.mkString("[", ",", "]") // [false,false,true,false,true,false]
```

## Broadcasting

Broadcasting in vecxt is **explicit**: use `broadcastTo` or `broadcastPair` before binary ops.

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

// Broadcast a row vector shape [1,3] → [4,3] (zero-copy: stride-0 in dim 0)
val row = NDArray(Array(1.0, 2.0, 3.0), Array(1, 3))
val bcast = row.broadcastTo(Array(4, 3))
bcast.shape.mkString("[", ",", "]")   // [4,3]
bcast.strides.mkString("[", ",", "]") // [0,1] — stride-0 replicates dim 0

// Materialise the broadcast: add zeros to produce a concrete array
val result = bcast + NDArray.zeros[Double](Array(4, 3))
result.shape.mkString("[", ",", "]")
result.toArray.mkString("[", ",", "]")

// broadcastPair: broadcast two operands to their common shape
val p = NDArray(Array(1.0, 2.0, 3.0), Array(3))
val q = NDArray(Array(10.0), Array(1))
val (p2, q2) = broadcastPair(p, q)
(p2 + q2).toArray.mkString("[", ",", "]") // [11.0,12.0,13.0]

// broadcastShape: inspect the common shape without creating arrays
broadcastShape(Array(1, 3), Array(4, 1)).mkString("[", ",", "]") // [4,3]

// Typical use: add a bias row to every row of a 2D array
val data = NDArray.fill(Array(4, 3), 0.0)
val bias = NDArray(Array(10.0, 20.0, 30.0), Array(1, 3))
val (data2, bias2) = broadcastPair(data, bias)
val biased = data2 + bias2
biased.toArray.mkString("[", ",", "]")
```

## Views and Mutation Semantics

NDArray views (slice, T, reshape on contiguous, squeeze, unsqueeze) share the backing array. Mutation through any view is visible in the original.

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

val m = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))

// Slice is a view
val col1view = m.slice(1, 1, 2) // dim=1 (cols), keep col 1 only
col1view.data eq m.data         // true — shared backing array

// Mutate through the slice — visible in original
col1view.update(0, 0, 999.0)   // writes to m(0,1)
m(0, 1)                        // 999.0

// Transpose is also a view
val t = m.T
t.data eq m.data                // true

// Copy explicitly for independence
val indep = NDArray(m.toArray, m.shape.clone())
indep.data eq m.data            // false — fresh backing array
```

## Working with Higher Dimensions (3D+)

```scala mdoc:reset:to-string
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

// A "batch" of 4 matrices, each 3×5: shape [4, 3, 5]
// col-major strides: [1, 4, 12]
val batch = NDArray(Array.tabulate(60)(_.toDouble), Array(4, 3, 5))
batch.ndim
batch.numel
batch.strides.mkString("[", ",", "]") // [1,4,12]

// Extract the second matrix (index 1 along dim 0) as a view
val slice1 = batch.slice(0, 1, 2)
slice1.shape.mkString("[", ",", "]") // [1,3,5]

// Remove the leading size-1 dim → shape [3,5]
val mat1 = slice1.squeeze
mat1.shape.mkString("[", ",", "]")  // [3,5]

// Element access: batch=1, row=2, col=4
batch(1, 2, 4)
```
