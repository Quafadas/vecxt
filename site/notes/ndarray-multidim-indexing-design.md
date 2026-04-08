# NDArray Multi-dimensional Indexing with `::` Syntax

## Motivation

Matrix already supports ergonomic slicing with `::`:

```scala
m(0 until 5, ::)     // rows 0–4, all columns
m(::, 2)             // all rows, column 2 (not yet, but the idea)
m(::, 1 until 3)     // all rows, columns 1–2
```

This is powered by `RangeExtender = Range | Array[Int] | ::.type` in `rangeExtender.scala`. The `::` object (Scala's `scala.collection.immutable.::` companion) acts as a "select all" sentinel — zero-copy when combined with contiguous ranges.

NDArray needs the same ergonomics generalised to N dimensions:

```scala
val cube = NDArray(data, Array(4, 5, 3))
cube(::, 1 until 3, ::)       // shape [4, 2, 3] — slice axis 1
cube(0 until 2, ::, ::)       // shape [2, 5, 3] — slice axis 0
cube(::, ::, Array(0, 2))     // shape [4, 5, 2] — gather on axis 2
```

---

## Design

### Selector type

Reuse the existing `RangeExtender` union directly:

```scala
type RangeExtender = Range | Array[Int] | ::.type
```

No new types needed. The `Int` case (selecting a single index along a dimension, collapsing it) could be added later as `type NDSelector = RangeExtender | Int`, but for now we keep things simple — single-element indexing already exists via the `apply(i0, i1, ...)` overloads.

### API

A single varargs `apply` on `NDArray[A]`:

```scala
extension [A](arr: NDArray[A])

  /** Multi-dimensional slice/select. One selector per dimension.
    *
    * - `::` → keep entire dimension (zero-copy)
    * - `Range` (e.g. `1 until 3`) → contiguous slice (zero-copy if all selectors are :: or contiguous Range)
    * - `Array[Int]` → gather (requires copy)
    *
    * Number of selectors must equal `arr.ndim`.
    */
  def apply(selectors: RangeExtender*)(using ClassTag[A]): NDArray[A]
```

### Return semantics

| All selectors are `::` or contiguous `Range` | Result |
|----------------------------------------------|--------|
| Yes | **Zero-copy view** — adjust offset, shape, keep strides. Shares backing data. |
| No (any `Array[Int]` or non-contiguous `Range`) | **Copy** — gather into a fresh contiguous col-major NDArray. |

This mirrors the Matrix `submatrix` behaviour exactly: contiguous sub-blocks are views, arbitrary index selections require copying.

### Zero-copy fast path

When every selector is either `::` or a contiguous `Range` (`start until end` with step 1), the result can be expressed as a strided view of the same backing array:

```
newShape(k)  = selectorLength(k)        // :: → arr.shape(k), Range → range.length
newStrides   = arr.strides (unchanged)
newOffset    = arr.offset + Σ start(k) * arr.strides(k)
```

This is a direct generalisation of the existing `slice(dim, start, end)`.

### Copy path (gather)

When any selector is `Array[Int]` (or a non-unit-step `Range` which gets materialised to `Array[Int]`), we materialise:

```scala
// Iterate all combinations of selected indices
// For each output element, compute input physical position and copy
val outShape = Array(sel0.length, sel1.length, ..., selN.length)
val out = new Array[A](outShape.product)
// nested iteration over selector arrays, writing into col-major output
```

### Bounds checking

Validate via the existing `BoundsCheck` pattern:
- Number of selectors must equal `ndim`
- Each `Range` must be within `[0, shape(k))`
- Each `Array[Int]` element must be within `[0, shape(k))`

### Relationship to existing `apply` overloads

The existing `apply(i0: Int)`, `apply(i0: Int, i1: Int)`, etc. return a **single element** (`A`). The new varargs `apply(selectors: RangeExtender*)` returns an **NDArray**. These don't conflict because:
- The element-access overloads take `Int` parameters
- The slice overload takes `RangeExtender` parameters (which is `Range | Array[Int] | ::.type`)
- Scala resolves the overload unambiguously at the call site

A potential ambiguity: `arr(0 until 3)` on a 1-D array — is it element access or slicing? It's slicing, because `0 until 3` is a `Range`, not an `Int`. Element access uses `arr(2)` (bare `Int`). This is correct and intuitive.

### Single-`Int` selectors (dimension collapsing) — future extension

NumPy allows mixing integers and slices: `cube[0, :, 1:3]` returns a 2-D result (the int-indexed dimensions are collapsed). This is a natural extension:

```scala
type NDSelector = RangeExtender | Int
def apply(selectors: NDSelector*): NDArray[A] | A
```

But it introduces complexity (return type depends on how many `Int` selectors there are) and the existing per-arity `apply` overloads already handle the fully-indexed case. Defer this.

---

## Implementation Plan

### File: `vecxt/src/ndarrayOps.scala`

Add the varargs `apply` to the existing `extension [A](arr: NDArray[A])` block.

### Implementation sketch

```scala
def apply(selectors: RangeExtender*)(using ct: ClassTag[A]): NDArray[A] =
  // Validate selector count
  if selectors.length != arr.ndim then
    throw InvalidNDArray(
      s"Expected ${arr.ndim} selectors for ndim=${arr.ndim}, got ${selectors.length}"
    )

  // Resolve each selector to its index array and check if it's a contiguous range
  val resolved = new Array[Array[Int]](arr.ndim)
  var allContiguous = true
  var k = 0
  while k < arr.ndim do
    val sel = selectors(k)
    resolved(k) = range(sel, arr.shape(k))
    sel match
      case _: ::.type => // always contiguous
      case r: Range =>
        if r.step != 1 then allContiguous = false
      case _: Array[Int] =>
        // Check contiguity
        if !resolved(k).contiguous then allContiguous = false
    k += 1
  end while

  if allContiguous then
    // Zero-copy view
    val newShape = new Array[Int](arr.ndim)
    var newOffset = arr.offset
    k = 0
    while k < arr.ndim do
      newShape(k) = resolved(k).length
      val start = if resolved(k).isEmpty then 0 else resolved(k)(0)
      newOffset += start * arr.strides(k)
      k += 1
    end while
    mkNDArray(arr.data, newShape, arr.strides.clone(), newOffset)
  else
    // Copy path — gather into fresh col-major array
    val newShape = new Array[Int](arr.ndim)
    k = 0
    while k < arr.ndim do
      newShape(k) = resolved(k).length
      k += 1
    end while
    val outStrides = colMajorStrides(newShape)
    val n = shapeProduct(newShape)
    val out = new Array[A](n)

    var j = 0
    while j < n do
      // Decompose flat output index j into coordinates via outStrides
      // Then map each coordinate through resolved(k) to get input coordinate
      // Then compute physical input position via arr.strides
      var posIn = arr.offset
      k = 0
      while k < arr.ndim do
        val coord = (j / outStrides(k)) % newShape(k)
        posIn += resolved(k)(coord) * arr.strides(k)
        k += 1
      end while
      out(j) = arr.data(posIn)
      j += 1
    end while
    mkNDArray(out, newShape, outStrides, 0)
end apply
```

### Contiguity check for `Array[Int]`

Reuses the existing `Array[Int].contiguous` extension (from `intarray.scala`), which checks `arr(i) == arr(i-1) + 1` for all i.

---

## Verification Plan

### Tests: `vecxt/test/src/ndarraySlicing.test.scala`

#### 2-D slicing (match Matrix behaviour)

| Test | Input | Selectors | Expected shape | Notes |
|------|-------|-----------|---------------|-------|
| All `::` | `[2,3]` | `(::, ::)` | `[2,3]` | Identity, zero-copy |
| Row slice | `[4,3]` | `(0 until 2, ::)` | `[2,3]` | Zero-copy view |
| Col slice | `[4,3]` | `(::, 1 until 3)` | `[4,2]` | Zero-copy view |
| Both sliced | `[4,3]` | `(1 until 3, 0 until 2)` | `[2,2]` | Zero-copy view |
| Gather rows | `[4,3]` | `(Array(0,2), ::)` | `[2,3]` | Copy (non-contiguous) |
| Gather cols | `[4,3]` | `(::, Array(0,2))` | `[4,2]` | Copy (non-contiguous) |

#### 3-D slicing

| Test | Input | Selectors | Expected shape | Notes |
|------|-------|-----------|---------------|-------|
| Middle axis slice | `[2,5,3]` | `(::, 1 until 3, ::)` | `[2,2,3]` | Zero-copy |
| Last axis gather | `[2,5,3]` | `(::, ::, Array(0,2))` | `[2,5,2]` | Copy |
| All `::` 3-D | `[2,3,4]` | `(::, ::, ::)` | `[2,3,4]` | Identity |

#### 1-D slicing

| Test | Input | Selectors | Expected shape | Notes |
|------|-------|-----------|---------------|-------|
| Range slice | `[10]` | `(2 until 5,)` | `[3]` | Zero-copy |
| Gather | `[10]` | `(Array(0,3,7),)` | `[3]` | Copy |

#### Zero-copy verification

```scala
test("zero-copy: mutating view mutates original") {
  val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
  val arr = NDArray(data, Array(2, 3))
  val view = arr(::, 1 until 2)  // col 1 only
  // view and arr share backing data
  view.data(view.offset) = 99.0
  assertEquals(arr(0, 1), 99.0)
}
```

#### Error cases

| Test | Selectors | Expected |
|------|-----------|----------|
| Wrong number of selectors | 2 selectors on 3-D | `InvalidNDArray` |
| Range out of bounds | `(0 until 10,)` on shape `[5]` | `IndexOutOfBoundsException` |
| Array index out of bounds | `(Array(0, 99), ::)` on shape `[3,3]` | `IndexOutOfBoundsException` |

#### Consistency with existing `slice`

```scala
test("varargs (::, 1 until 3) matches chained slice") {
  val arr = NDArray(data, Array(4, 5))
  val via_varargs = arr(::, 1 until 3)
  val via_slice = arr.slice(1, 1, 3)
  // Same shape, strides, offset, data reference
  assertEquals(via_varargs.shape.toSeq, via_slice.shape.toSeq)
  assertEquals(via_varargs.offset, via_slice.offset)
  assert(via_varargs.data eq via_slice.data)
}
```

---

## What this does NOT change

- The existing `apply(i0: Int, ...)` element-access overloads are untouched
- The existing `slice(dim, start, end)` method is untouched (it remains useful for single-axis slicing)
- `RangeExtender` type in `rangeExtender.scala` is reused as-is
- No new dependencies or platform-specific code
- Matrix `apply(RangeExtender, RangeExtender)` is unaffected — it lives on `Matrix`, not `NDArray`

---

## Future: `update` with `::` syntax

Once slicing works, the natural follow-up is assignment:

```scala
cube(::, 1 until 3, ::) = otherNDArray   // write into a view
cube(::, 0, ::) = vector                  // broadcast-assign along axis
```

This can work for the zero-copy case (view-based assignment), but the gather case (non-contiguous indices) requires scatter semantics. Defer to a later milestone.
