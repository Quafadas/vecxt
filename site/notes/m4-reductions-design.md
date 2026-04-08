# Milestone 4: NDArray Reduction Operations — Design, Implementation & Verification

## Summary

Milestone 4 adds **reduction operations** to `NDArray[Double]`: aggregations that collapse one or more dimensions of an N-dimensional array into a scalar or a lower-rank NDArray. This is the last piece needed before NDArray becomes useful for real numeric workloads (statistics, loss functions, normalisation).

---

## Scope

### In scope

| Category | Operations |
|----------|-----------|
| **Full reductions** | `sum`, `mean`, `min`, `max`, `product`, `variance`, `norm` (L2) |
| **Axis reductions** | `sum(axis)`, `mean(axis)`, `min(axis)`, `max(axis)`, `product(axis)` |
| **Arg reductions** | `argmax` (full), `argmin` (full) |
| **Axis arg reductions** | `argmax(axis)`, `argmin(axis)` |
| **Linear algebra** | `dot` (1-D only), `matmul` (2-D × 2-D) delegating to existing BLAS |
| **Norms** | `norm` (L2, full reduction) |

### Out of scope (future milestones)

- Reductions over multiple axes simultaneously (e.g. `sum(axes = Array(0, 2))`)
- `NDArray[Int]`, `NDArray[Float]`, `NDArray[Boolean]` reductions (M6)
- Higher-order reductions (`scan`, `cumsum` on NDArray)
- Axis-wise `variance`, `norm` (can be added later without API breakage)

---

## Design Decisions

### 1. Axis parameter: `Int`, not `DimensionExtender`

Matrix uses `DimensionExtender` (union `Int | Dimension`), with a `Dimension` enum providing `Rows`, `Cols`, `X`, `Y`. This is ergonomic for 2-D but doesn't generalise to N-D. NDArray axis reductions take a plain `Int` axis index.

```scala
// Matrix (existing)
m.sum(Dimension.Rows)   // reduce along rows → column vector
m.sum(0)                // same

// NDArray (new)
arr.sum(axis = 1)       // reduce axis 1
```

### 2. Axis reduction output shape: collapsed dimension removed

Following NumPy semantics, reducing axis `k` of shape `[d₀, …, dₖ, …, dₙ₋₁]` produces shape `[d₀, …, dₖ₋₁, dₖ₊₁, …, dₙ₋₁]` — the reduced dimension is **removed**, not kept as size 1. Users who want keepdims behaviour can `unsqueeze(axis)` on the result.

Rationale: simpler default, matches NumPy `keepdims=False` default, avoids ambiguity.

### 3. Full reductions return `Double`, not `NDArray[Double]`

`arr.sum` returns a `Double`, not a 0-D NDArray. Vecxt doesn't use 0-D NDArrays — they add indirection without benefit in a library that composes with raw `Double` and `Array[Double]`.

### 4. Fast path for col-major contiguous arrays

Every reduction has two code paths:

1. **Col-major contiguous** (`a.isColMajor`): operate directly on `a.data` with flat indexing — this is to the SIMD/BLAS fast path on JVM and simple while loops on JS/Native.
2. **General strided**: iterate in column-major coordinate order using the stride-based index computation already established in `NDArrayDoubleOps`.

This mirrors the pattern in every existing M3 element-wise operation.

### 5. Platform dispatch: cross-platform first, SIMD in JVM source set

All reduction code lives in **shared `src/`** using while loops. The col-major fast path for full reductions can delegate to the existing `Array[Double]` extension methods (e.g. `a.data.sumSIMD`), which are already SIMD-accelerated on the JVM. This avoids duplicating SIMD code — the delegation happens through the already-exported `vecxt.arrays.*` extensions.

For `dot` and `matmul`, the existing BLAS-backed implementations on `Array[Double]` and `Matrix[Double]` are reused.

### 6. `matmul` is 2-D only; `dot` is 1-D only

No batched matmul or broadcasting matmul. These are simple rank-checked operations that delegate to existing, well-tested code. Higher-rank contractions can come in a later milestone.

### 7. All operations are `inline def` extension methods

Following the project convention — `inline` avoids dispatch overhead, and extension methods in a dedicated object keep the API modular.

---

## API Surface

All methods are extensions on `NDArray[Double]`, defined in a new file `vecxt/src/ndarrayReductions.scala` and exported via `all.scala`.

```scala
package vecxt

object NDArrayReductions:

  extension (a: NDArray[Double])

    // ── Full reductions ──────────────────────────────────────────────────

    /** Sum of all elements. */
    inline def sum: Double

    /** Arithmetic mean of all elements. */
    inline def mean: Double

    /** Minimum element. */
    inline def min: Double

    /** Maximum element. */
    inline def max: Double

    /** Product of all elements. */
    inline def product: Double

    /** Population variance. */
    inline def variance: Double

    /** L2 (Euclidean) norm: √(Σ xᵢ²). */
    inline def norm: Double

    /** Index of the maximum element (flat, col-major order). */
    inline def argmax: Int

    /** Index of the minimum element (flat, col-major order). */
    inline def argmin: Int

    // ── Axis reductions ──────────────────────────────────────────────────

    /** Sum along axis `axis`. Result has one fewer dimension. */
    inline def sum(axis: Int): NDArray[Double]

    /** Mean along axis `axis`. */
    inline def mean(axis: Int): NDArray[Double]

    /** Min along axis `axis`. */
    inline def min(axis: Int): NDArray[Double]

    /** Max along axis `axis`. */
    inline def max(axis: Int): NDArray[Double]

    /** Product along axis `axis`. */
    inline def product(axis: Int): NDArray[Double]

    /** Argmax along axis `axis`. Returns NDArray[Double] of indices
      * (Double for type consistency; values are integral).
      */
    inline def argmax(axis: Int): NDArray[Double]

    /** Argmin along axis `axis`. */
    inline def argmin(axis: Int): NDArray[Double]

    // ── Linear algebra ───────────────────────────────────────────────────

    /** Dot product of two 1-D NDArrays. */
    inline def dot(b: NDArray[Double])(using inline bc: BoundsCheck): Double

    /** Matrix multiply two 2-D NDArrays. Result shape: [a.shape(0), b.shape(1)]. */
    inline def matmul(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double]

    /** Alias for matmul. */
    inline def @@(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double]

end NDArrayReductions
```

---

## Implementation Plan

### File layout

| File | Contents |
|------|----------|
| `vecxt/src/ndarrayReductions.scala` | All reduction extension methods (cross-platform) |
| `vecxt/test/src/ndarrayReductions.test.scala` | Cross-platform tests |
| `vecxt/src/all.scala` | Add `export vecxt.NDArrayReductions.*` |

No platform-specific files needed — all SIMD acceleration is inherited by delegating full reductions to existing `Array[Double]` extensions.

### Internal architecture

#### General strided iteration kernel

Reuse the same coordinate-iteration pattern from `NDArrayDoubleOps`:

```scala
private[NDArrayReductions] inline def reduceGeneral(
    a: NDArray[Double],
    initial: Double,
    inline f: (Double, Double) => Double
): Double =
  val n = a.numel
  val ndim = a.ndim
  val cumProd = colMajorStrides(a.shape)
  var acc = initial
  var j = 0
  while j < n do
    var pos = a.offset
    var k = 0
    while k < ndim do
      val coord = (j / cumProd(k)) % a.shape(k)
      pos += coord * a.strides(k)
      k += 1
    end while
    acc = f(acc, a.data(pos))
    j += 1
  end while
  acc
end reduceGeneral
```

#### Full reductions (col-major fast path)

```scala
inline def sum: Double =
  if a.isColMajor then a.data.sumSIMD   // delegates to SIMD on JVM
  else reduceGeneral(a, 0.0, _ + _)
```

The `sumSIMD`, `product`, `maxSIMD`, `minSIMD`, `norm`, `argmax`, `argmin` extensions on `Array[Double]` already exist on all platforms.

#### Full `variance`

Two-pass (mean first, then sum of squared deviations):

```scala
inline def variance: Double =
  val m = a.mean
  if a.isColMajor then
    var acc = 0.0
    var i = 0
    while i < a.data.length do
      val d = a.data(i) - m
      acc += d * d
      i += 1
    end while
    acc / a.data.length
  else
    var acc = 0.0
    // general strided iteration...
    acc / a.numel
```

Population variance (divide by N), matching the existing `Array[Double].variance` default.

#### Axis reductions

The core routine iterates all elements, computing each element's coordinate along the reduction axis and the position in the output array:

```scala
private[NDArrayReductions] def reduceAxis(
    a: NDArray[Double],
    axis: Int,
    initial: Double,
    f: (Double, Double) => Double
): NDArray[Double] =
  // 1. Compute output shape (remove axis)
  val outShape = removeAxis(a.shape, axis)
  val outStrides = colMajorStrides(outShape)
  val outN = shapeProduct(outShape)
  val out = Array.fill(outN)(initial)

  // 2. Iterate all input elements
  val n = a.numel
  val inCumProd = colMajorStrides(a.shape)
  var j = 0
  while j < n do
    // Compute N-D coordinate
    val coords = new Array[Int](a.ndim)
    var temp = j
    var k = a.ndim - 1
    while k >= 0 do
      coords(k) = (j / inCumProd(k)) % a.shape(k)
      k -= 1
    end while

    // Physical position in input data
    var posIn = a.offset
    k = 0
    while k < a.ndim do
      posIn += coords(k) * a.strides(k)
      k += 1
    end while

    // Output position (project out the axis dimension)
    var posOut = 0
    var outDim = 0
    k = 0
    while k < a.ndim do
      if k != axis then
        posOut += coords(k) * outStrides(outDim)
        outDim += 1
      end if
      k += 1
    end while

    out(posOut) = f(out(posOut), a.data(posIn))
    j += 1
  end while

  mkNDArray(out, outShape, outStrides, 0)
end reduceAxis
```

**Col-major fast path for axis reductions:** When `a.isColMajor`, the innermost axis (axis 0) reduction is a special case — it's a contiguous-chunk reduction (stride of adjacent elements along axis 0 is 1). The implementation can iterate in blocks of `a.shape(0)`, reducing each block to a single value. For the outermost axis, a similar stride-based optimisation applies. The general kernel above is correct for all cases; optimised paths for axis 0 and axis (ndim-1) can be added as follow-up performance work.

#### Argmax / argmin (axis)

Returns `NDArray[Double]` containing indices (as doubles). Same axis reduction pattern but tracking index along the reduction dimension:

```scala
// For each output position, track the index of the extremum along `axis`
var currentVal = initial
if compare(a.data(posIn), currentVal) then
  currentVal = a.data(posIn)
  outIdx(posOut) = coords(axis).toDouble  // the index along the reduced axis
```

Returning `NDArray[Double]` rather than `NDArray[Int]` for now — it avoids introducing `NDArray[Int]` operations ahead of M6 and the values are always integral indices.

#### `dot` (1-D)

```scala
inline def dot(b: NDArray[Double])(using inline bc: BoundsCheck): Double =
  inline if bc then
    if a.ndim != 1 then throw InvalidNDArray("dot requires 1-D arrays")
    if b.ndim != 1 then throw InvalidNDArray("dot requires 1-D arrays")
    if a.shape(0) != b.shape(0) then throw ShapeMismatchException("dot: length mismatch")
  end if
  if a.isColMajor && b.isColMajor then
    a.data.dot(b.data)  // delegates to BLAS on JVM
  else
    // general strided dot
    var acc = 0.0
    var i = 0
    while i < a.shape(0) do
      acc += a.data(a.offset + i * a.strides(0)) * b.data(b.offset + i * b.strides(0))
      i += 1
    end while
    acc
```

#### `matmul` (2-D × 2-D)

```scala
inline def matmul(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] =
  inline if bc then
    if a.ndim != 2 || b.ndim != 2 then throw InvalidNDArray("matmul requires 2-D arrays")
    if a.shape(1) != b.shape(0) then throw ShapeMismatchException("matmul: inner dimensions mismatch")
  end if
  // If both are col-major contiguous → delegate to existing Matrix matmul (BLAS dgemm)
  if a.isColMajor && b.isColMajor then
    val matA = Matrix(a.data, (a.shape(0), a.shape(1)))(using BoundsCheck.DoBoundsCheck.no)
    val matB = Matrix(b.data, (b.shape(0), b.shape(1)))(using BoundsCheck.DoBoundsCheck.no)
    val result = matA @@ matB
    mkNDArray(result.raw, Array(result.rows, result.cols), colMajorStrides(Array(result.rows, result.cols)), 0)
  else
    // Fallback: materialise to contiguous, then delegate
    val contA = if a.isColMajor then a else /* copy to col-major */ ...
    val contB = if b.isColMajor then b else /* copy to col-major */ ...
    contA.matmul(contB)
```

For non-contiguous inputs, the implementation materialises to contiguous arrays first. BLAS is so much faster than a manual triple loop that the copy overhead is always worthwhile for non-trivial matrices.

---

## Export

Add to `vecxt/src/all.scala`:

```scala
export vecxt.NDArrayReductions.*
```

---

## Verification Plan

### Test file

`vecxt/test/src/ndarrayReductions.test.scala` — cross-platform (shared `test/src/`).

### Test matrix

#### Full reductions

| Test | Input | Expected | Notes |
|------|-------|----------|-------|
| `sum` 1-D | `[1, 2, 3, 4]` | `10.0` | |
| `sum` 2-D col-major | `[[1,3],[2,4]]` (data `[1,2,3,4]`, shape `[2,2]`) | `10.0` | |
| `sum` 3-D | `shape [2,2,2]`, `data = [1..8]` | `36.0` | |
| `sum` strided (transposed) | `NDArray([1,2,3,4], [2,2]).T` | `10.0` | Exercises general path |
| `mean` 1-D | `[1, 2, 3, 4]` | `2.5` | |
| `min` 1-D | `[3, 1, 4, 1, 5]` | `1.0` | Duplicates |
| `max` 1-D | `[3, 1, 4, 1, 5]` | `5.0` | |
| `min` with negative | `[-1, -5, 2]` | `-5.0` | |
| `product` 1-D | `[2, 3, 4]` | `24.0` | |
| `product` with zero | `[2, 0, 4]` | `0.0` | |
| `variance` 1-D | `[2, 4, 4, 4, 5, 5, 7, 9]` | `4.0` | Population variance |
| `norm` 1-D | `[3, 4]` | `5.0` | Classic 3-4-5 triangle |
| `norm` 2-D | `[[1,0],[0,1]]` reshaped as NDArray | `√2` | Frobenius norm |
| `argmax` 1-D | `[1, 5, 3, 2]` | `1` | |
| `argmin` 1-D | `[4, 2, 7, 1]` | `3` | |
| `argmax` 2-D col-major | `[1,4,3,2]` shape `[2,2]` | `1` | Flat index in col-major order |

#### Axis reductions

| Test | Input shape | Axis | Expected shape | Expected data (col-major) | Notes |
|------|------------|------|---------------|--------------------------|-------|
| `sum(0)` on `[2,3]` | `[1,2,3,4,5,6]` | 0 | `[3]` | `[3, 7, 11]` | Sum rows: (1+2, 3+4, 5+6) |
| `sum(1)` on `[2,3]` | `[1,2,3,4,5,6]` | 1 | `[2]` | `[9, 12]` | Sum cols: (1+3+5, 2+4+6) |
| `max(0)` on `[2,3]` | `[1,2,3,4,5,6]` | 0 | `[3]` | `[2, 4, 6]` | Max across rows |
| `min(1)` on `[2,3]` | `[1,2,3,4,5,6]` | 1 | `[2]` | `[1, 2]` | Min across cols |
| `mean(0)` on `[2,3]` | `[1,2,3,4,5,6]` | 0 | `[3]` | `[1.5, 3.5, 5.5]` | |
| `product(0)` on `[2,3]` | `[1,2,3,4,5,6]` | 0 | `[3]` | `[2, 12, 30]` | |
| `sum(0)` on `[2,3,2]` 3-D | 12 elements | 0 | `[3,2]` | Hand-computed | Verify rank reduction |
| `sum(1)` on `[2,3,2]` 3-D | 12 elements | 1 | `[2,2]` | Hand-computed | Middle axis |
| `sum(2)` on `[2,3,2]` 3-D | 12 elements | 2 | `[2,3]` | Hand-computed | Last axis |
| `argmax(0)` on `[2,3]` | `[1,4,3,2,5,6]` | 0 | `[3]` | `[1,0,1]` | Indices along axis 0 |
| `argmin(1)` on `[2,3]` | `[5,6,1,2,3,4]` | 1 | `[2]` | `[1,1]` | Indices along axis 1 |
| Axis reduction on transposed | `arr.T.sum(0)` | 0 | check | check | General strided path |

#### Axis validation

| Test | Input | Expected |
|------|-------|----------|
| `sum(-1)` | any | `IndexOutOfBoundsException` or `InvalidNDArray` |
| `sum(ndim)` | any | `IndexOutOfBoundsException` or `InvalidNDArray` |
| `sum(0)` on 0-element shape `[0, 3]` | empty | Shape `[3]` with all zeros (identity for sum) |

#### dot

| Test | Input | Expected | Notes |
|------|-------|----------|-------|
| `dot` 1-D | `[1,2,3]` · `[4,5,6]` | `32.0` | |
| `dot` 1-D col-major (BLAS path) | `[1,0,0]` · `[0,0,1]` | `0.0` | Orthogonal |
| `dot` strided (sliced view) | slice of larger array | correct | General path |
| `dot` rank mismatch | 2-D · 1-D | `InvalidNDArray` | |
| `dot` length mismatch | `[3]` · `[4]` | `ShapeMismatchException` | |

#### matmul

| Test | Input | Expected | Notes |
|------|-------|----------|-------|
| Identity | `eye(3) @@ X` | `X` | |
| `[2,3] @@ [3,2]` | known values | hand-computed `[2,2]` | |
| Result shape | `[4,3] @@ [3,5]` | shape `[4,5]` | |
| Inner dim mismatch | `[2,3] @@ [2,3]` | `ShapeMismatchException` | |
| Rank mismatch | 1-D `@@ 2-D` | `InvalidNDArray` | |
| Non-contiguous input | transposed @@ something | correct result | Forces materialise-then-BLAS |
| Consistency with Matrix | same data via Matrix `@@` | same result | Regression |

#### Numerical edge cases

| Test | Notes |
|------|-------|
| Sum of empty 1-D (`shape [0]`) | `0.0` |
| Product of empty 1-D | `1.0` (identity) |
| Min/max of empty | Should throw (no meaningful answer) |
| Sum with NaN | Result is NaN |
| Min/max with NaN | NaN propagation (IEEE 754) |
| Very large arrays (1M elements) | Correctness + no timeout (sanity) |

#### Cross-platform consistency

Every test above runs on **JVM, JS, and Native** via the shared test source set. The tolerance for floating-point comparison is `1e-10` (matching the existing `assertNDArrayClose` helper).

### Test helpers needed

Extend the existing `helpers.forTests.scala`:

```scala
def assertNDArrayShapeAndClose(
    actual: NDArray[Double],
    expectedShape: Array[Int],
    expectedData: Array[Double]
)(implicit loc: munit.Location): Unit =
  assertEquals(actual.shape.toSeq, expectedShape.toSeq, "shape mismatch")
  assertNDArrayClose(actual, expectedData)
end assertNDArrayShapeAndClose
```

---

## Worked Example: 3-D axis reduction

To ensure the axis reduction logic is unambiguous, here is a fully worked example.

**Input:** `NDArray(data, Array(2, 3, 2))` with `data = Array(1,2, 3,4, 5,6, 7,8, 9,10, 11,12)`.

Column-major layout means shape `[2, 3, 2]`, strides `[1, 2, 6]`:

```
Coordinates → flat index → value:
(0,0,0) → 0  → 1      (1,0,0) → 1  → 2
(0,1,0) → 2  → 3      (1,1,0) → 3  → 4
(0,2,0) → 4  → 5      (1,2,0) → 5  → 6
(0,0,1) → 6  → 7      (1,0,1) → 7  → 8
(0,1,1) → 8  → 9      (1,1,1) → 9  → 10
(0,2,1) → 10 → 11     (1,2,1) → 11 → 12
```

**`sum(axis=0)`:** Collapse dim 0 (size 2). Output shape `[3, 2]`, strides `[1, 3]`.

For each `(j, k)` in output: `out(j, k) = Σ over i: input(i, j, k)`.

```
out(0,0) = input(0,0,0) + input(1,0,0) = 1 + 2 = 3
out(1,0) = input(0,1,0) + input(1,1,0) = 3 + 4 = 7
out(2,0) = input(0,2,0) + input(1,2,0) = 5 + 6 = 11
out(0,1) = input(0,0,1) + input(1,0,1) = 7 + 8 = 15
out(1,1) = input(0,1,1) + input(1,1,1) = 9 + 10 = 19
out(2,1) = input(0,2,1) + input(1,2,1) = 11 + 12 = 23
```

**Expected:** shape `[3, 2]`, data `[3, 7, 11, 15, 19, 23]` (col-major).

**`sum(axis=1)`:** Collapse dim 1 (size 3). Output shape `[2, 2]`, strides `[1, 2]`.

For each `(i, k)` in output: `out(i, k) = Σ over j: input(i, j, k)`.

```
out(0,0) = 1 + 3 + 5 = 9
out(1,0) = 2 + 4 + 6 = 12
out(0,1) = 7 + 9 + 11 = 27
out(1,1) = 8 + 10 + 12 = 30
```

**Expected:** shape `[2, 2]`, data `[9, 12, 27, 30]`.

**`sum(axis=2)`:** Collapse dim 2 (size 2). Output shape `[2, 3]`, strides `[1, 2]`.

For each `(i, j)` in output: `out(i, j) = Σ over k: input(i, j, k)`.

```
out(0,0) = 1 + 7 = 8
out(1,0) = 2 + 8 = 10
out(0,1) = 3 + 9 = 12
out(1,1) = 4 + 10 = 14
out(0,2) = 5 + 11 = 16
out(1,2) = 6 + 12 = 18
```

**Expected:** shape `[2, 3]`, data `[8, 10, 12, 14, 16, 18]`.

These exact values should appear as test cases.

---

## Implementation Order

1. **Private kernels** (`reduceGeneral`, `reduceAxis`, `argReduceAxis`, `removeAxis` helper)
2. **Full reductions** (`sum`, `mean`, `min`, `max`, `product`, `variance`, `norm`, `argmax`, `argmin`)
3. **Axis reductions** (`sum(axis)`, `mean(axis)`, `min(axis)`, `max(axis)`, `product(axis)`, `argmax(axis)`, `argmin(axis)`)
4. **Linear algebra** (`dot`, `matmul`, `@@`)
5. **Export** (update `all.scala`)
6. **Tests** (all the above, cross-platform)

Each step is independently compilable and testable.

---

## Performance Considerations

### What we get for free

- **Full reductions on col-major data** delegate to `Array[Double]` extensions which are already SIMD-accelerated on JVM (`sumSIMD`, `maxSIMD`, `minSIMD`, `norm` via BLAS `dnrm2`, `dot` via BLAS `ddot`, `argmax`/`argmin` with ILP-optimised block scanning).
- **`matmul`** delegates to `Matrix.@@` which uses BLAS `dgemm`.

### What needs attention later (not in M4)

- **Axis reductions** use the general coordinate-iteration kernel. For large col-major arrays, axis-0 reduction can be optimised to process contiguous chunks (stride-1 along axis 0 means `shape(0)` elements are contiguous). This is a performance follow-up, not a correctness concern.
- **JVM SIMD for axis reductions**: The general kernel doesn't vectorise. For M4, correctness is the priority. SIMD axis-reduction kernels can be added in `src-jvm/` later.

### Benchmarking

The existing `benchmark/` module can be extended with NDArray reduction benchmarks post-M4. The `benchmark_vs_breeze/` module can add NDArray variants alongside the existing Matrix comparisons.

---

## Risk Assessment

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| Axis indexing off-by-one in N-D | Medium | Worked examples with hand-computed values; test 1-D, 2-D, 3-D exhaustively |
| Col-major stride assumptions wrong for views | Low | Already validated by M2/M3 tests; axis reduction tests include transposed inputs |
| `matmul` via Matrix loses precision vs direct | None | Same BLAS routine, same data — bit-identical |
| `variance` numerical stability | Low | Two-pass algorithm; can upgrade to Welford in future if needed |
| API conflict with existing `Array[Double].sum` | None | Extension methods on `NDArray[Double]` are in a different type — no ambiguity |

---

## Relationship to Other Milestones

- **M3 (element-wise):** M4 reuses the same private kernels (`colMajorStrides`, `mkNDArray`, coordinate iteration pattern). Many reduction tests compose element-wise ops with reductions.
- **M5 (Matrix bridge):** Once `toMatrix`/`toNDArray` exist, `matmul` can be simplified to always go through Matrix. For M4, we construct Matrix inline from the NDArray's data.
- **M6 (extended types):** `NDArray[Int].sum`, `NDArray[Boolean].any`/`.all` follow the same patterns established here.
- **Future AD (M7/M8):** Every reduction has a clean VJP:
  - `sum` → broadcast gradient to input shape
  - `mean` → broadcast `grad / n`
  - `max`/`min` → gradient only to argmax/argmin position
  - `matmul` → `grad @@ b.T`, `a.T @@ grad`
  - `dot` → `grad * b`, `grad * a`
