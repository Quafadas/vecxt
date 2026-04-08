# Milestone 6: Extended Element Types + Polish — Implementation Specification

## Summary

Add `NDArray[Int]`, `NDArray[Boolean]`, and `NDArray[Float]` element-wise operations, reductions,
boolean indexing, and `where`. All code lives in **shared `src/`** (cross-platform). No new
platform-specific files — the NDArray general kernels (col-major fast path + stride-based fallback)
already abstract away platform differences.

**Prior art to study before starting:**
- `vecxt/src/ndarrayDoubleOps.scala` — the template for element-wise + comparison ops
- `vecxt/src/ndarrayReductions.scala` — the template for full + axis reductions
- `vecxt/src/ndarray.scala` — `NDArray`, `mkNDArray`, `colMajorStrides`, `shapeProduct`
- `vecxt/src/broadcast.scala` — `sameShape`, `ShapeMismatchException`
- `vecxt/src/ZeroOne.scala` — `OneAndZero[A]` typeclass (already has `Boolean`, `Int`, `Float` instances via `Numeric`)
- `vecxt/test/src/helpers.forTests.scala` — existing assertion helpers
- `vecxt/src-jvm/BooleanArrays.scala` + `vecxt/src-js-native/BooleanArrays.scala` — existing flat `Array[Boolean]` ops (SIMD on JVM)

---

## Architecture Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| File per type | Yes: `ndarrayIntOps.scala`, `ndarrayBooleanOps.scala`, `ndarrayFloatOps.scala` | Mirrors `ndarrayDoubleOps.scala` pattern; keeps files focused |
| Kernel pattern | Same two-tier dispatch as Double: col-major fast path + general stride kernel | Consistency; proven pattern; general kernel handles all view/broadcast cases |
| Helper object pattern | One per file for reductions (e.g. `NDArrayIntReductionHelpers`) | Prevents name-shadowing when delegating to `Array[Int]` extension methods (see `NDArrayReductionHelpers` pattern) |
| Boolean reductions | Delegate to existing `BooleanArrays` for col-major fast path | Reuses SIMD acceleration on JVM already in `src-jvm/BooleanArrays.scala` |
| In-place ops for Int/Float | Yes, same pattern as Double | Consistency |
| `where` function | Free function in a new object `NDArrayWhere` | Not type-specific; operates on `NDArray[Boolean]` condition + two same-type operands |
| Boolean indexing | Extension on `NDArray[A]` taking `NDArray[Boolean]` | Returns 1-D col-major `NDArray[A]` of selected elements |
| Exports in `all.scala` | Add new objects | Match existing export pattern |

---

## Part 0: Fix existing `NDArray[Double]` axis `argmax`/`argmin` return type

**Before implementing any new types**, change the existing Double axis `argmax`/`argmin` from `NDArray[Double]` → `NDArray[Int]`. This is a breaking change, but the current API is wrong — indices should never be floating-point.

### Changes to `vecxt/src/ndarrayReductions.scala`

1. **`argReduceAxis` kernel** — change `outIdx` from `Array[Double]` to `Array[Int]`, and the return type from `NDArray[Double]` to `NDArray[Int]`:

```scala
private[NDArrayReductions] inline def argReduceAxis(
    a: NDArray[Double],
    axis: Int,
    inline initial: Double,
    inline compare: (Double, Double) => Boolean
): NDArray[Int] =                                        // ← was NDArray[Double]
  val outShape = removeAxis(a.shape, axis)
  val outStrides = colMajorStrides(outShape)
  val outN = shapeProduct(outShape)
  val bestVals = Array.fill(outN)(initial)
  val outIdx = new Array[Int](outN)                      // ← was Array[Double]
  // ... stride iteration unchanged ...
      if compare(v, bestVals(posOut)) then
        bestVals(posOut) = v
        outIdx(posOut) = axisCoord                       // ← was axisCoord.toDouble
      end if
  // ...
  mkNDArray(outIdx, outShape, colMajorStrides(outShape), 0)
end argReduceAxis
```

2. **Extension methods** — change return types:

```scala
inline def argmax(axis: Int): NDArray[Int] =   // ← was NDArray[Double]
inline def argmin(axis: Int): NDArray[Int] =   // ← was NDArray[Double]
```

### Changes to `vecxt/test/src/ndarrayReductions.test.scala`

Update the two axis arg-reduction tests. They currently use `assertNDArrayShapeAndClose` (which expects `NDArray[Double]`). Change to use `NDArray[Int]` assertions:

```scala
test("argmax(0) on [2,3]") {
  val arr = NDArray(Array(1.0, 5.0, 3.0, 2.0, 6.0, 4.0), Array(2, 3))
  val result = arr.argmax(0)
  assertEquals(result.shape.toSeq, Seq(3))
  assertEquals(result.toArray.toSeq, Seq(1, 0, 0))      // Int values, not Double
}

test("argmin(1) on [2,3]") {
  val arr = NDArray(Array(3.0, 1.0, 1.0, 2.0, 2.0, 5.0), Array(2, 3))
  val result = arr.argmin(1)
  assertEquals(result.shape.toSeq, Seq(2))
  assertEquals(result.toArray.toSeq, Seq(1, 1))          // Int values
}
```

### Compile + test to verify no regressions: `./mill vecxt.__.test`

---

## Part 1: `NDArray[Int]` — `ndarrayIntOps.scala`

### File: `vecxt/src/ndarrayIntOps.scala`

```
package vecxt
```

**Object:** `NDArrayIntOps`

### 1.1 Private kernel helpers (inside the object, before extensions)

Follow `NDArrayDoubleOps` exactly. Create these **private inline helpers**:

```scala
// Flat (col-major) helpers
private inline def flatBinaryOp(aData: Array[Int], bData: Array[Int], inline f: (Int, Int) => Int): Array[Int]
private inline def flatUnaryOp(data: Array[Int], inline f: Int => Int): Array[Int]
private inline def flatBinaryCompare(aData: Array[Int], bData: Array[Int], inline f: (Int, Int) => Boolean): Array[Boolean]
private inline def flatScalarCompare(data: Array[Int], s: Int, inline f: (Int, Int) => Boolean): Array[Boolean]

// General (strided) kernels
private[NDArrayIntOps] def binaryOpGeneral(a: NDArray[Int], b: NDArray[Int], f: (Int, Int) => Int): NDArray[Int]
private[NDArrayIntOps] def unaryOpGeneral(a: NDArray[Int], f: Int => Int): NDArray[Int]
private[NDArrayIntOps] def compareGeneral(a: NDArray[Int], b: NDArray[Int], f: (Int, Int) => Boolean): NDArray[Boolean]
private[NDArrayIntOps] def compareScalarGeneral(a: NDArray[Int], s: Int, f: (Int, Int) => Boolean): NDArray[Boolean]
private[NDArrayIntOps] def binaryOpInPlaceGeneral(a: NDArray[Int], b: NDArray[Int], f: (Int, Int) => Int): Unit
```

These are **identical in structure** to the Double versions — just substitute `Double` → `Int` and `Array[Double]` → `Array[Int]`. Copy the stride iteration pattern from `NDArrayDoubleOps.binaryOpGeneral` verbatim (it uses `colMajorStrides`, `mkNDArray`, etc. from `ndarray.*`).

### 1.2 Extension methods on `NDArray[Int]`

**Binary ops (same shape required):**
- `+(b: NDArray[Int]): NDArray[Int]`
- `-(b: NDArray[Int]): NDArray[Int]`
- `*(b: NDArray[Int]): NDArray[Int]`
- `/(b: NDArray[Int]): NDArray[Int]` — integer division
- `%(b: NDArray[Int]): NDArray[Int]` — modulo (Int-specific, not on Double)

Each follows the exact pattern of `NDArrayDoubleOps.+`:
1. Check `sameShape(a.shape, b.shape)` — throw `ShapeMismatchException` if not
2. If `a.isColMajor && b.isColMajor` → `flatBinaryOp` fast path
3. Else → `binaryOpGeneral`

**Scalar ops (right):**
- `+(s: Int): NDArray[Int]`
- `-(s: Int): NDArray[Int]`
- `*(s: Int): NDArray[Int]`
- `/(s: Int): NDArray[Int]`
- `%(s: Int): NDArray[Int]`

Pattern: if `a.isColMajor` → flat loop with scalar, else `unaryOpGeneral(a, x => x op s)`.

**Left-scalar ops** (separate `extension (s: Int)` block):
- `+(a: NDArray[Int]): NDArray[Int]`
- `-(a: NDArray[Int]): NDArray[Int]`
- `*(a: NDArray[Int]): NDArray[Int]`
- `/(a: NDArray[Int]): NDArray[Int]`

Use `@targetName` annotations to disambiguate from right-scalar ops, exactly as done for `Double` left-scalar ops in `ndarrayDoubleOps.scala`. Check that file for the naming pattern.

**Unary ops:**
- `neg: NDArray[Int]` — negate each element
- `abs: NDArray[Int]` — absolute value

**In-place ops:**
- `+=(b: NDArray[Int]): Unit`
- `-=(b: NDArray[Int]): Unit`
- `*=(b: NDArray[Int]): Unit`
- `+=(s: Int): Unit`, `-=(s: Int)`, `*=(s: Int)`

Pattern: require same shape, require `a.isContiguous` (throw `InvalidNDArray` if not). If both col-major → flat in-place loop, else `binaryOpInPlaceGeneral`.

**Comparison ops → `NDArray[Boolean]`:**
- `>(b: NDArray[Int]): NDArray[Boolean]`
- `<(b: NDArray[Int]): NDArray[Boolean]`
- `>=(b: NDArray[Int]): NDArray[Boolean]`
- `<=(b: NDArray[Int]): NDArray[Boolean]`
- `=:=(b: NDArray[Int]): NDArray[Boolean]`
- `!:=(b: NDArray[Int]): NDArray[Boolean]`

**Scalar comparison ops:**
- `>(s: Int)`, `<(s: Int)`, `>=(s: Int)`, `<=(s: Int)`, `=:=(s: Int)`, `!:=(s: Int)` → `NDArray[Boolean]`

### 1.3 Reductions: `ndarrayIntReductions.scala`

### File: `vecxt/src/ndarrayIntReductions.scala`

**Object:** `NDArrayIntReductions`

**Helper object** (top-level private, **before** the main object):

```scala
private object NDArrayIntReductionHelpers:
  // Only needed if delegating to existing Array[Int] extension methods.
  // Currently Array[Int] has minimal ops (select, contiguous) — no sum/min/max.
  // So this may be empty or contain inline helpers.
end NDArrayIntReductionHelpers
```

Since `Array[Int]` has no existing `sum`/`min`/`max` SIMD-accelerated extensions (unlike `Array[Double]`), the col-major fast path for Int reductions uses **simple flat while loops** directly — no delegation needed. This is still fast because flat array access is cache-friendly.

**Private kernel helpers** — copy from `NDArrayReductions`:
```scala
private[NDArrayIntReductions] inline def removeAxis(shape: Array[Int], axis: Int): Array[Int]
private[NDArrayIntReductions] inline def reduceGeneral(a: NDArray[Int], inline initial: Int, inline f: (Int, Int) => Int): Int
private[NDArrayIntReductions] inline def reduceAxis(a: NDArray[Int], axis: Int, inline initial: Int, inline f: (Int, Int) => Int): NDArray[Int]
private[NDArrayIntReductions] inline def argReduceAxis(a: NDArray[Int], axis: Int, inline initial: Int, inline compare: (Int, Int) => Boolean): NDArray[Int]
```

Note: `removeAxis` is identical to Double version; should be shared. Options:
- **Option A (preferred):** Duplicate it — it's 10 lines, inlined away at compile time, avoids cross-object coupling.
- **Option B:** Move to `ndarray` package object. Only if you're confident this won't cause import/shadowing issues.

Go with **Option A** (duplicate in each reductions object).

**Full reductions (`extension (a: NDArray[Int])`):**
- `sum: Int` — if contiguous, flat loop on `a.data`; else `reduceGeneral(a, 0, _ + _)`
- `mean: Double` — `a.sum.toDouble / a.numel`
- `min: Int` — if contiguous, flat loop; else `reduceGeneral(a, Int.MaxValue, (acc, x) => if x < acc then x else acc)`
- `max: Int` — if contiguous, flat loop; else `reduceGeneral(a, Int.MinValue, (acc, x) => if x > acc then x else acc)`
- `product: Int` — if contiguous, flat loop; else `reduceGeneral(a, 1, _ * _)`
- `argmax: Int` — flat index of max in col-major order
- `argmin: Int` — flat index of min in col-major order

**Axis reductions:**
- `sum(axis: Int): NDArray[Int]`
- `mean(axis: Int): NDArray[Double]` — reduce via `Int` sum, then divide each output element by `a.shape(axis).toDouble`. Result is `NDArray[Double]` not `NDArray[Int]`.
- `min(axis: Int): NDArray[Int]`
- `max(axis: Int): NDArray[Int]`
- `product(axis: Int): NDArray[Int]`
- `argmax(axis: Int): NDArray[Int]`
- `argmin(axis: Int): NDArray[Int]`

**Design note on `mean` return type:** `mean` returns `Double`/`NDArray[Double]` because integer mean is inherently fractional. This matches NumPy behavior.

**Design note on `argmax`/`argmin` return type:** Axis `argmax`/`argmin` returns `NDArray[Int]` for **all** element types (Int, Float, Double). Indices are inherently integers — storing them as `Double` or `Float` is lossy and confusing. The existing M4 `NDArray[Double]` axis `argmax`/`argmin` (which currently returns `NDArray[Double]`) must be changed to `NDArray[Int]` as part of this milestone. See "Part 0" below.

---

## Part 2: `NDArray[Boolean]` — `ndarrayBooleanOps.scala`

### File: `vecxt/src/ndarrayBooleanOps.scala`

**Object:** `NDArrayBooleanOps`

### 2.1 Private kernel helpers

```scala
// Flat helpers
private inline def flatBinaryLogical(aData: Array[Boolean], bData: Array[Boolean], inline f: (Boolean, Boolean) => Boolean): Array[Boolean]

// General (strided) kernels
private[NDArrayBooleanOps] def binaryLogicalGeneral(a: NDArray[Boolean], b: NDArray[Boolean], f: (Boolean, Boolean) => Boolean): NDArray[Boolean]
private[NDArrayBooleanOps] def unaryLogicalGeneral(a: NDArray[Boolean]): NDArray[Boolean]  // NOT
```

### 2.2 Extension methods on `NDArray[Boolean]`

**Logical binary ops (same shape required):**
- `&&(b: NDArray[Boolean]): NDArray[Boolean]` — element-wise AND
- `||(b: NDArray[Boolean]): NDArray[Boolean]` — element-wise OR

Pattern: same-shape check → col-major fast path → general kernel.

**Col-major fast path:** Delegate to existing `BooleanArrays.&&` and `BooleanArrays.||` on flat arrays. These are already SIMD-accelerated on JVM. Import `vecxt.BooleanArrays.*` inside the col-major branch.

**Important:** `BooleanArrays.&&`/`||` only exist on JVM (`src-jvm/BooleanArrays.scala`), **not** on JS/Native. On JS/Native, `src-js-native/BooleanArrays.scala` does NOT define `&&`/`||`.

**Resolution:** The col-major fast path for `&&`/`||` should use a flat while loop directly (not delegate), which works cross-platform. The SIMD version could be added later as a JVM-specific enhancement if benchmarks warrant it. This matches the pattern of `ndarrayDoubleOps.scala` which uses platform-neutral `flatBinaryOp` loops.

```scala
// Col-major fast path for &&
private inline def flatBinaryLogical(
    aData: Array[Boolean],
    bData: Array[Boolean],
    inline f: (Boolean, Boolean) => Boolean
): Array[Boolean] =
  val n = aData.length
  val out = new Array[Boolean](n)
  var i = 0
  while i < n do
    out(i) = f(aData(i), bData(i))
    i += 1
  end while
  out
end flatBinaryLogical
```

**Unary ops:**
- `unary_! : NDArray[Boolean]` — element-wise NOT. **Caution:** Scala's `unary_!` prefix syntax is finicky with extension methods. Use `not` instead (matches existing `BooleanArrays.not` naming).
- `not: NDArray[Boolean]` — if col-major, flat loop `!data(i)`; else `unaryLogicalGeneral`

**In-place:**
- `not!: Unit` — in-place negation (require contiguous)

### 2.3 Reductions on `NDArray[Boolean]`

Add these **in the same file** (or a separate `ndarrayBooleanReductions.scala` — your choice; same file is fine since there are few):

**Full reductions:**
- `any: Boolean` — true if any element is true. Col-major fast path: delegate to `BooleanArrays.any` on `a.data`. General: stride iteration with early exit.
- `all: Boolean` — true if all elements are true. Col-major fast path: delegate to `BooleanArrays.allTrue` on `a.data`. General: stride iteration with early exit.
- `countTrue: Int` — count of true elements. Col-major fast path: delegate to `BooleanArrays.trues` on `a.data`. General: stride accumulate.

**These delegations DO work cross-platform** — `any`, `allTrue`, `trues` exist in both `src-jvm/BooleanArrays.scala` and `src-js-native/BooleanArrays.scala`.

**Axis reductions:**
- `any(axis: Int): NDArray[Boolean]` — true if any element along axis is true. Use a reduce-axis kernel with `initial = false`, `f = _ || _`.
- `all(axis: Int): NDArray[Boolean]` — Use `initial = true`, `f = _ && _`.
- `countTrue(axis: Int): NDArray[Int]` — Count trues along axis. This returns `NDArray[Int]`. Kernel: accumulate `if elem then 1 else 0` into `Array[Int]` output.

For axis reductions, adapt the `reduceAxis` kernel pattern from `NDArrayReductions` but typed for `Boolean`/`Int` as appropriate:

```scala
// Boolean axis reduce — same structure as NDArrayReductions.reduceAxis
private[NDArrayBooleanOps] inline def reduceAxisBool(
    a: NDArray[Boolean],
    axis: Int,
    inline initial: Boolean,
    inline f: (Boolean, Boolean) => Boolean
): NDArray[Boolean] =
  // Copy reduceAxis pattern, substitute Boolean for Double
```

```scala
// countTrue axis reduce — accumulates Int
private[NDArrayBooleanOps] inline def countTrueAxis(
    a: NDArray[Boolean],
    axis: Int
): NDArray[Int] =
  val outShape = removeAxis(a.shape, axis)
  val outStrides = colMajorStrides(outShape)
  val outN = shapeProduct(outShape)
  val out = new Array[Int](outN) // initialized to 0
  // iterate all elements, accumulate count
  // ... same stride iteration pattern ...
  mkNDArray(out, outShape, colMajorStrides(outShape), 0)
```

---

## Part 3: `NDArray[Float]` — `ndarrayFloatOps.scala`

### File: `vecxt/src/ndarrayFloatOps.scala`

**Object:** `NDArrayFloatOps`

This is structurally **identical** to `NDArrayIntOps` but with `Float` instead of `Int`. No modulo operation (not useful for floats).

### 3.1 Extension methods on `NDArray[Float]`

**Binary ops:** `+`, `-`, `*`, `/`
**Scalar ops:** `+ s`, `- s`, `* s`, `/ s`
**Left-scalar ops:** `s + a`, `s - a`, `s * a`, `s / a`
**Unary ops:** `neg`, `abs`, `exp`, `log`, `sqrt`, `tanh`, `sigmoid`
**In-place:** `+=`, `-=`, `*=`, `/=` (array and scalar variants)
**Comparisons → NDArray[Boolean]:** `>`, `<`, `>=`, `<=`, `=:=`, `!:=` (array and scalar)

### 3.2 Reductions: `ndarrayFloatReductions.scala`

### File: `vecxt/src/ndarrayFloatReductions.scala`

**Object:** `NDArrayFloatReductions`

**Full reductions:**
- `sum: Float`
- `mean: Float` — `sum / numel`
- `min: Float`
- `max: Float`
- `product: Float`
- `variance: Float`
- `norm: Float` — `sqrt(sum of squares)`
- `argmax: Int`
- `argmin: Int`

**Axis reductions:**
- `sum(axis): NDArray[Float]`
- `mean(axis): NDArray[Float]`
- `min(axis): NDArray[Float]`
- `max(axis): NDArray[Float]`
- `product(axis): NDArray[Float]`
- `argmax(axis): NDArray[Int]` — indices are always Int
- `argmin(axis): NDArray[Int]`

Float reductions use `Float.PositiveInfinity`, `Float.NegativeInfinity`, `Float.MaxValue`, etc. as sentinels.

---

## Part 4: Boolean Indexing

### File: `vecxt/src/ndarrayBooleanIndexing.scala`

**Object:** `NDArrayBooleanIndexing`

### 4.1 Boolean mask selection

```scala
extension [A: ClassTag](arr: NDArray[A])
  /** Select elements where mask is true. Returns a 1-D col-major NDArray.
    * Mask must have the same shape as arr.
    * Result length = mask.countTrue.
    */
  inline def apply(mask: NDArray[Boolean]): NDArray[A] =
```

**Implementation:**
1. Validate `sameShape(arr.shape, mask.shape)` — throw `ShapeMismatchException` if not
2. First pass: count trues (use `mask.countTrue` if mask is contiguous, else stride-iterate)
3. Allocate `Array[A](count)`
4. Second pass: iterate both `arr` and `mask` in col-major coordinate order, copy matching elements
5. Return `NDArray.fromArray(result)` (1-D)

**Performance note:** Two passes (count + copy) avoids `ArrayBuffer` allocation overhead. This is the standard pattern.

**Col-major fast path:** If both `arr.isColMajor && mask.isColMajor`, iterate flat arrays directly:
```scala
var i = 0
var j = 0
while i < arr.data.length do
  if mask.data(i) then
    out(j) = arr.data(i)
    j += 1
  end if
  i += 1
end while
```

**General path:** Use the standard stride iteration pattern (same `colMajorStrides` + coordinate decomposition loop) over both arrays simultaneously.

### 4.2 Boolean mask assignment (optional, lower priority)

```scala
extension [A](arr: NDArray[A])
  /** Set elements where mask is true to `value`. Mutates arr.data in-place. */
  inline def update(mask: NDArray[Boolean], value: A): Unit =
```

Only implement this if time permits. Boolean selection (read) is the priority.

---

## Part 5: `where` — Conditional Element Selection

### File: `vecxt/src/ndarrayWhere.scala`

**Object:** `NDArrayWhere`

```scala
/** Element-wise conditional: where(condition, x, y) → result where
  * result(i) = x(i) if condition(i) else y(i).
  * All three must have the same shape.
  */
inline def where[A: ClassTag](
    condition: NDArray[Boolean],
    x: NDArray[A],
    y: NDArray[A]
): NDArray[A] =
```

**Implementation:**
1. Validate all three shapes match — `sameShape(condition.shape, x.shape) && sameShape(condition.shape, y.shape)`
2. Col-major fast path if all three are col-major: flat loop
3. General path: stride iteration with three position trackers (condition, x, y)
4. Output is always fresh col-major `NDArray[A]`

**Scalar overloads:**
```scala
inline def where[A: ClassTag](condition: NDArray[Boolean], x: A, y: NDArray[A]): NDArray[A]
inline def where[A: ClassTag](condition: NDArray[Boolean], x: NDArray[A], y: A): NDArray[A]
inline def where[A: ClassTag](condition: NDArray[Boolean], x: A, y: A): NDArray[A]
```

The scalar variants avoid allocating broadcast NDArrays for common cases like `where(mask, 0.0, arr)`.

---

## Part 6: Exports in `all.scala`

Add to `vecxt/src/all.scala`:

```scala
  export vecxt.NDArrayIntOps.*
  export vecxt.NDArrayIntReductions.*
  export vecxt.NDArrayBooleanOps.*
  export vecxt.NDArrayFloatOps.*
  export vecxt.NDArrayFloatReductions.*
  export vecxt.NDArrayBooleanIndexing.*
  export vecxt.NDArrayWhere.*
```

---

## Part 7: Required Imports / Dependencies

All new files need:
```scala
package vecxt

import vecxt.broadcast.*       // sameShape, ShapeMismatchException
import vecxt.ndarray.*          // NDArray, mkNDArray, colMajorStrides, shapeProduct
```

Reductions files additionally need:
```scala
import vecxt.BoundsCheck.BoundsCheck
```

Boolean ops need:
```scala
import vecxt.BooleanArrays.*   // for flat-array delegation (any, allTrue, trues)
```

Boolean indexing needs:
```scala
import scala.reflect.ClassTag
```

---

## Validation Tests

All tests go in `vecxt/test/src/`. Use munit `FunSuite`. Import `all.*` and `BoundsCheck.DoBoundsCheck.yes`.

### Test file: `vecxt/test/src/ndarrayIntOps.test.scala`

```scala
class NDArrayIntOpsSuite extends FunSuite:

  // ── Construction ──────────────────────────────────────────────────────────

  test("NDArray[Int] zeros and ones") {
    val z = NDArray.zeros[Int](Array(2, 3))
    assertEquals(z.toArray.toSeq, Seq(0, 0, 0, 0, 0, 0))
    val o = NDArray.ones[Int](Array(2, 3))
    assertEquals(o.toArray.toSeq, Seq(1, 1, 1, 1, 1, 1))
  }

  // ── Binary ops (col-major fast path) ──────────────────────────────────────

  test("NDArray[Int] + NDArray[Int] (col-major 1D)") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    val b = NDArray(Array(4, 5, 6), Array(3))
    val result = a + b
    assertEquals(result.toArray.toSeq, Seq(5, 7, 9))
    assert(result.isColMajor)
  }

  test("NDArray[Int] - NDArray[Int] (col-major 2D)") {
    val a = NDArray(Array(10, 20, 30, 40), Array(2, 2))
    val b = NDArray(Array(1, 2, 3, 4), Array(2, 2))
    assertEquals((a - b).toArray.toSeq, Seq(9, 18, 27, 36))
  }

  test("NDArray[Int] * NDArray[Int]") {
    val a = NDArray(Array(2, 3, 4), Array(3))
    val b = NDArray(Array(5, 6, 7), Array(3))
    assertEquals((a * b).toArray.toSeq, Seq(10, 18, 28))
  }

  test("NDArray[Int] / NDArray[Int] (integer division)") {
    val a = NDArray(Array(7, 10, 15), Array(3))
    val b = NDArray(Array(2, 3, 4), Array(3))
    assertEquals((a / b).toArray.toSeq, Seq(3, 3, 3))
  }

  test("NDArray[Int] % NDArray[Int]") {
    val a = NDArray(Array(7, 10, 15), Array(3))
    val b = NDArray(Array(2, 3, 4), Array(3))
    assertEquals((a % b).toArray.toSeq, Seq(1, 1, 3))
  }

  // ── Binary ops (general kernel — transposed views) ────────────────────────

  test("NDArray[Int] + NDArray[Int] via general kernel (transposed view)") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val at = a.T  // shape [3,2], strides [2,1] — non-col-major
    val bt = a.T
    val result = at + bt
    val expected = at.toArray.map(_ * 2)
    assertEquals(result.toArray.toSeq, expected.toSeq)
    assert(result.isColMajor, "result of general kernel should be col-major")
  }

  // ── Scalar ops ────────────────────────────────────────────────────────────

  test("NDArray[Int] + scalar") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((a + 10).toArray.toSeq, Seq(11, 12, 13))
  }

  test("NDArray[Int] * scalar") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((a * 3).toArray.toSeq, Seq(3, 6, 9))
  }

  test("NDArray[Int] / scalar") {
    val a = NDArray(Array(6, 9, 12), Array(3))
    assertEquals((a / 3).toArray.toSeq, Seq(2, 3, 4))
  }

  test("NDArray[Int] % scalar") {
    val a = NDArray(Array(7, 10, 15), Array(3))
    assertEquals((a % 4).toArray.toSeq, Seq(3, 2, 3))
  }

  // ── Left-scalar ops ───────────────────────────────────────────────────────

  test("scalar + NDArray[Int]") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((10 + a).toArray.toSeq, Seq(11, 12, 13))
  }

  test("scalar - NDArray[Int]") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    assertEquals((10 - a).toArray.toSeq, Seq(9, 8, 7))
  }

  // ── Unary ops ─────────────────────────────────────────────────────────────

  test("NDArray[Int] neg") {
    val a = NDArray(Array(1, -2, 3), Array(3))
    assertEquals(a.neg.toArray.toSeq, Seq(-1, 2, -3))
  }

  test("NDArray[Int] abs") {
    val a = NDArray(Array(-3, 0, 4), Array(3))
    assertEquals(a.abs.toArray.toSeq, Seq(3, 0, 4))
  }

  // ── Comparison ops ────────────────────────────────────────────────────────

  test("NDArray[Int] > NDArray[Int]") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    val b = NDArray(Array(2, 4, 3), Array(3))
    assertEquals((a > b).toArray.toSeq, Seq(false, true, false))
  }

  test("NDArray[Int] =:= scalar") {
    val a = NDArray(Array(1, 2, 1, 3), Array(4))
    assertEquals((a =:= 1).toArray.toSeq, Seq(true, false, true, false))
  }

  test("NDArray[Int] >= NDArray[Int]") {
    val a = NDArray(Array(1, 5, 3), Array(3))
    val b = NDArray(Array(2, 4, 3), Array(3))
    assertEquals((a >= b).toArray.toSeq, Seq(false, true, true))
  }

  // ── In-place ops ──────────────────────────────────────────────────────────

  test("NDArray[Int] += NDArray[Int]") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    val b = NDArray(Array(10, 20, 30), Array(3))
    a += b
    assertEquals(a.toArray.toSeq, Seq(11, 22, 33))
  }

  test("NDArray[Int] += scalar") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    a += 5
    assertEquals(a.toArray.toSeq, Seq(6, 7, 8))
  }

  test("NDArray[Int] in-place on non-contiguous throws") {
    val a = NDArray(Array(1, 2, 3, 4), Array(2, 2))
    val at = a.T // non-contiguous view
    val b = NDArray(Array(1, 1, 1, 1), Array(2, 2))
    intercept[InvalidNDArray] { at += b.T }
  }

  // ── Shape mismatch ────────────────────────────────────────────────────────

  test("NDArray[Int] + shape mismatch throws") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    val b = NDArray(Array(1, 2), Array(2))
    intercept[ShapeMismatchException] { a + b }
  }
```

### Test file: `vecxt/test/src/ndarrayIntReductions.test.scala`

```scala
class NDArrayIntReductionsSuite extends FunSuite:

  test("NDArray[Int] sum") {
    val a = NDArray(Array(1, 2, 3, 4), Array(4))
    assertEquals(a.sum, 10)
  }

  test("NDArray[Int] sum (col-major 2D)") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    assertEquals(a.sum, 21)
  }

  test("NDArray[Int] mean") {
    val a = NDArray(Array(2, 4, 6, 8), Array(4))
    assertEqualsDouble(a.mean, 5.0, 1e-10)
  }

  test("NDArray[Int] min / max") {
    val a = NDArray(Array(3, 1, 4, 1, 5, 9), Array(6))
    assertEquals(a.min, 1)
    assertEquals(a.max, 9)
  }

  test("NDArray[Int] product") {
    val a = NDArray(Array(1, 2, 3, 4), Array(4))
    assertEquals(a.product, 24)
  }

  test("NDArray[Int] argmax / argmin") {
    val a = NDArray(Array(3, 1, 4, 1, 5, 9), Array(6))
    assertEquals(a.argmax, 5)
    assertEquals(a.argmin, 1)
  }

  // ── Axis reductions (2D) ─────────────────────────────────────────────────

  test("NDArray[Int] sum(axis=0) on 2×3") {
    // col-major data [1,2,3,4,5,6] → shape [2,3]
    // col 0: [1,2], col 1: [3,4], col 2: [5,6]
    // sum along axis 0 → [3, 7, 11]
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.sum(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(3, 7, 11))
  }

  test("NDArray[Int] sum(axis=1) on 2×3") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.sum(1)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(9, 12))
  }

  test("NDArray[Int] max(axis=0) on 2×3") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val result = a.max(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(2, 4, 6))
  }

  test("NDArray[Int] mean(axis=0) returns NDArray[Double]") {
    // [1,2,3,4] shape [2,2] → mean along axis 0 → [1.5, 3.5]
    val a = NDArray(Array(1, 2, 3, 4), Array(2, 2))
    val result: NDArray[Double] = a.mean(0)
    assertEquals(result.shape.toSeq, Seq(2))
    assertNDArrayClose(result, Array(1.5, 3.5))
  }

  test("NDArray[Int] argmax(axis=0) returns NDArray[Int]") {
    // [1,5, 3,2, 6,4] shape [2,3] → argmax along axis 0 → [1, 0, 0]
    val a = NDArray(Array(1, 5, 3, 2, 6, 4), Array(2, 3))
    val result = a.argmax(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(1, 0, 0))
  }

  // ── Non-contiguous (strided view) reductions ──────────────────────────────

  test("NDArray[Int] sum on transposed view") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val t = a.T // shape [3,2], strides [2,1]
    assertEquals(t.sum, 21) // same total regardless of layout
  }

  test("NDArray[Int] axis reduction on transposed view") {
    val a = NDArray(Array(1, 2, 3, 4, 5, 6), Array(2, 3))
    val t = a.T // [3,2] view
    // t rows = a cols: [1,3,5] and [2,4,6]
    // sum(axis=0) collapses first dim → shape [2]
    val result = t.sum(0)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(9, 12))
  }

  // ── 3D axis reductions ───────────────────────────────────────────────────

  test("NDArray[Int] sum(axis=0) on 2×3×2 3D array") {
    // 12 elements shape [2,3,2] col-major
    val data = Array(1,2,3,4,5,6,7,8,9,10,11,12)
    val a = NDArray(data, Array(2, 3, 2))
    val result = a.sum(0)
    assertEquals(result.shape.toSeq, Seq(3, 2))
    assertEquals(result.toArray.toSeq, Seq(3, 7, 11, 15, 19, 23))
  }

  // ── Edge cases ────────────────────────────────────────────────────────────

  test("NDArray[Int] axis out of range throws") {
    val a = NDArray(Array(1, 2, 3), Array(3))
    intercept[InvalidNDArray] { a.sum(1) }
    intercept[InvalidNDArray] { a.sum(-1) }
  }
```

### Test file: `vecxt/test/src/ndarrayBooleanOps.test.scala`

```scala
class NDArrayBooleanOpsSuite extends FunSuite:

  // ── Logical binary ops ──────────────────────────────────────────────────

  test("NDArray[Boolean] && (col-major)") {
    val a = NDArray(Array(true, false, true, true), Array(4))
    val b = NDArray(Array(true, true, false, true), Array(4))
    assertEquals((a && b).toArray.toSeq, Seq(true, false, false, true))
  }

  test("NDArray[Boolean] || (col-major)") {
    val a = NDArray(Array(true, false, true, false), Array(4))
    val b = NDArray(Array(false, false, true, true), Array(4))
    assertEquals((a || b).toArray.toSeq, Seq(true, false, true, true))
  }

  test("NDArray[Boolean] && (general kernel — transposed)") {
    val a = NDArray(Array(true, false, true, false, true, true), Array(2, 3))
    val b = NDArray(Array(true, true, true, true, true, true), Array(2, 3))
    val at = a.T  // [3,2] non-col-major
    val bt = b.T
    val result = at && bt
    // result should equal at (since b is all-true)
    assertEquals(result.toArray.toSeq, at.toArray.toSeq)
    assert(result.isColMajor)
  }

  test("NDArray[Boolean] && shape mismatch throws") {
    val a = NDArray(Array(true, false), Array(2))
    val b = NDArray(Array(true, false, true), Array(3))
    intercept[ShapeMismatchException] { a && b }
  }

  // ── NOT ───────────────────────────────────────────────────────────────────

  test("NDArray[Boolean] not (col-major)") {
    val a = NDArray(Array(true, false, true), Array(3))
    assertEquals(a.not.toArray.toSeq, Seq(false, true, false))
  }

  test("NDArray[Boolean] not (general — transposed)") {
    val a = NDArray(Array(true, false, true, false), Array(2, 2))
    val result = a.T.not
    assertEquals(result.toArray.toSeq, a.T.toArray.map(!_).toSeq)
  }

  // ── Reductions ──────────────────────────────────────────────────────────

  test("NDArray[Boolean] any — all false") {
    val a = NDArray(Array(false, false, false), Array(3))
    assertEquals(a.any, false)
  }

  test("NDArray[Boolean] any — one true") {
    val a = NDArray(Array(false, true, false), Array(3))
    assertEquals(a.any, true)
  }

  test("NDArray[Boolean] all — all true") {
    val a = NDArray(Array(true, true, true), Array(3))
    assertEquals(a.all, true)
  }

  test("NDArray[Boolean] all — one false") {
    val a = NDArray(Array(true, false, true), Array(3))
    assertEquals(a.all, false)
  }

  test("NDArray[Boolean] countTrue") {
    val a = NDArray(Array(true, false, true, true, false, true), Array(6))
    assertEquals(a.countTrue, 4)
  }

  // ── Non-contiguous reductions ──────────────────────────────────────────

  test("NDArray[Boolean] any on transposed view") {
    val a = NDArray(Array(false, false, true, false), Array(2, 2))
    val t = a.T
    assertEquals(t.any, true)
  }

  test("NDArray[Boolean] all on transposed view") {
    val a = NDArray(Array(true, true, true, true), Array(2, 2))
    val t = a.T
    assertEquals(t.all, true)
  }

  test("NDArray[Boolean] countTrue on transposed view") {
    val a = NDArray(Array(true, false, true, false, true, true), Array(2, 3))
    val t = a.T
    assertEquals(t.countTrue, 4) // same total as a
  }

  // ── Axis reductions ───────────────────────────────────────────────────

  test("NDArray[Boolean] any(axis=0) on 2×3") {
    // col-major [true,false, true,true, false,true] shape [2,3]
    // col 0: [T,F]→T, col 1: [T,T]→T, col 2: [F,T]→T
    val a = NDArray(Array(true, false, true, true, false, true), Array(2, 3))
    val result = a.any(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(true, true, true))
  }

  test("NDArray[Boolean] all(axis=0) on 2×3") {
    val a = NDArray(Array(true, false, true, true, false, true), Array(2, 3))
    val result = a.all(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(false, true, false))
  }

  test("NDArray[Boolean] countTrue(axis=1) on 2×3") {
    // col-major [true,false, true,true, false,true] shape [2,3]
    // row 0 across cols: T, T, F → count=2
    // row 1 across cols: F, T, T → count=2
    val a = NDArray(Array(true, false, true, true, false, true), Array(2, 3))
    val result = a.countTrue(1)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(2, 2))
  }

  // ── In-place NOT ──────────────────────────────────────────────────────

  test("NDArray[Boolean] not! in-place") {
    val a = NDArray(Array(true, false, true), Array(3))
    a.`not!`
    assertEquals(a.toArray.toSeq, Seq(false, true, false))
  }
```

### Test file: `vecxt/test/src/ndarrayFloatOps.test.scala`

```scala
class NDArrayFloatOpsSuite extends FunSuite:

  private val eps = 1e-5f

  private def assertNDArrayFloatClose(
      actual: NDArray[Float],
      expected: Array[Float]
  )(implicit loc: munit.Location): Unit =
    val arr = actual.toArray
    assertEquals(arr.length, expected.length, "length mismatch")
    for i <- expected.indices do
      assert(Math.abs(arr(i) - expected(i)) < eps, s"element $i: expected ${expected(i)} got ${arr(i)}")
    end for
  end assertNDArrayFloatClose

  // ── Binary ops ──────────────────────────────────────────────────────────

  test("NDArray[Float] + NDArray[Float] (col-major 1D)") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val b = NDArray(Array(4.0f, 5.0f, 6.0f), Array(3))
    assertNDArrayFloatClose(a + b, Array(5.0f, 7.0f, 9.0f))
  }

  test("NDArray[Float] - NDArray[Float]") {
    val a = NDArray(Array(5.0f, 6.0f, 7.0f), Array(3))
    val b = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertNDArrayFloatClose(a - b, Array(4.0f, 4.0f, 4.0f))
  }

  test("NDArray[Float] * NDArray[Float] (col-major 2D)") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f), Array(2, 2))
    val b = NDArray(Array(2.0f, 3.0f, 4.0f, 5.0f), Array(2, 2))
    assertNDArrayFloatClose(a * b, Array(2.0f, 6.0f, 12.0f, 20.0f))
  }

  test("NDArray[Float] / NDArray[Float]") {
    val a = NDArray(Array(6.0f, 9.0f, 12.0f), Array(3))
    val b = NDArray(Array(2.0f, 3.0f, 4.0f), Array(3))
    assertNDArrayFloatClose(a / b, Array(3.0f, 3.0f, 3.0f))
  }

  // ── General kernel (non-col-major) ────────────────────────────────────

  test("NDArray[Float] + via general kernel (transposed)") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val at = a.T
    val bt = a.T
    val result = at + bt
    val expected = at.toArray.map(_ * 2.0f)
    assertNDArrayFloatClose(result, expected)
    assert(result.isColMajor)
  }

  // ── Scalar ops ────────────────────────────────────────────────────────

  test("NDArray[Float] + scalar") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertNDArrayFloatClose(a + 10.0f, Array(11.0f, 12.0f, 13.0f))
  }

  test("scalar * NDArray[Float]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertNDArrayFloatClose(2.0f * a, Array(2.0f, 4.0f, 6.0f))
  }

  // ── Unary ops ─────────────────────────────────────────────────────────

  test("NDArray[Float] neg") {
    val a = NDArray(Array(1.0f, -2.0f, 3.0f), Array(3))
    assertNDArrayFloatClose(a.neg, Array(-1.0f, 2.0f, -3.0f))
  }

  test("NDArray[Float] abs") {
    val a = NDArray(Array(-3.0f, 0.0f, 4.0f), Array(3))
    assertNDArrayFloatClose(a.abs, Array(3.0f, 0.0f, 4.0f))
  }

  test("NDArray[Float] exp") {
    val a = NDArray(Array(0.0f, 1.0f), Array(2))
    assertNDArrayFloatClose(a.exp, Array(1.0f, Math.E.toFloat))
  }

  test("NDArray[Float] log") {
    val a = NDArray(Array(1.0f, Math.E.toFloat), Array(2))
    assertNDArrayFloatClose(a.log, Array(0.0f, 1.0f))
  }

  test("NDArray[Float] sqrt") {
    val a = NDArray(Array(4.0f, 9.0f, 16.0f), Array(3))
    assertNDArrayFloatClose(a.sqrt, Array(2.0f, 3.0f, 4.0f))
  }

  // ── Comparison ops ────────────────────────────────────────────────────

  test("NDArray[Float] > NDArray[Float]") {
    val a = NDArray(Array(1.0f, 5.0f, 3.0f), Array(3))
    val b = NDArray(Array(2.0f, 4.0f, 3.0f), Array(3))
    assertEquals((a > b).toArray.toSeq, Seq(false, true, false))
  }

  test("NDArray[Float] >= scalar") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    assertEquals((a >= 2.0f).toArray.toSeq, Seq(false, true, true))
  }

  // ── In-place ──────────────────────────────────────────────────────────

  test("NDArray[Float] += NDArray[Float]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val b = NDArray(Array(10.0f, 20.0f, 30.0f), Array(3))
    a += b
    assertNDArrayFloatClose(a, Array(11.0f, 22.0f, 33.0f))
  }

  // ── Shape mismatch ────────────────────────────────────────────────────

  test("NDArray[Float] + shape mismatch throws") {
    val a = NDArray(Array(1.0f, 2.0f), Array(2))
    val b = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    intercept[ShapeMismatchException] { a + b }
  }
```

### Test file: `vecxt/test/src/ndarrayFloatReductions.test.scala`

```scala
class NDArrayFloatReductionsSuite extends FunSuite:

  private val eps = 1e-5f

  test("NDArray[Float] sum") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f), Array(4))
    assert(Math.abs(a.sum - 10.0f) < eps)
  }

  test("NDArray[Float] mean") {
    val a = NDArray(Array(2.0f, 4.0f, 6.0f, 8.0f), Array(4))
    assert(Math.abs(a.mean - 5.0f) < eps)
  }

  test("NDArray[Float] min / max") {
    val a = NDArray(Array(3.0f, 1.0f, 4.0f, 1.0f, 5.0f, 9.0f), Array(6))
    assert(Math.abs(a.min - 1.0f) < eps)
    assert(Math.abs(a.max - 9.0f) < eps)
  }

  test("NDArray[Float] product") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f), Array(4))
    assert(Math.abs(a.product - 24.0f) < eps)
  }

  test("NDArray[Float] variance") {
    // [1,2,3,4] mean=2.5, var = ((1-2.5)^2 + ... ) / 4 = 1.25
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f), Array(4))
    assert(Math.abs(a.variance - 1.25f) < eps)
  }

  test("NDArray[Float] norm") {
    val a = NDArray(Array(3.0f, 4.0f), Array(2))
    assert(Math.abs(a.norm - 5.0f) < eps)
  }

  test("NDArray[Float] argmax / argmin") {
    val a = NDArray(Array(3.0f, 1.0f, 4.0f, 1.0f, 5.0f, 9.0f), Array(6))
    assertEquals(a.argmax, 5)
    assertEquals(a.argmin, 1)
  }

  // ── Axis reductions ───────────────────────────────────────────────────

  test("NDArray[Float] sum(axis=0) on 2×3") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val result = a.sum(0)
    assertEquals(result.shape.toSeq, Seq(3))
    val expected = Array(3.0f, 7.0f, 11.0f)
    result.toArray.zip(expected).foreach { (actual, exp) =>
      assert(Math.abs(actual - exp) < eps, s"expected $exp got $actual")
    }
  }

  test("NDArray[Float] sum on transposed view") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val t = a.T
    assert(Math.abs(t.sum - 21.0f) < eps)
  }
```

### Test file: `vecxt/test/src/ndarrayBooleanIndexing.test.scala`

```scala
class NDArrayBooleanIndexingSuite extends FunSuite:

  // ── Boolean indexing ──────────────────────────────────────────────────

  test("Boolean indexing on 1D NDArray[Double]") {
    val a = NDArray(Array(10.0, 20.0, 30.0, 40.0, 50.0), Array(5))
    val mask = NDArray(Array(true, false, true, false, true), Array(5))
    val result = a(mask)
    assertEquals(result.shape.toSeq, Seq(3))
    assertNDArrayClose(result, Array(10.0, 30.0, 50.0))
  }

  test("Boolean indexing on 2D NDArray[Double] (flattens to 1D)") {
    // col-major [1,2,3,4] shape [2,2]
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val mask = NDArray(Array(true, false, false, true), Array(2, 2))
    val result = a(mask)
    assertEquals(result.shape.toSeq, Seq(2))
    // col-major order: mask selects elements at flat idx 0 and 3 → 1.0, 4.0
    assertNDArrayClose(result, Array(1.0, 4.0))
  }

  test("Boolean indexing on NDArray[Int]") {
    val a = NDArray(Array(10, 20, 30, 40, 50), Array(5))
    val mask = NDArray(Array(true, false, true, false, true), Array(5))
    val result = a(mask)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(10, 30, 50))
  }

  test("Boolean indexing on NDArray[Float]") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    val mask = NDArray(Array(false, true, true), Array(3))
    val result = a(mask)
    assertEquals(result.shape.toSeq, Seq(2))
    assertEquals(result.toArray.toSeq, Seq(2.0f, 3.0f))
  }

  test("Boolean indexing — all false") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(false, false, false), Array(3))
    val result = a(mask)
    assertEquals(result.shape.toSeq, Seq(0))
    assertEquals(result.toArray.length, 0)
  }

  test("Boolean indexing — all true") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(true, true, true), Array(3))
    val result = a(mask)
    assertEquals(result.shape.toSeq, Seq(3))
    assertNDArrayClose(result, Array(1.0, 2.0, 3.0))
  }

  test("Boolean indexing — shape mismatch throws") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val mask = NDArray(Array(true, false), Array(2))
    intercept[ShapeMismatchException] { a(mask) }
  }

  test("Boolean indexing on transposed view") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val mask = NDArray(Array(true, false, true, false), Array(2, 2))
    // a.T => transposed view (non-col-major)
    // mask.T => transposed mask
    val result = a.T(mask.T)
    // T swaps shape: [2,2]→[2,2] with different strides
    // a.T in col-major order: [1,3,2,4]; mask.T in col-major: [T,T,F,F]
    // selected: [1, 3]
    assertEquals(result.shape.toSeq, Seq(2))
    assertNDArrayClose(result, Array(1.0, 3.0))
  }
```

### Test file: `vecxt/test/src/ndarrayWhere.test.scala`

```scala
class NDArrayWhereSuite extends FunSuite:

  test("where(condition, x, y) element-wise — Double") {
    val cond = NDArray(Array(true, false, true, false), Array(4))
    val x = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val y = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(4))
    val result = where(cond, x, y)
    assertNDArrayClose(result, Array(1.0, 20.0, 3.0, 40.0))
  }

  test("where(condition, x, y) — Int") {
    val cond = NDArray(Array(true, false, true), Array(3))
    val x = NDArray(Array(1, 2, 3), Array(3))
    val y = NDArray(Array(10, 20, 30), Array(3))
    val result = where(cond, x, y)
    assertEquals(result.toArray.toSeq, Seq(1, 20, 3))
  }

  test("where(condition, x, y) — Float") {
    val cond = NDArray(Array(false, true), Array(2))
    val x = NDArray(Array(1.0f, 2.0f), Array(2))
    val y = NDArray(Array(10.0f, 20.0f), Array(2))
    val result = where(cond, x, y)
    assertEquals(result.toArray.toSeq, Seq(10.0f, 2.0f))
  }

  test("where with scalar x") {
    val cond = NDArray(Array(true, false, true), Array(3))
    val y = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    val result = where(cond, 0.0, y)
    assertNDArrayClose(result, Array(0.0, 20.0, 0.0))
  }

  test("where with scalar y") {
    val cond = NDArray(Array(true, false, true), Array(3))
    val x = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val result = where(cond, x, 0.0)
    assertNDArrayClose(result, Array(1.0, 0.0, 3.0))
  }

  test("where with both scalars") {
    val cond = NDArray(Array(true, false, true, false), Array(4))
    val result = where(cond, 1.0, 0.0)
    assertNDArrayClose(result, Array(1.0, 0.0, 1.0, 0.0))
  }

  test("where 2D (col-major)") {
    val cond = NDArray(Array(true, false, false, true), Array(2, 2))
    val x = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val y = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(2, 2))
    val result = where(cond, x, y)
    assertEquals(result.shape.toSeq, Seq(2, 2))
    assertNDArrayClose(result, Array(1.0, 20.0, 30.0, 4.0))
  }

  test("where on transposed views") {
    val cond = NDArray(Array(true, false, false, true), Array(2, 2))
    val x = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val y = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(2, 2))
    val result = where(cond.T, x.T, y.T)
    // .T swaps strides → general kernel path
    // cond.T col-major: [T, F, F, T]; x.T: [1,3,2,4]; y.T: [10,30,20,40]
    assertNDArrayClose(result, Array(1.0, 30.0, 20.0, 4.0))
    assert(result.isColMajor)
  }

  test("where shape mismatch throws") {
    val cond = NDArray(Array(true, false, true), Array(3))
    val x = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val y = NDArray(Array(10.0, 20.0), Array(2))
    intercept[ShapeMismatchException] { where(cond, x, y) }
  }

  test("where condition shape mismatch throws") {
    val cond = NDArray(Array(true, false), Array(2))
    val x = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val y = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    intercept[ShapeMismatchException] { where(cond, x, y) }
  }
```

---

## Implementation Order

Execute in this order for fastest feedback loops:

0. **Fix existing Double `argmax`/`argmin`** → change `NDArray[Double]` to `NDArray[Int]` in `ndarrayReductions.scala` + update tests → `./mill vecxt.__.test`
1. **`ndarrayIntOps.scala`** + `ndarrayIntReductions.scala` + tests → compile + test
2. **`ndarrayBooleanOps.scala`** (ops + reductions in one file) + tests → compile + test
3. **`ndarrayFloatOps.scala`** + `ndarrayFloatReductions.scala` + tests → compile + test
4. **`ndarrayBooleanIndexing.scala`** + tests → compile + test
5. **`ndarrayWhere.scala`** + tests → compile + test
6. **`all.scala` exports** → compile + full test run
7. **Format**: `./mill mill.scalalib.scalafmt.ScalafmtModule/`
8. **Full cross-platform test**: `./mill vecxt.__.test`

---

## Files to Create (summary)

| File | Object | Purpose |
|------|--------|---------|
| `vecxt/src/ndarrayIntOps.scala` | `NDArrayIntOps` | Element-wise + comparison ops for `NDArray[Int]` |
| `vecxt/src/ndarrayIntReductions.scala` | `NDArrayIntReductions` | Full + axis reductions for `NDArray[Int]` |
| `vecxt/src/ndarrayBooleanOps.scala` | `NDArrayBooleanOps` | Logical ops + reductions for `NDArray[Boolean]` |
| `vecxt/src/ndarrayFloatOps.scala` | `NDArrayFloatOps` | Element-wise + comparison ops for `NDArray[Float]` |
| `vecxt/src/ndarrayFloatReductions.scala` | `NDArrayFloatReductions` | Full + axis reductions for `NDArray[Float]` |
| `vecxt/src/ndarrayBooleanIndexing.scala` | `NDArrayBooleanIndexing` | Boolean mask selection on any `NDArray[A]` |
| `vecxt/src/ndarrayWhere.scala` | `NDArrayWhere` | `where(cond, x, y)` conditional selection |
| `vecxt/test/src/ndarrayIntOps.test.scala` | `NDArrayIntOpsSuite` | Int ops tests |
| `vecxt/test/src/ndarrayIntReductions.test.scala` | `NDArrayIntReductionsSuite` | Int reduction tests |
| `vecxt/test/src/ndarrayBooleanOps.test.scala` | `NDArrayBooleanOpsSuite` | Boolean ops + reduction tests |
| `vecxt/test/src/ndarrayFloatOps.test.scala` | `NDArrayFloatOpsSuite` | Float ops tests |
| `vecxt/test/src/ndarrayFloatReductions.test.scala` | `NDArrayFloatReductionsSuite` | Float reduction tests |
| `vecxt/test/src/ndarrayBooleanIndexing.test.scala` | `NDArrayBooleanIndexingSuite` | Boolean indexing tests |
| `vecxt/test/src/ndarrayWhere.test.scala` | `NDArrayWhereSuite` | where() tests |

## Files to Modify

| File | Change |
|------|--------|
| `vecxt/src/ndarrayReductions.scala` | Change `argReduceAxis` return type from `NDArray[Double]` → `NDArray[Int]`; update `argmax(axis)`/`argmin(axis)` signatures |
| `vecxt/test/src/ndarrayReductions.test.scala` | Update argmax/argmin axis tests from `assertNDArrayShapeAndClose` to Int assertions |
| `vecxt/src/all.scala` | Add 7 new export lines |

---

## Critical Implementation Notes

1. **All new source files go in `vecxt/src/` (shared cross-platform).** No `src-jvm/`/`src-js/` files — the two-tier dispatch (col-major flat loop vs general stride kernel) handles platform differences automatically. The flat loops are already fast on all platforms.

2. **Import pattern for every new source file:**
   ```scala
   package vecxt
   import vecxt.broadcast.*
   import vecxt.ndarray.*
   ```

3. **Use `mkNDArray` (unchecked factory) for results**, not `NDArray.apply` — the ops have already validated shapes. This matches the existing pattern in `ndarrayDoubleOps.scala`.

4. **The `@@` operator on Double is *not* needed for Int/Float** — no matrix multiply for these types. `dot` on Int/Float is also out of scope (no BLAS delegation possible).

5. **Do not add `@specialized`** — as noted in the design doc, it adds complexity without benefit.

6. **`@targetName` annotations** are required for left-scalar extension methods to avoid JVM signature collisions. Look at `ndarrayDoubleOps.scala` for the exact naming convention.

7. **In-place ops must check `a.isContiguous`** and throw `InvalidNDArray("In-place operation requires contiguous array")` if not. This matches the existing pattern.

8. **Run `./mill mill.scalalib.scalafmt.ScalafmtModule/`** before committing — CI enforces formatting.

9. **Compile with `./mill vecxt.__.compile`** (all platforms). Cold compile takes ~2 minutes.

10. **Test with `./mill vecxt.__.test`** (all platforms). Run JVM-only first for faster iteration: `./mill vecxt.jvm.test`.
