# Milestone 3: Element-wise Operations for `NDArray[Double]` — Implementation Plan

## Context

This is the third coding milestone for NDArray support in vecxt.

**Prerequisites:** M1 (NDArray core type + factories, `vecxt/src/ndarray.scala`) and M2
(indexing + views, `vecxt/src/ndarrayOps.scala`) must be merged before this work begins.

Read `site/notes/ndarray-design.md` for overall design rationale.
Read `styleguide.md` and `.github/copilot-instructions.md` before writing any code.

## Goal

After this milestone, `NDArray[Double]` supports all element-wise arithmetic with:

- Correct broadcasting (NumPy semantics)
- Platform-specific performance: SIMD on the JVM for contiguous arrays; `while` loops on JS and Native
- Consistent cross-platform API
- `BoundsCheck`-erasable shape validation

## Background: What M1/M2 provide

From `vecxt/src/ndarray.scala` (after M1/M2 merge):

```scala
class NDArray[A] private[ndarray] (
  val data: Array[A],     // backing storage
  val shape: Array[Int],  // dimensions [d0, d1, ..., dn-1]
  val strides: Array[Int],// step per dimension [s0, s1, ..., sn-1]
  val offset: Int         // start position in data
)

// Available private[vecxt] helpers:
colMajorStrides(shape): Array[Int]       // strides = [1, d0, d0*d1, ...]
shapeProduct(shape): Int                 // product of all dims
mkNDArray(data, shape, strides, offset)  // unchecked constructor for views
```

Lazy properties: `ndim`, `numel`, `isColMajor`, `isRowMajor`, `isContiguous`.

Element access via `ndarrayOps`: `arr(i)`, `arr(i, j)`, `arr.slice(dim, start, end)`,
`arr.T`, `arr.transpose(perm)`, `arr.reshape(newShape)`, `arr.toArray`.

## Operations to implement

### Binary element-wise (both `NDArray[Double]` operands, broadcasting)
`+`, `-`, `*`, `/`

### Scalar element-wise (`NDArray[Double]` op `Double`)
`+ (s: Double)`, `- (s: Double)`, `* (s: Double)`, `/ (s: Double)`

### Unary element-wise
`neg` / `unary_-`, `abs`, `exp`, `log`, `sqrt`, `tanh`, `sigmoid`

### In-place element-wise (mutate backing array)
`+= NDArray`, `-= NDArray`, `*= NDArray`, `/= NDArray`
`+= Double`, `-= Double`, `*= Double`, `/= Double`

### Comparison (return `NDArray[Boolean]`)
`>`, `<`, `>=`, `<=`, `=:=`

---

## Broadcasting design

Broadcasting follows NumPy semantics:
1. Shapes are right-aligned.
2. Dimensions are compatible if equal or one of them is 1.
3. A dimension of 1 in the input is broadcast (stride 0 in the view) to match.

### `broadcastShapes`

```scala
private[vecxt] def broadcastShapes(a: Array[Int], b: Array[Int]): Array[Int] =
  val ndim = math.max(a.length, b.length)
  val out  = new Array[Int](ndim)
  var i    = 0
  while i < ndim do
    val ai = if i < ndim - a.length then 1 else a(i - (ndim - a.length))
    val bi = if i < ndim - b.length then 1 else b(i - (ndim - b.length))
    if ai != bi && ai != 1 && bi != 1 then
      throw InvalidNDArray(
        s"Shapes [${a.mkString(",")}] and [${b.mkString(",")}] are not broadcastable at dim $i: $ai vs $bi"
      )
    out(i) = math.max(ai, bi)
    i += 1
  end while
  out
end broadcastShapes
```

### `broadcastTo`

```scala
// Return a view of arr broadcast to shape.
// Dimensions where arr has size 1 get stride 0.
// Prepended dimensions also get stride 0. No data is copied.
private[vecxt] def broadcastTo(arr: NDArray[Double], shape: Array[Int]): NDArray[Double] =
  val ndim    = shape.length
  val newStrides = new Array[Int](ndim)
  val pad     = ndim - arr.ndim
  var i = 0
  while i < ndim do
    if i < pad then
      newStrides(i) = 0
    else
      val srcDim = i - pad
      newStrides(i) = if arr.shape(srcDim) == 1 then 0 else arr.strides(srcDim)
    end if
    i += 1
  end while
  mkNDArray(arr.data, shape, newStrides, arr.offset)
end broadcastTo
```

### Generic odometer iteration

```scala
// Generic element-wise binary op for arbitrary strides / broadcasting.
// Produces a fresh col-major NDArray.
private[vecxt] def genericBinaryOp(
    a: NDArray[Double],
    b: NDArray[Double]
)(op: (Double, Double) => Double): NDArray[Double] =
  val outShape = broadcastShapes(a.shape, b.shape)
  val n        = shapeProduct(outShape)
  val out      = new Array[Double](n)
  val ba       = broadcastTo(a, outShape)
  val bb       = broadcastTo(b, outShape)
  val ndim     = outShape.length
  val indices  = new Array[Int](ndim)

  var k = 0
  while k < n do
    var ia = ba.offset
    var ib = bb.offset
    var d  = 0
    while d < ndim do
      ia += indices(d) * ba.strides(d)
      ib += indices(d) * bb.strides(d)
      d += 1
    end while
    out(k) = op(ba.data(ia), bb.data(ib))

    // advance odometer: col-major (index 0 changes fastest)
    d = 0
    while d < ndim do
      indices(d) += 1
      if indices(d) < outShape(d) then d = ndim // break
      else
        indices(d) = 0
        d += 1
      end if
    end while
    k += 1
  end while

  mkNDArray(out, outShape, colMajorStrides(outShape), 0)
end genericBinaryOp

// Generic in-place binary op. b is broadcast to a's shape.
private[vecxt] def genericInPlaceBinaryOp(
    a: NDArray[Double],
    b: NDArray[Double]
)(op: (Double, Double) => Double): Unit =
  val bb    = broadcastTo(b, a.shape)
  val n     = a.numel
  val ndim  = a.ndim
  val indices = new Array[Int](ndim)

  var k = 0
  while k < n do
    var ia = a.offset
    var ib = bb.offset
    var d  = 0
    while d < ndim do
      ia += indices(d) * a.strides(d)
      ib += indices(d) * bb.strides(d)
      d += 1
    end while
    a.data(ia) = op(a.data(ia), bb.data(ib))

    d = 0
    while d < ndim do
      indices(d) += 1
      if indices(d) < a.shape(d) then d = ndim
      else
        indices(d) = 0
        d += 1
      end if
    end while
    k += 1
  end while
end genericInPlaceBinaryOp

// Generic unary op. Produces a fresh col-major NDArray.
private[vecxt] def genericUnaryOp(a: NDArray[Double])(op: Double => Double): NDArray[Double] =
  val n       = a.numel
  val out     = new Array[Double](n)
  val ndim    = a.ndim
  val indices = new Array[Int](ndim)

  var k = 0
  while k < n do
    var ia = a.offset
    var d  = 0
    while d < ndim do
      ia += indices(d) * a.strides(d)
      d += 1
    end while
    out(k) = op(a.data(ia))

    d = 0
    while d < ndim do
      indices(d) += 1
      if indices(d) < a.shape(d) then d = ndim
      else
        indices(d) = 0
        d += 1
      end if
    end while
    k += 1
  end while

  mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
end genericUnaryOp

// Generic comparison op. Produces a fresh col-major NDArray[Boolean].
private[vecxt] def genericCompareOp(
    a: NDArray[Double],
    b: NDArray[Double]
)(op: (Double, Double) => Boolean): NDArray[Boolean] =
  val outShape = broadcastShapes(a.shape, b.shape)
  val n        = shapeProduct(outShape)
  val out      = new Array[Boolean](n)
  val ba       = broadcastTo(a, outShape)
  val bb       = broadcastTo(b, outShape)
  val ndim     = outShape.length
  val indices  = new Array[Int](ndim)

  var k = 0
  while k < n do
    var ia = ba.offset
    var ib = bb.offset
    var d  = 0
    while d < ndim do
      ia += indices(d) * ba.strides(d)
      ib += indices(d) * bb.strides(d)
      d += 1
    end while
    out(k) = op(ba.data(ia), bb.data(ib))

    d = 0
    while d < ndim do
      indices(d) += 1
      if indices(d) < outShape(d) then d = ndim
      else
        indices(d) = 0
        d += 1
      end if
    end while
    k += 1
  end while

  mkNDArray(out, outShape, colMajorStrides(outShape), 0)
end genericCompareOp
```

---

## Files to create / modify

### 1. `vecxt/src/ndarray_elemwise.scala` — Cross-platform shared utilities

Package: `vecxt`. Contains `broadcastShapes`, `broadcastTo`, and the five generic helper
functions above. These are `private[vecxt]` so accessible from any file in `package vecxt`.

```scala
package vecxt

import vecxt.ndarray.*
import vecxt.ndarray.NDArray

object NDArrayElemwiseShared:
  private[vecxt] def broadcastShapes(a: Array[Int], b: Array[Int]): Array[Int] = ...
  private[vecxt] def broadcastTo(arr: NDArray[Double], shape: Array[Int]): NDArray[Double] = ...
  private[vecxt] def genericBinaryOp(a: NDArray[Double], b: NDArray[Double])(op: (Double, Double) => Double): NDArray[Double] = ...
  private[vecxt] def genericInPlaceBinaryOp(a: NDArray[Double], b: NDArray[Double])(op: (Double, Double) => Double): Unit = ...
  private[vecxt] def genericUnaryOp(a: NDArray[Double])(op: Double => Double): NDArray[Double] = ...
  private[vecxt] def genericCompareOp(a: NDArray[Double], b: NDArray[Double])(op: (Double, Double) => Boolean): NDArray[Boolean] = ...
end NDArrayElemwiseShared
```

### 2. `vecxt/src-jvm/ndarray_elemwise_jvm.scala` — JVM SIMD fast paths

Package: `vecxt`. Defines `object NDArrayDoubleOps` with extension methods on `NDArray[Double]`.

**Fast path condition:** `a.isColMajor && b.isColMajor && java.util.Arrays.equals(a.shape, b.shape)`

When fast path applies, use SIMD `DoubleVector` on the flat `data` arrays (adjusted by
`offset`). When it does not apply, delegate to the generic functions.

```scala
package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.ndarray.*
import vecxt.ndarray.NDArray
import vecxt.NDArrayElemwiseShared.*
import vecxt.arrays.{spd, spdl}

import jdk.incubator.vector.DoubleVector
import jdk.incubator.vector.VectorOperators

import java.util.Arrays.equals as arrayEquals

object NDArrayDoubleOps:

  extension (a: NDArray[Double])

    // Binary ops
    inline def +(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] =
      inline if bc then broadcastShapes(a.shape, b.shape) end if
      if a.isColMajor && b.isColMajor && arrayEquals(a.shape, b.shape) then
        val n   = a.numel
        val out = new Array[Double](n)
        var i   = 0
        while i < spd.loopBound(n) do
          DoubleVector.fromArray(spd, a.data, a.offset + i)
            .add(DoubleVector.fromArray(spd, b.data, b.offset + i))
            .intoArray(out, i)
          i += spdl
        end while
        while i < n do
          out(i) = a.data(a.offset + i) + b.data(b.offset + i)
          i += 1
        end while
        mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
      else
        genericBinaryOp(a, b)(_ + _)
      end if
    end +

    // (- , *, / follow the same SIMD pattern using .sub, .mul, .div)
    inline def -(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] = ...
    inline def *(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] = ...
    inline def /(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] = ...

    // Scalar ops — broadcast the scalar via DoubleVector.broadcast
    inline def +(s: Double): NDArray[Double] =
      if a.isColMajor then
        val n   = a.numel
        val out = new Array[Double](n)
        val sv  = DoubleVector.broadcast(spd, s)
        var i   = 0
        while i < spd.loopBound(n) do
          DoubleVector.fromArray(spd, a.data, a.offset + i).add(sv).intoArray(out, i)
          i += spdl
        end while
        while i < n do
          out(i) = a.data(a.offset + i) + s
          i += 1
        end while
        mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
      else
        genericUnaryOp(a)(_ + s)
      end if
    end +

    inline def -(s: Double): NDArray[Double] = a + (-s)
    inline def *(s: Double): NDArray[Double] = ...  // DoubleVector.broadcast + .mul
    inline def /(s: Double): NDArray[Double] = a * (1.0 / s)

    // Unary ops — use VectorOperators lanewise for col-major
    inline def neg: NDArray[Double] =
      if a.isColMajor then
        val n   = a.numel
        val out = new Array[Double](n)
        var i   = 0
        while i < spd.loopBound(n) do
          DoubleVector.fromArray(spd, a.data, a.offset + i)
            .lanewise(VectorOperators.NEG).intoArray(out, i)
          i += spdl
        end while
        while i < n do
          out(i) = -a.data(a.offset + i)
          i += 1
        end while
        mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
      else genericUnaryOp(a)(-_)
    end neg

    inline def unary_- : NDArray[Double] = a.neg

    // abs, exp, log, sqrt, tanh follow the same pattern with ABS, EXP, LOG, SQRT, TANH
    inline def abs: NDArray[Double]  = ...  // VectorOperators.ABS
    inline def exp: NDArray[Double]  = ...  // VectorOperators.EXP
    inline def log: NDArray[Double]  = ...  // VectorOperators.LOG
    inline def sqrt: NDArray[Double] = ...  // VectorOperators.SQRT
    inline def tanh: NDArray[Double] = ...  // VectorOperators.TANH

    // sigmoid = 1 / (1 + exp(-x))  — composed from SIMD NEG, EXP, ADD, DIV
    inline def sigmoid: NDArray[Double] =
      if a.isColMajor then
        val n   = a.numel
        val out = new Array[Double](n)
        val one = DoubleVector.broadcast(spd, 1.0)
        var i   = 0
        while i < spd.loopBound(n) do
          val x = DoubleVector.fromArray(spd, a.data, a.offset + i)
          one.div(x.lanewise(VectorOperators.NEG).lanewise(VectorOperators.EXP).add(one))
            .intoArray(out, i)
          i += spdl
        end while
        while i < n do
          out(i) = 1.0 / (1.0 + math.exp(-a.data(a.offset + i)))
          i += 1
        end while
        mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
      else
        genericUnaryOp(a)(x => 1.0 / (1.0 + math.exp(-x)))
    end sigmoid

    // In-place binary — SIMD when contiguous same-shape, otherwise generic
    inline def +=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit =
      inline if bc then broadcastShapes(a.shape, b.shape) end if
      if a.isColMajor && b.isColMajor && arrayEquals(a.shape, b.shape) then
        val n = a.numel
        var i = 0
        while i < spd.loopBound(n) do
          val av = DoubleVector.fromArray(spd, a.data, a.offset + i)
          val bv = DoubleVector.fromArray(spd, b.data, b.offset + i)
          av.add(bv).intoArray(a.data, a.offset + i)
          i += spdl
        end while
        while i < n do
          a.data(a.offset + i) += b.data(b.offset + i)
          i += 1
        end while
      else
        genericInPlaceBinaryOp(a, b)(_ + _)
      end if
    end +=

    // (-=, *=, /= follow same pattern)
    inline def -=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit = ...
    inline def *=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit = ...
    inline def /=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit = ...

    // In-place scalar — SIMD broadcast when col-major
    inline def +=(s: Double): Unit =
      if a.isColMajor then
        val sv = DoubleVector.broadcast(spd, s)
        val n  = a.numel
        var i  = 0
        while i < spd.loopBound(n) do
          DoubleVector.fromArray(spd, a.data, a.offset + i).add(sv)
            .intoArray(a.data, a.offset + i)
          i += spdl
        end while
        while i < n do
          a.data(a.offset + i) += s
          i += 1
        end while
      else
        genericInPlaceBinaryOp(a, NDArray.fill(a.shape, s)(using BoundsCheck.DoBoundsCheck.no))(_ + _)
    end +=

    inline def -=(s: Double): Unit = a += (-s)
    inline def *=(s: Double): Unit = ...  // SIMD .mul(sv)
    inline def /=(s: Double): Unit = a *= (1.0 / s)

    // Comparison ops — generic path always (no Boolean SIMD in vecxt yet)
    inline def >(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]   = genericCompareOp(a, b)(_ > _)
    inline def <(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]   = genericCompareOp(a, b)(_ < _)
    inline def >=(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]  = genericCompareOp(a, b)(_ >= _)
    inline def <=(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]  = genericCompareOp(a, b)(_ <= _)
    inline def =:=(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean] = genericCompareOp(a, b)(_ == _)

  end extension

end NDArrayDoubleOps
```

### 3. `vecxt/src-js-native/ndarray_elemwise_jsnative.scala` — JS + Native while loops

Package: `vecxt`. Defines `object JsNativeNDArrayDoubleOps` with the **same extension method
names** as `NDArrayDoubleOps`. The fast path for contiguous same-shape arrays still exists
(plain while loop on the flat data) for better performance than odometer overhead.

```scala
package vecxt

import vecxt.BoundsCheck.BoundsCheck
import vecxt.ndarray.*
import vecxt.ndarray.NDArray
import vecxt.NDArrayElemwiseShared.*

import java.util.Arrays.equals as arrayEquals

object JsNativeNDArrayDoubleOps:

  extension (a: NDArray[Double])

    inline def +(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] =
      inline if bc then broadcastShapes(a.shape, b.shape) end if
      if a.isColMajor && b.isColMajor && arrayEquals(a.shape, b.shape) then
        val n   = a.numel
        val out = new Array[Double](n)
        var i   = 0
        while i < n do
          out(i) = a.data(a.offset + i) + b.data(b.offset + i)
          i += 1
        end while
        mkNDArray(out, a.shape.clone(), colMajorStrides(a.shape), 0)
      else
        genericBinaryOp(a, b)(_ + _)
      end if
    end +

    // (-, *, / follow the same while-loop fast path pattern)
    inline def -(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] = ...
    inline def *(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] = ...
    inline def /(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Double] = ...

    inline def +(s: Double): NDArray[Double] = genericUnaryOp(a)(_ + s)
    inline def -(s: Double): NDArray[Double] = genericUnaryOp(a)(_ - s)
    inline def *(s: Double): NDArray[Double] = genericUnaryOp(a)(_ * s)
    inline def /(s: Double): NDArray[Double] = genericUnaryOp(a)(_ / s)

    inline def neg: NDArray[Double]      = genericUnaryOp(a)(-_)
    inline def unary_- : NDArray[Double] = a.neg
    inline def abs: NDArray[Double]      = genericUnaryOp(a)(math.abs)
    inline def exp: NDArray[Double]      = genericUnaryOp(a)(math.exp)
    inline def log: NDArray[Double]      = genericUnaryOp(a)(math.log)
    inline def sqrt: NDArray[Double]     = genericUnaryOp(a)(math.sqrt)
    inline def tanh: NDArray[Double]     = genericUnaryOp(a)(math.tanh)
    inline def sigmoid: NDArray[Double]  = genericUnaryOp(a)(x => 1.0 / (1.0 + math.exp(-x)))

    inline def +=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit =
      inline if bc then broadcastShapes(a.shape, b.shape) end if
      genericInPlaceBinaryOp(a, b)(_ + _)
    end +=

    inline def -=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit = genericInPlaceBinaryOp(a, b)(_ - _)
    inline def *=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit = genericInPlaceBinaryOp(a, b)(_ * _)
    inline def /=(b: NDArray[Double])(using inline bc: BoundsCheck): Unit = genericInPlaceBinaryOp(a, b)(_ / _)

    inline def +=(s: Double): Unit = genericInPlaceBinaryOp(a, NDArray.fill(a.shape, s)(using BoundsCheck.DoBoundsCheck.no))(_ + _)
    inline def -=(s: Double): Unit = a += (-s)
    inline def *=(s: Double): Unit = genericInPlaceBinaryOp(a, NDArray.fill(a.shape, s)(using BoundsCheck.DoBoundsCheck.no))(_ * _)
    inline def /=(s: Double): Unit = a *= (1.0 / s)

    inline def >(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]   = genericCompareOp(a, b)(_ > _)
    inline def <(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]   = genericCompareOp(a, b)(_ < _)
    inline def >=(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]  = genericCompareOp(a, b)(_ >= _)
    inline def <=(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean]  = genericCompareOp(a, b)(_ <= _)
    inline def =:=(b: NDArray[Double])(using inline bc: BoundsCheck): NDArray[Boolean] = genericCompareOp(a, b)(_ == _)

  end extension

end JsNativeNDArrayDoubleOps
```

### 4. `vecxt/src-js/ndarray_elemwise_js.scala` — JS shim

```scala
package vecxt

object NDArrayDoubleOps:
  export vecxt.JsNativeNDArrayDoubleOps.*
end NDArrayDoubleOps
```

### 5. `vecxt/src-native/ndarray_elemwise_native.scala` — Native shim

```scala
package vecxt

object NDArrayDoubleOps:
  export vecxt.JsNativeNDArrayDoubleOps.*
end NDArrayDoubleOps
```

### 6. Update `vecxt/src/all.scala`

In the `// ndarray` section (added by M1/M2):

```scala
  // ndarray
  export vecxt.ndarray.*
  export vecxt.ndarrayOps.*
  export vecxt.NDArrayDoubleOps.*    // new in M3
```

---

## Conventions to follow (IMPORTANT)

- Package: `vecxt` (not a sub-package)
- `inline` on all public extension methods
- `while` loops everywhere — no `for`, `.map`, `.flatMap`, `.zip` in implementations
- `@publicInBinary()` already on `NDArray` constructor — no change needed
- `inline if doCheck then ...` for bounds checking (see `BoundsCheck.scala`)
- Exception type: `InvalidNDArray` (defined in `NDArrayCheck.scala` by M1/M2)
- Use `mkNDArray` for all internal NDArray construction (bypasses bounds check)
- Use `colMajorStrides` and `shapeProduct` from `ndarray.scala`
- Clone shape arrays when building output NDArrays (`a.shape.clone()`)
- Fast path condition for binary ops: `a.isColMajor && b.isColMajor && arrayEquals(a.shape, b.shape)`
- Broadcasting always produces a **new** col-major NDArray
- In-place ops mutate through `a.offset + index * a.strides(d)` arithmetic
- Scalar in-place for non-contiguous: allocate temp via `NDArray.fill` (acceptable in M3)

---

## Validation Tests

File: `vecxt/test/src/ndarray_elemwise.test.scala`

Package: `vecxt`, class `NDArrayElemwiseSuite extends munit.FunSuite`.
Import `all.*` and `BoundsCheck.DoBoundsCheck.yes`.

**Helper** (add to `vecxt/test/src/helpers.forTests.scala`):

```scala
def assertNDEquals(
    a: NDArray[Double],
    b: NDArray[Double],
    tol: Double = 1e-6
)(implicit loc: munit.Location): Unit =
  assertEquals(a.ndim, b.ndim, "ndim mismatch")
  assert(a.shape.sameElements(b.shape), s"shape mismatch: [${a.shape.mkString(",")}] vs [${b.shape.mkString(",")}]")
  val aa = a.toArray
  val bb = b.toArray
  var i  = 0
  while i < aa.length do
    assertEqualsDouble(aa(i), bb(i), tol, s"at flat index $i")
    i += 1
  end while
end assertNDEquals
```

### Full test suite

```scala
package vecxt

import munit.FunSuite
import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayElemwiseSuite extends FunSuite:

  // 1. Binary ops — contiguous same shape (exercises fast path)

  test("NDArray[Double] + contiguous 1D") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val b = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(4))
    assertNDEquals(a + b, NDArray(Array(11.0, 22.0, 33.0, 44.0), Array(4)))
  }

  test("NDArray[Double] - contiguous 1D") {
    val a = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(4))
    val b = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    assertNDEquals(a - b, NDArray(Array(9.0, 18.0, 27.0, 36.0), Array(4)))
  }

  test("NDArray[Double] * contiguous 1D") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val b = NDArray(Array(2.0, 3.0, 4.0, 5.0), Array(4))
    assertNDEquals(a * b, NDArray(Array(2.0, 6.0, 12.0, 20.0), Array(4)))
  }

  test("NDArray[Double] / contiguous 1D") {
    val a = NDArray(Array(6.0, 9.0, 12.0, 20.0), Array(4))
    val b = NDArray(Array(2.0, 3.0, 4.0, 5.0), Array(4))
    assertNDEquals(a / b, NDArray(Array(3.0, 3.0, 3.0, 4.0), Array(4)))
  }

  test("NDArray[Double] + contiguous 2D") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    val b = NDArray(Array(6.0, 5.0, 4.0, 3.0, 2.0, 1.0), Array(2, 3))
    assertNDEquals(a + b, NDArray.fill[Double](Array(2, 3), 7.0))
  }

  test("NDArray[Double] + contiguous 3D") {
    val n = 2 * 3 * 4
    val a = NDArray(Array.tabulate(n)(_.toDouble), Array(2, 3, 4))
    val b = NDArray(Array.fill(n)(1.0), Array(2, 3, 4))
    assertNDEquals(a + b, NDArray(Array.tabulate(n)(i => i.toDouble + 1.0), Array(2, 3, 4)))
  }

  test("result of + is col-major with correct shape") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val c = a + a
    assertEquals(c.shape.toSeq, Seq(2, 2))
    assert(c.isColMajor, "result must be col-major")
  }

  // 2. Scalar ops

  test("NDArray[Double] + scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDEquals(a + 10.0, NDArray(Array(11.0, 12.0, 13.0), Array(3)))
  }

  test("NDArray[Double] - scalar") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    assertNDEquals(a - 5.0, NDArray(Array(5.0, 15.0, 25.0), Array(3)))
  }

  test("NDArray[Double] * scalar") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    assertNDEquals(a * 3.0, NDArray(Array(3.0, 6.0, 9.0), Array(3)))
  }

  test("NDArray[Double] / scalar") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    assertNDEquals(a / 2.0, NDArray(Array(5.0, 10.0, 15.0), Array(3)))
  }

  // 3. Unary ops

  test("NDArray[Double].neg") {
    val a = NDArray(Array(1.0, -2.0, 3.0), Array(3))
    assertNDEquals(a.neg, NDArray(Array(-1.0, 2.0, -3.0), Array(3)))
  }

  test("NDArray[Double] unary_-") {
    val a = NDArray(Array(1.0, -2.0, 3.0), Array(3))
    assertNDEquals(-a, NDArray(Array(-1.0, 2.0, -3.0), Array(3)))
  }

  test("NDArray[Double].abs") {
    val a = NDArray(Array(-1.0, 2.0, -3.0, 0.0), Array(4))
    assertNDEquals(a.abs, NDArray(Array(1.0, 2.0, 3.0, 0.0), Array(4)))
  }

  test("NDArray[Double].exp") {
    val a = NDArray(Array(0.0, 1.0, 2.0), Array(3))
    assertNDEquals(a.exp, NDArray(Array(1.0, math.E, math.exp(2.0)), Array(3)), tol = 1e-10)
  }

  test("NDArray[Double].log") {
    val a = NDArray(Array(1.0, math.E, math.exp(2.0)), Array(3))
    assertNDEquals(a.log, NDArray(Array(0.0, 1.0, 2.0), Array(3)), tol = 1e-10)
  }

  test("NDArray[Double].sqrt") {
    val a = NDArray(Array(0.0, 1.0, 4.0, 9.0, 16.0), Array(5))
    assertNDEquals(a.sqrt, NDArray(Array(0.0, 1.0, 2.0, 3.0, 4.0), Array(5)), tol = 1e-10)
  }

  test("NDArray[Double].tanh") {
    val a = NDArray(Array(0.0, 1.0, -1.0), Array(3))
    assertNDEquals(a.tanh, NDArray(Array(0.0, math.tanh(1.0), math.tanh(-1.0)), Array(3)), tol = 1e-10)
  }

  test("NDArray[Double].sigmoid values") {
    val a    = NDArray(Array(0.0, 10.0, -10.0), Array(3))
    val flat = a.sigmoid.toArray
    assertEqualsDouble(flat(0), 0.5, 1e-10)
    assertEqualsDouble(flat(1), 1.0 / (1.0 + math.exp(-10.0)), 1e-10)
    assertEqualsDouble(flat(2), 1.0 / (1.0 + math.exp(10.0)), 1e-10)
  }

  test("NDArray[Double] log . exp round-trips") {
    val a = NDArray(Array(0.5, 1.0, 2.0, 3.0, 4.0), Array(5))
    assertNDEquals(a.log.exp, a, tol = 1e-10)
    assertNDEquals(a.exp.log, a, tol = 1e-10)
  }

  // 4. In-place binary ops

  test("NDArray[Double] += in place same shape") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val b = NDArray(Array(10.0, 20.0, 30.0, 40.0), Array(4))
    a += b
    assertNDEquals(a, NDArray(Array(11.0, 22.0, 33.0, 44.0), Array(4)))
  }

  test("NDArray[Double] -= in place") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    val b = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    a -= b
    assertNDEquals(a, NDArray(Array(9.0, 18.0, 27.0), Array(3)))
  }

  test("NDArray[Double] *= in place") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(2.0, 3.0, 4.0), Array(3))
    a *= b
    assertNDEquals(a, NDArray(Array(2.0, 6.0, 12.0), Array(3)))
  }

  test("NDArray[Double] /= in place") {
    val a = NDArray(Array(6.0, 9.0, 12.0), Array(3))
    val b = NDArray(Array(2.0, 3.0, 4.0), Array(3))
    a /= b
    assertNDEquals(a, NDArray(Array(3.0, 3.0, 3.0), Array(3)))
  }

  test("NDArray[Double] += scalar in place") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    a += 5.0
    assertNDEquals(a, NDArray(Array(6.0, 7.0, 8.0), Array(3)))
  }

  test("NDArray[Double] -= scalar in place") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    a -= 5.0
    assertNDEquals(a, NDArray(Array(5.0, 15.0, 25.0), Array(3)))
  }

  test("NDArray[Double] *= scalar in place") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    a *= 3.0
    assertNDEquals(a, NDArray(Array(3.0, 6.0, 9.0), Array(3)))
  }

  test("NDArray[Double] /= scalar in place") {
    val a = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    a /= 2.0
    assertNDEquals(a, NDArray(Array(5.0, 10.0, 15.0), Array(3)))
  }

  test("in-place += through strided view mutates backing array selectively") {
    // Create a strided view with stride 2 over a 6-element array
    val backing = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    val arr     = NDArray(backing, Array(6))
    val view    = NDArray(backing, Array(3), Array(2), 0) // selects indices 0, 2, 4
    view += NDArray(Array(1.0, 1.0, 1.0), Array(3))
    // indices 0, 2, 4 should be 1.0; indices 1, 3, 5 unchanged (0.0)
    assertEqualsDouble(backing(0), 1.0, 1e-10)
    assertEqualsDouble(backing(1), 0.0, 1e-10)
    assertEqualsDouble(backing(2), 1.0, 1e-10)
    assertEqualsDouble(backing(3), 0.0, 1e-10)
    assertEqualsDouble(backing(4), 1.0, 1e-10)
    assertEqualsDouble(backing(5), 0.0, 1e-10)
  }

  // 5. Comparison ops

  test("NDArray[Double] > NDArray") {
    val a = NDArray(Array(1.0, 5.0, 3.0, 4.0), Array(4))
    val b = NDArray(Array(2.0, 3.0, 3.0, 5.0), Array(4))
    assertEquals((a > b).toArray.toSeq, Seq(false, true, false, false))
  }

  test("NDArray[Double] < NDArray") {
    val a = NDArray(Array(1.0, 5.0, 3.0, 4.0), Array(4))
    val b = NDArray(Array(2.0, 3.0, 3.0, 5.0), Array(4))
    assertEquals((a < b).toArray.toSeq, Seq(true, false, false, true))
  }

  test("NDArray[Double] >= NDArray") {
    val a = NDArray(Array(3.0, 5.0, 3.0), Array(3))
    val b = NDArray(Array(3.0, 3.0, 5.0), Array(3))
    assertEquals((a >= b).toArray.toSeq, Seq(true, true, false))
  }

  test("NDArray[Double] <= NDArray") {
    val a = NDArray(Array(3.0, 5.0, 3.0), Array(3))
    val b = NDArray(Array(3.0, 3.0, 5.0), Array(3))
    assertEquals((a <= b).toArray.toSeq, Seq(true, false, true))
  }

  test("NDArray[Double] =:= NDArray") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    val b = NDArray(Array(1.0, 0.0, 3.0, 0.0), Array(4))
    assertEquals((a =:= b).toArray.toSeq, Seq(true, false, true, false))
  }

  test("comparison result has correct shape") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    val b = NDArray(Array(6.0, 5.0, 4.0, 3.0, 2.0, 1.0), Array(2, 3))
    val c = a > b
    assertEquals(c.shape.toSeq, Seq(2, 3))
    assert(c.isColMajor, "comparison result should be col-major")
  }

  // 6. Broadcasting

  test("broadcasting: shape[1] + shape[5] -> shape[5]") {
    val a = NDArray(Array(100.0), Array(1))
    val b = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0), Array(5))
    assertNDEquals(a + b, NDArray(Array(101.0, 102.0, 103.0, 104.0, 105.0), Array(5)))
  }

  test("broadcasting: col-vector (3,1) + row-1D (3) -> (3,3)") {
    // a.shape=[3,1], b.shape=[3] -> right-aligned -> [3,1] vs [1,3] -> broadcast [3,3]
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3, 1))
    val b = NDArray(Array(10.0, 20.0, 30.0), Array(3))
    val c = a + b
    assertEquals(c.shape.toSeq, Seq(3, 3))
    val flat = c.toArray  // col-major: col changes second
    // c(i,j) = a(i,0) + b(j) = (i+1) + (j+1)*10
    // flat col-major: c(0,0)=11, c(1,0)=12, c(2,0)=13, c(0,1)=21, c(1,1)=22, ...
    assertEqualsDouble(flat(0), 11.0, 1e-10)
    assertEqualsDouble(flat(1), 12.0, 1e-10)
    assertEqualsDouble(flat(2), 13.0, 1e-10)
    assertEqualsDouble(flat(3), 21.0, 1e-10)
    assertEqualsDouble(flat(4), 22.0, 1e-10)
    assertEqualsDouble(flat(5), 23.0, 1e-10)
    assertEqualsDouble(flat(6), 31.0, 1e-10)
    assertEqualsDouble(flat(7), 32.0, 1e-10)
    assertEqualsDouble(flat(8), 33.0, 1e-10)
  }

  test("broadcasting: (1,4) + (3,4) -> (3,4)") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(1, 4))
    val b = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
    val c = a + b
    assertEquals(c.shape.toSeq, Seq(3, 4))
    val flat = c.toArray
    // col-major: b(i,j) = j*3 + i; a(0,j) = j+1
    // c(i,j) = (j+1) + (j*3 + i)
    var i = 0
    while i < 3 do
      var j = 0
      while j < 4 do
        assertEqualsDouble(flat(j * 3 + i), (j + 1).toDouble + (j * 3 + i).toDouble, 1e-10, s"($i,$j)")
        j += 1
      end while
      i += 1
    end while
  }

  test("broadcasting: incompatible shapes throws InvalidNDArray") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(1.0, 2.0), Array(2))
    intercept[InvalidNDArray] { a + b }
  }

  test("broadcasting: (4,1) + (1,1) -> (4,1)") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4, 1))
    val b = NDArray(Array(10.0), Array(1, 1))
    assertNDEquals(a + b, NDArray(Array(11.0, 12.0, 13.0, 14.0), Array(4, 1)))
  }

  test("broadcasting comparison: (3,1) > (3) -> (3,3)") {
    val a = NDArray(Array(2.0, 5.0, 1.0), Array(3, 1))
    val b = NDArray(Array(1.0, 3.0, 4.0), Array(3))
    val c = a > b
    assertEquals(c.shape.toSeq, Seq(3, 3))
  }

  // 7. Non-contiguous views

  test("NDArray[Double] + with stride-2 view") {
    val backing = Array(0.0, 99.0, 2.0, 99.0, 4.0, 99.0)
    val view    = NDArray(backing, Array(3), Array(2), 0)
    val b       = NDArray(Array(10.0, 10.0, 10.0), Array(3))
    assertNDEquals(view + b, NDArray(Array(10.0, 12.0, 14.0), Array(3)))
  }

  test("NDArray[Double] * transposed 2D view") {
    val a  = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    val at = a.T   // shape [3,2], strides [2,1] — not col-major
    val b  = NDArray(Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0), Array(3, 2))
    val c  = at * b
    assertEquals(c.shape.toSeq, Seq(3, 2))
    assert(c.isColMajor, "result must be col-major")
    val flat = c.toArray
    // at(0,0)=1, at(1,0)=3, at(2,0)=5, at(0,1)=2, at(1,1)=4, at(2,1)=6
    assertEqualsDouble(flat(0), 1.0, 1e-10)
    assertEqualsDouble(flat(1), 3.0, 1e-10)
    assertEqualsDouble(flat(2), 5.0, 1e-10)
    assertEqualsDouble(flat(3), 2.0, 1e-10)
    assertEqualsDouble(flat(4), 4.0, 1e-10)
    assertEqualsDouble(flat(5), 6.0, 1e-10)
  }

  test("unary op on non-contiguous view") {
    val backing = Array(1.0, 99.0, 4.0, 99.0, 9.0)
    val view    = NDArray(backing, Array(3), Array(2), 0)  // reads indices 0, 2, 4 -> 1, 4, 9
    assertNDEquals(view.sqrt, NDArray(Array(1.0, 2.0, 3.0), Array(3)), tol = 1e-10)
  }

  // 8. Edge cases and correctness checks

  test("1-element NDArray ops") {
    val a = NDArray(Array(5.0), Array(1))
    val b = NDArray(Array(3.0), Array(1))
    assertNDEquals(a + b, NDArray(Array(8.0), Array(1)))
    assertNDEquals(a * b, NDArray(Array(15.0), Array(1)))
    assertNDEquals(a / b, NDArray(Array(5.0 / 3.0), Array(1)), tol = 1e-10)
  }

  test("NDArray ops with zeros array do not mutate inputs") {
    val a_data = Array(1.0, 2.0, 3.0)
    val a      = NDArray(a_data, Array(3))
    val b      = NDArray(Array(4.0, 5.0, 6.0), Array(3))
    val _      = a + b
    assertEquals(a_data.toSeq, Seq(1.0, 2.0, 3.0))
  }

  test("sigmoid output in [0,1] for extreme values") {
    val a    = NDArray(Array(-100.0, -1.0, 0.0, 1.0, 100.0), Array(5))
    val flat = a.sigmoid.toArray
    var i    = 0
    while i < flat.length do
      assert(flat(i) >= 0.0 && flat(i) <= 1.0, s"sigmoid(${a.data(i)}) = ${flat(i)} not in [0,1]")
      i += 1
    end while
  }

  test("sqrt of squared values == abs") {
    val data = Array(-3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0)
    val a    = NDArray(data, Array(7))
    assertNDEquals((a * a).sqrt, a.abs, tol = 1e-10)
  }

  // 9. Large array — exercises SIMD loopBound tail handling

  test("+ large array (size not a multiple of SIMD width)") {
    val n    = 33
    val data = Array.tabulate(n)(_.toDouble)
    val a    = NDArray(data, Array(n))
    val b    = NDArray(Array.fill(n)(1.0), Array(n))
    val flat = (a + b).toArray
    var i    = 0
    while i < n do
      assertEqualsDouble(flat(i), i.toDouble + 1.0, 1e-10, s"at $i")
      i += 1
    end while
  }

  test("unary op large array tail handling") {
    val n    = 33
    val a    = NDArray(Array.tabulate[Double](n)(i => i.toDouble + 1.0), Array(n))
    assertNDEquals(a.log.exp, a, tol = 1e-9)
  }

end NDArrayElemwiseSuite
```

---

## Build & validate

```bash
# Compile all platforms
./mill vecxt.__.compile

# Run all tests (JVM + JS + Native)
./mill vecxt.__.test

# Format before pushing
./mill mill.scalalib.scalafmt.ScalafmtModule/
```

All tests must pass on JVM, JS, and Native. The generic implementations in `vecxt/src/` are
identical across platforms; only the JVM has SIMD fast paths. Numerical results must be
identical within the specified tolerances.

---

## Dependency on M1/M2

| Symbol | Source file |
|--------|-------------|
| `NDArray[A]` | `vecxt/src/ndarray.scala` |
| `mkNDArray` | `vecxt/src/ndarray.scala` |
| `colMajorStrides` | `vecxt/src/ndarray.scala` |
| `shapeProduct` | `vecxt/src/ndarray.scala` |
| `InvalidNDArray` | `vecxt/src/NDArrayCheck.scala` |
| `NDArray.fill` | `vecxt/src/ndarray.scala` |
| `.T`, `.slice`, `.toArray` | `vecxt/src/ndarrayOps.scala` |
| `isColMajor`, `isContiguous` | `vecxt/src/ndarray.scala` |

All are `private[vecxt]` or package-level, so directly accessible from `package vecxt`.

---

## What NOT to do

- Do NOT modify `matrix.scala`, `arrays.scala`, or any existing file
- Do NOT add reduction ops (`sum`, `mean`, `norm`) — that is M4
- Do NOT add `NDArray[Int]` or `NDArray[Float]` ops — that is M6
- Do NOT use `for` comprehensions, `.map`, `.flatMap`, `.zip` in implementations
- Do NOT copy data unless producing a new output (views share `data`)
- Do NOT assume arrays are contiguous in the generic path
- Do NOT skip broadcasting shape validation when `BoundsCheck` is on
- Do NOT add new dependencies to `package.mill`
