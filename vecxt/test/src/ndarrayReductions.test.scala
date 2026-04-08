package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayReductionsSuite extends FunSuite:

  // ── Full reductions ────────────────────────────────────────────────────────

  test("sum 1-D") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    assertClose(arr.sum, 10.0)
  }

  test("sum 2-D col-major") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    assertClose(arr.sum, 10.0)
  }

  test("sum 3-D") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), Array(2, 2, 2))
    assertClose(arr.sum, 36.0)
  }

  test("sum strided (transposed) exercises general path") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val t = arr.T
    assert(!t.isColMajor)
    assertClose(t.sum, 10.0)
  }

  test("mean 1-D") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    assertClose(arr.mean, 2.5)
  }

  test("mean strided") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val t = arr.T
    assert(!t.isColMajor)
    assertClose(t.mean, 2.5)
  }

  test("min 1-D") {
    val arr = NDArray(Array(3.0, 1.0, 4.0, 1.0, 5.0), Array(5))
    assertClose(arr.min, 1.0)
  }

  test("min with negative values") {
    val arr = NDArray(Array(-1.0, -5.0, 2.0), Array(3))
    assertClose(arr.min, -5.0)
  }

  test("max 1-D") {
    val arr = NDArray(Array(3.0, 1.0, 4.0, 1.0, 5.0), Array(5))
    assertClose(arr.max, 5.0)
  }

  test("product 1-D") {
    val arr = NDArray(Array(2.0, 3.0, 4.0), Array(3))
    assertClose(arr.product, 24.0)
  }

  test("product with zero") {
    val arr = NDArray(Array(2.0, 0.0, 4.0), Array(3))
    assertClose(arr.product, 0.0)
  }

  test("variance 1-D — population variance") {
    // Population variance of [2,4,4,4,5,5,7,9] = 4.0
    val arr = NDArray(Array(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0), Array(8))
    assertClose(arr.variance, 4.0)
  }

  test("norm 1-D — 3-4-5 triangle") {
    val arr = NDArray(Array(3.0, 4.0), Array(2))
    assertClose(arr.norm, 5.0)
  }

  test("norm 2-D identity-like — Frobenius norm") {
    // [[1,0],[0,1]] stored col-major: data=[1,0,0,1]
    val arr = NDArray(Array(1.0, 0.0, 0.0, 1.0), Array(2, 2))
    assertClose(arr.norm, Math.sqrt(2.0))
  }

  test("argmax 1-D") {
    val arr = NDArray(Array(1.0, 5.0, 3.0, 2.0), Array(4))
    assertEquals(arr.argmax, 1)
  }

  test("argmin 1-D") {
    val arr = NDArray(Array(4.0, 2.0, 7.0, 1.0), Array(4))
    assertEquals(arr.argmin, 3)
  }

  test("argmax 2-D col-major (flat index)") {
    // data [1,4,3,2] shape [2,2] col-major: (0,0)=1,(1,0)=4,(0,1)=3,(1,1)=2 → max at flat index 1
    val arr = NDArray(Array(1.0, 4.0, 3.0, 2.0), Array(2, 2))
    assertEquals(arr.argmax, 1)
  }

  test("sum strided general path gives same result as col-major") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertClose(a.sum, a.T.sum)
  }

  test("min/max strided exercises general path") {
    val arr = NDArray(Array(3.0, 1.0, 4.0, 1.0, 5.0, 9.0), Array(2, 3))
    val t = arr.T
    assert(!t.isColMajor)
    assertClose(t.min, 1.0)
    assertClose(t.max, 9.0)
  }

  test("product strided exercises general path") {
    val arr = NDArray(Array(2.0, 3.0, 4.0, 5.0), Array(2, 2))
    val t = arr.T
    assert(!t.isColMajor)
    assertClose(t.product, 120.0)
  }

  test("argmax/argmin strided exercises general path") {
    val arr = NDArray(Array(1.0, 5.0, 3.0, 2.0), Array(2, 2))
    val t = arr.T
    assert(!t.isColMajor)
    // Col-major traversal of t (shape [2,2]): (0,0)=1, (1,0)=3, (0,1)=5, (1,1)=2 → argmax=2, argmin=0
    assertEquals(t.argmax, 2)
    assertEquals(t.argmin, 0)
  }

  test("sum of empty 1-D (shape [0])") {
    val arr = NDArray(Array.empty[Double], Array(1, 0), Array(1, 1), 0)(using BoundsCheck.DoBoundsCheck.no)
    // numel = 0, reduce loop doesn't execute → initial value 0.0
    assertClose(arr.sum, 0.0)
  }

  test("product of empty 1-D gives identity 1.0") {
    val arr = NDArray(Array.empty[Double], Array(1, 0), Array(1, 1), 0)(using BoundsCheck.DoBoundsCheck.no)
    assertClose(arr.product, 1.0)
  }

  // ── Axis reductions — 2-D ─────────────────────────────────────────────────

  test("sum(0) on [2,3]") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.sum(0), Array(3), Array(3.0, 7.0, 11.0))
  }

  test("sum(1) on [2,3]") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.sum(1), Array(2), Array(9.0, 12.0))
  }

  test("max(0) on [2,3]") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.max(0), Array(3), Array(2.0, 4.0, 6.0))
  }

  test("min(1) on [2,3]") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.min(1), Array(2), Array(1.0, 2.0))
  }

  test("mean(0) on [2,3]") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.mean(0), Array(3), Array(1.5, 3.5, 5.5))
  }

  test("product(0) on [2,3]") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.product(0), Array(3), Array(2.0, 12.0, 30.0))
  }

  // ── Axis reductions — 3-D (worked examples from design doc) ──────────────

  test("sum(0) on [2,3,2] 3-D") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0)
    val arr = NDArray(data, Array(2, 3, 2))
    // Expected shape [3,2], data [3,7,11,15,19,23]
    assertNDArrayShapeAndClose(arr.sum(0), Array(3, 2), Array(3.0, 7.0, 11.0, 15.0, 19.0, 23.0))
  }

  test("sum(1) on [2,3,2] 3-D") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0)
    val arr = NDArray(data, Array(2, 3, 2))
    // Expected shape [2,2], data [9,12,27,30]
    assertNDArrayShapeAndClose(arr.sum(1), Array(2, 2), Array(9.0, 12.0, 27.0, 30.0))
  }

  test("sum(2) on [2,3,2] 3-D") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0)
    val arr = NDArray(data, Array(2, 3, 2))
    // Expected shape [2,3], data [8,10,12,14,16,18]
    assertNDArrayShapeAndClose(arr.sum(2), Array(2, 3), Array(8.0, 10.0, 12.0, 14.0, 16.0, 18.0))
  }

  // ── Axis arg-reductions ───────────────────────────────────────────────────

  test("argmax(0) on [2,3]") {
    // data [1,4,3,2,5,6] col-major [2,3]: (0,0)=1,(1,0)=4,(0,1)=3,(1,1)=2,(0,2)=5,(1,2)=6
    // max along axis 0: col0→max at row1 (idx=1), col1→max at row0 (idx=0), col2→max at row1 (idx=1)
    val arr = NDArray(Array(1.0, 4.0, 3.0, 2.0, 5.0, 6.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.argmax(0), Array(3), Array(1.0, 0.0, 1.0))
  }

  test("argmin(1) on [2,3]") {
    // data [5,6,1,2,3,4] col-major [2,3]: (0,0)=5,(1,0)=6,(0,1)=1,(1,1)=2,(0,2)=3,(1,2)=4
    // min along axis 1: row0→min at col1 (idx=1), row1→min at col1 (idx=1)
    val arr = NDArray(Array(5.0, 6.0, 1.0, 2.0, 3.0, 4.0), Array(2, 3))
    assertNDArrayShapeAndClose(arr.argmin(1), Array(2), Array(1.0, 1.0))
  }

  test("axis reduction on transposed (general strided path)") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    val t = arr.T // shape [3,2], not col-major
    assert(!t.isColMajor)
    // sum(0) on transposed [3,2]: collapse rows (size 3) → shape [2]
    // t(row,col): t(0,0)=arr(0,0)=1, t(1,0)=arr(0,1)=3, t(2,0)=arr(0,2)=5 → sum=9
    //             t(0,1)=arr(1,0)=2, t(1,1)=arr(1,1)=4, t(2,1)=arr(1,2)=6 → sum=12
    assertNDArrayShapeAndClose(t.sum(0), Array(2), Array(9.0, 12.0))
  }

  // ── Axis validation ────────────────────────────────────────────────────────

  test("sum(-1) throws InvalidNDArray") {
    val arr = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    intercept[InvalidNDArray] {
      arr.sum(-1)
    }
  }

  test("sum(ndim) throws InvalidNDArray") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    intercept[InvalidNDArray] {
      arr.sum(2)
    }
  }

  // ── dot ───────────────────────────────────────────────────────────────────

  test("dot 1-D") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(4.0, 5.0, 6.0), Array(3))
    assertClose(a.dot(b), 32.0)
  }

  test("dot 1-D orthogonal vectors") {
    val a = NDArray(Array(1.0, 0.0, 0.0), Array(3))
    val b = NDArray(Array(0.0, 0.0, 1.0), Array(3))
    assertClose(a.dot(b), 0.0)
  }

  test("dot strided (slice view) exercises general path") {
    // slice a 1-D view from a 2-D array
    val backing = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(6))
    // stride-2 view: elements 0,2,4 → [1,3,5]
    val strided = mkNDArray(backing.data, Array(3), Array(2), 0)
    val b = NDArray(Array(1.0, 1.0, 1.0), Array(3))
    assertClose(strided.dot(b), 9.0) // 1+3+5=9
  }

  test("dot rank mismatch throws InvalidNDArray") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val b = NDArray(Array(1.0, 2.0), Array(2))
    intercept[InvalidNDArray] {
      a.dot(b)
    }
  }

  test("dot length mismatch throws ShapeMismatchException") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(4))
    intercept[ShapeMismatchException] {
      a.dot(b)
    }
  }

  // ── matmul ────────────────────────────────────────────────────────────────

  test("matmul [2,3] @@ [3,2]") {
    // a col-major [2,3]: (r,c) → a(0,0)=1,a(1,0)=2,a(0,1)=3,a(1,1)=4,a(0,2)=5,a(1,2)=6
    // b col-major [3,2]: (r,c) → b(0,0)=7,b(1,0)=8,b(2,0)=9,b(0,1)=10,b(1,1)=11,b(2,1)=12
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    val b = NDArray(Array(7.0, 8.0, 9.0, 10.0, 11.0, 12.0), Array(3, 2))
    // result[i,j] = sum_k a[i,k]*b[k,j]
    // result[0,0] = 1*7+3*8+5*9 = 7+24+45 = 76
    // result[1,0] = 2*7+4*8+6*9 = 14+32+54 = 100
    // result[0,1] = 1*10+3*11+5*12 = 10+33+60 = 103
    // result[1,1] = 2*10+4*11+6*12 = 20+44+72 = 136
    val result = a.matmul(b)
    assertEquals(result.shape.toSeq, Seq(2, 2))
    assertNDArrayClose(result, Array(76.0, 100.0, 103.0, 136.0))
  }

  test("matmul @@ alias") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    val b = NDArray(Array(7.0, 8.0, 9.0, 10.0, 11.0, 12.0), Array(3, 2))
    val r1 = a.matmul(b)
    val r2 = a @@ b
    assertNDArrayClose(r2, r1.toArray)
  }

  test("matmul result shape [4,3] @@ [3,5] = [4,5]") {
    val a = NDArray(Array.fill(12)(1.0), Array(4, 3))
    val b = NDArray(Array.fill(15)(1.0), Array(3, 5))
    val result = a @@ b
    assertEquals(result.shape.toSeq, Seq(4, 5))
  }

  test("matmul with non-contiguous (transposed) input") {
    val a = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val b = NDArray(Array(1.0, 0.0, 0.0, 1.0), Array(2, 2)) // identity
    val at = a.T
    assert(!at.isColMajor)
    // a.T @@ identity = a.T materialised
    val result = at @@ b
    assertNDArrayClose(result, at.toArray)
  }

  test("matmul inner dim mismatch throws ShapeMismatchException") {
    val a = NDArray(Array.fill(6)(1.0), Array(2, 3))
    val b = NDArray(Array.fill(6)(1.0), Array(2, 3))
    intercept[ShapeMismatchException] {
      a @@ b
    }
  }

  test("matmul rank mismatch throws InvalidNDArray") {
    val a = NDArray(Array(1.0, 2.0, 3.0), Array(3))
    val b = NDArray(Array.fill(6)(1.0), Array(2, 3))
    intercept[InvalidNDArray] {
      a @@ b
    }
  }

  test("matmul consistency with Matrix @@") {
    import vecxt.matrix.*
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    val a = NDArray(data, Array(3, 3))
    val b = NDArray(data, Array(3, 3))
    val ndResult = a @@ b
    val matA = Matrix[Double](data, 3, 3)(using BoundsCheck.DoBoundsCheck.no)
    val matB = Matrix[Double](data, 3, 3)(using BoundsCheck.DoBoundsCheck.no)
    val matResult = matA @@ matB
    assertEquals(ndResult.shape.toSeq, Seq(3, 3))
    assertNDArrayClose(ndResult, matResult.raw)
  }

end NDArrayReductionsSuite
