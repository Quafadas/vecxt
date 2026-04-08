package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArraySlicingSuite extends FunSuite:

  import ndarrayOps.*

  // ── 1-D slicing ──────────────────────────────────────────────────────────

  test("1D :: keeps all elements (zero-copy)") {
    val data = Array.tabulate(10)(_.toDouble)
    val arr = NDArray.fromArray(data)
    val result = arr(::)
    assertEquals(result.shape.toSeq, Seq(10))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
  }

  test("1D Range slice (zero-copy view)") {
    val data = Array.tabulate(10)(_.toDouble)
    val arr = NDArray.fromArray(data)
    val result = arr(2 until 5)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
    assertEquals(result.offset, 2)
  }

  test("1D Range slice values correct") {
    val data = Array.tabulate(10)(_.toDouble)
    val arr = NDArray.fromArray(data)
    val result = arr(2 until 5)
    assertEquals(result(0), 2.0)
    assertEquals(result(1), 3.0)
    assertEquals(result(2), 4.0)
  }

  test("1D gather via varargs on 2D: Array(0, 2) selects rows") {
    val data = Array.tabulate(9)(_.toDouble)
    val arr = NDArray(data, Array(3, 3))
    val result = arr(Array(0, 2), ::)
    assertEquals(result.shape.toSeq, Seq(2, 3))
    assertEquals(result(0, 0), arr(0, 0))
    assertEquals(result(1, 0), arr(2, 0))
  }

  // ── 2-D slicing ──────────────────────────────────────────────────────────

  test("2D (::, ::) identity — zero-copy") {
    val data = Array.tabulate(6)(_.toDouble)
    val arr = NDArray(data, Array(2, 3))
    val result = arr(::, ::)
    assertEquals(result.shape.toSeq, Seq(2, 3))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
    assertEquals(result.offset, arr.offset)
  }

  test("2D (0 until 2, ::) row slice — zero-copy") {
    val data = Array.tabulate(12)(_.toDouble)
    val arr = NDArray(data, Array(4, 3))
    val result = arr(0 until 2, ::)
    assertEquals(result.shape.toSeq, Seq(2, 3))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
    assertEquals(result.offset, 0)
  }

  test("2D (::, 1 until 3) column slice — zero-copy") {
    val data = Array.tabulate(12)(_.toDouble)
    val arr = NDArray(data, Array(4, 3))
    val result = arr(::, 1 until 3)
    assertEquals(result.shape.toSeq, Seq(4, 2))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
    // offset = 1 * stride(1) = 1 * 4 = 4
    assertEquals(result.offset, 4)
  }

  test("2D (1 until 3, 0 until 2) both sliced — zero-copy") {
    val data = Array.tabulate(12)(_.toDouble)
    val arr = NDArray(data, Array(4, 3))
    val result = arr(1 until 3, 0 until 2)
    assertEquals(result.shape.toSeq, Seq(2, 2))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
    // offset = 1 * stride(0) + 0 * stride(1) = 1 * 1 + 0 * 4 = 1
    assertEquals(result.offset, 1)
  }

  test("2D (Array(0, 2), ::) gather rows — copy") {
    val data = Array.tabulate(12)(_.toDouble)
    val arr = NDArray(data, Array(4, 3))
    val result = arr(Array(0, 2), ::)
    assertEquals(result.shape.toSeq, Seq(2, 3))
    // col-major: shape [2,3], result(i,j) == arr(Array(0,2)(i), j)
    assertEquals(result(0, 0), arr(0, 0))
    assertEquals(result(1, 0), arr(2, 0))
    assertEquals(result(0, 1), arr(0, 1))
    assertEquals(result(1, 1), arr(2, 1))
    assertEquals(result(0, 2), arr(0, 2))
    assertEquals(result(1, 2), arr(2, 2))
  }

  test("2D (::, Array(0, 2)) gather columns — copy") {
    val data = Array.tabulate(12)(_.toDouble)
    val arr = NDArray(data, Array(4, 3))
    val result = arr(::, Array(0, 2))
    assertEquals(result.shape.toSeq, Seq(4, 2))
    // result(i,j) == arr(i, Array(0,2)(j))
    var i = 0
    while i < 4 do
      assertEquals(result(i, 0), arr(i, 0))
      assertEquals(result(i, 1), arr(i, 2))
      i += 1
    end while
  }

  // ── 3-D slicing ──────────────────────────────────────────────────────────

  test("3D (::, ::, ::) identity — zero-copy") {
    val data = Array.tabulate(24)(_.toDouble)
    val arr = NDArray(data, Array(2, 3, 4))
    val result = arr(::, ::, ::)
    assertEquals(result.shape.toSeq, Seq(2, 3, 4))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
  }

  test("3D (::, 1 until 3, ::) middle-axis slice — zero-copy") {
    val data = Array.tabulate(30)(_.toDouble)
    val arr = NDArray(data, Array(2, 5, 3))
    // strides = [1, 2, 10]
    val result = arr(::, 1 until 3, ::)
    assertEquals(result.shape.toSeq, Seq(2, 2, 3))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
    // offset = 0 + 1 * strides(1) + 0 = 1 * 2 = 2
    assertEquals(result.offset, 2)
  }

  test("3D (::, ::, Array(0, 2)) last-axis gather — copy") {
    val data = Array.tabulate(30)(_.toDouble)
    val arr = NDArray(data, Array(2, 5, 3))
    val result = arr(::, ::, Array(0, 2))
    assertEquals(result.shape.toSeq, Seq(2, 5, 2))
    // result(i,j,k) == arr(i,j, Array(0,2)(k))
    var i = 0
    while i < 2 do
      var j = 0
      while j < 5 do
        assertEquals(result(i, j, 0), arr(i, j, 0))
        assertEquals(result(i, j, 1), arr(i, j, 2))
        j += 1
      end while
      i += 1
    end while
  }

  test("3D (0 until 2, ::, ::) first-axis slice — zero-copy") {
    val data = Array.tabulate(60)(_.toDouble)
    val arr = NDArray(data, Array(4, 5, 3))
    val result = arr(0 until 2, ::, ::)
    assertEquals(result.shape.toSeq, Seq(2, 5, 3))
    assertEquals(result.strides.toSeq, arr.strides.toSeq)
    assert(result.data eq arr.data)
    assertEquals(result.offset, 0)
  }

  // ── Zero-copy mutation check ──────────────────────────────────────────────

  test("zero-copy: mutating view mutates original") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val arr = NDArray(data, Array(2, 3))
    val view = arr(::, 1 until 2)
    // view.data eq arr.data, so mutation is visible
    assert(view.data eq arr.data)
    view.update(0, 0, 99.0)
    assertEquals(arr(0, 1), 99.0)
  }

  // ── Consistency with slice ────────────────────────────────────────────────

  test("(::, 1 until 3) matches slice(1, 1, 3)") {
    val data = Array.tabulate(20)(_.toDouble)
    val arr = NDArray(data, Array(4, 5))
    val viaVarargs = arr(::, 1 until 3)
    val viaSlice = arr.slice(1, 1, 3)
    assertEquals(viaVarargs.shape.toSeq, viaSlice.shape.toSeq)
    assertEquals(viaVarargs.offset, viaSlice.offset)
    assertEquals(viaVarargs.strides.toSeq, viaSlice.strides.toSeq)
    assert(viaVarargs.data eq viaSlice.data)
  }

  test("(1 until 3, ::) matches slice(0, 1, 3)") {
    val data = Array.tabulate(20)(_.toDouble)
    val arr = NDArray(data, Array(4, 5))
    val viaVarargs = arr(1 until 3, ::)
    val viaSlice = arr.slice(0, 1, 3)
    assertEquals(viaVarargs.shape.toSeq, viaSlice.shape.toSeq)
    assertEquals(viaVarargs.offset, viaSlice.offset)
    assertEquals(viaVarargs.strides.toSeq, viaSlice.strides.toSeq)
    assert(viaVarargs.data eq viaSlice.data)
  }

  // ── Contiguous Array[Int] — zero-copy ─────────────────────────────────────

  test("contiguous Array[Int] selector gives zero-copy view") {
    val data = Array.tabulate(12)(_.toDouble)
    val arr = NDArray(data, Array(4, 3))
    // Array(1, 2) is contiguous: same as Range 1 until 3
    val result = arr(Array(1, 2), ::)
    assertEquals(result.shape.toSeq, Seq(2, 3))
    assert(result.data eq arr.data)
    assertEquals(result.offset, 1) // start = 1, stride(0) = 1
  }

  // ── Error cases ───────────────────────────────────────────────────────────

  test("wrong number of selectors throws InvalidNDArray") {
    val arr = NDArray(Array.tabulate(24)(_.toDouble), Array(2, 3, 4))
    intercept[InvalidNDArray] {
      arr(::, ::) // 2 selectors for ndim=3
    }
  }

  test("Range out of bounds throws IndexOutOfBoundsException") {
    val arr = NDArray.fromArray(Array.tabulate(5)(_.toDouble))
    intercept[java.lang.IndexOutOfBoundsException] {
      arr(0 until 10) // 10 > shape(0)=5
    }
  }

  test("Array[Int] index out of bounds throws IndexOutOfBoundsException") {
    val arr = NDArray(Array.tabulate(9)(_.toDouble), Array(3, 3))
    intercept[java.lang.IndexOutOfBoundsException] {
      arr(Array(0, 99), ::) // 99 >= shape(0)=3
    }
  }

  test("zero selectors on 0-dim array throws InvalidNDArray") {
    // NDArray construction validates shape, can't create 0-dim, so just test wrong count
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))
    intercept[InvalidNDArray] {
      arr(::, ::) // 2 selectors for ndim=1
    }
  }

end NDArraySlicingSuite
