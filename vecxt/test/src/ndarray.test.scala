package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArraySuite extends FunSuite:

  // ---- Construction tests ----

  test("NDArray from data + shape (1D)") {
    val data = Array(1.0, 2.0, 3.0)
    val arr = NDArray(data, Array(3))
    assertEquals(arr.ndim, 1)
    assertEquals(arr.numel, 3)
    assertEquals(arr.shape.toSeq, Seq(3))
    assertEquals(arr.strides.toSeq, Seq(1))
  }

  test("NDArray from data + shape (2D)") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val arr = NDArray(data, Array(2, 3))
    assertEquals(arr.ndim, 2)
    assertEquals(arr.numel, 6)
    assertEquals(arr.shape.toSeq, Seq(2, 3))
    // column-major strides: [1, 2]
    assertEquals(arr.strides.toSeq, Seq(1, 2))
  }

  test("NDArray from data + shape (3D)") {
    val data = Array.fill(24)(0.0)
    val arr = NDArray(data, Array(2, 3, 4))
    assertEquals(arr.ndim, 3)
    assertEquals(arr.numel, 24)
    // column-major strides: [1, 2, 6]
    assertEquals(arr.strides.toSeq, Seq(1, 2, 6))
  }

  test("NDArray from data + shape (4D)") {
    val data = Array.fill(120)(0.0)
    val arr = NDArray(data, Array(2, 3, 4, 5))
    assertEquals(arr.ndim, 4)
    assertEquals(arr.numel, 120)
    // column-major strides: [1, 2, 6, 24]
    assertEquals(arr.strides.toSeq, Seq(1, 2, 6, 24))
  }

  test("NDArray fromArray") {
    val data = Array(10.0, 20.0, 30.0)
    val arr = NDArray.fromArray(data)
    assertEquals(arr.ndim, 1)
    assertEquals(arr.numel, 3)
    assertEquals(arr.shape.toSeq, Seq(3))
    assertEquals(arr.strides.toSeq, Seq(1))
  }

  test("NDArray with explicit strides") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val arr = NDArray(data, Array(2, 3), Array(1, 2), 0)
    assertEquals(arr.ndim, 2)
    assertEquals(arr.numel, 6)
    assertEquals(arr.shape.toSeq, Seq(2, 3))
    assertEquals(arr.strides.toSeq, Seq(1, 2))
    assertEquals(arr.offset, 0)
  }

  test("NDArray with offset") {
    val data = Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val arr = NDArray(data, Array(2, 3), Array(1, 2), 1)
    assertEquals(arr.offset, 1)
    assertEquals(arr.ndim, 2)
    assertEquals(arr.numel, 6)
  }

  // ---- Factory tests ----

  test("NDArray.zeros Double") {
    val arr = NDArray.zeros[Double](Array(2, 3))
    assertEquals(arr.numel, 6)
    var i = 0
    while i < arr.data.length do
      assertEquals(arr.data(i), 0.0)
      i += 1
    end while
  }

  test("NDArray.zeros Int") {
    val arr = NDArray.zeros[Int](Array(3, 4))
    assertEquals(arr.numel, 12)
    var i = 0
    while i < arr.data.length do
      assertEquals(arr.data(i), 0)
      i += 1
    end while
  }

  test("NDArray.zeros Boolean") {
    val arr = NDArray.zeros[Boolean](Array(2, 2))
    assertEquals(arr.numel, 4)
    var i = 0
    while i < arr.data.length do
      assertEquals(arr.data(i), false)
      i += 1
    end while
  }

  test("NDArray.ones Double") {
    val arr = NDArray.ones[Double](Array(2, 3))
    assertEquals(arr.numel, 6)
    var i = 0
    while i < arr.data.length do
      assertEquals(arr.data(i), 1.0)
      i += 1
    end while
  }

  test("NDArray.ones Boolean") {
    val arr = NDArray.ones[Boolean](Array(3))
    assertEquals(arr.numel, 3)
    var i = 0
    while i < arr.data.length do
      assertEquals(arr.data(i), true)
      i += 1
    end while
  }

  test("NDArray.fill") {
    val arr = NDArray.fill(Array(2, 4), 7.0)
    assertEquals(arr.numel, 8)
    var i = 0
    while i < arr.data.length do
      assertEquals(arr.data(i), 7.0)
      i += 1
    end while
  }

  // ---- Property tests ----

  test("isColMajor true for default construction") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assert(arr.isColMajor)
    assert(arr.isContiguous)
  }

  test("isColMajor false for row-major strides") {
    // Row-major for shape [2,3]: strides = [3, 1]
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3), Array(3, 1), 0)
    assert(!arr.isColMajor)
  }

  test("isRowMajor") {
    // Row-major for shape [2,3]: strides = [3, 1]
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3), Array(3, 1), 0)
    assert(arr.isRowMajor)
    assert(arr.isContiguous)
  }

  test("isContiguous false for strided view") {
    // Non-contiguous: strides = [2, 4] for shape [2, 3] in an 8-element array
    val arr = NDArray(Array.fill(12)(1.0), Array(2, 3), Array(2, 4), 0)
    assert(!arr.isContiguous)
  }

  test("numel") {
    val arr = NDArray(Array.fill(60)(0.0), Array(3, 4, 5))
    assertEquals(arr.numel, 60)
  }

  test("ndim") {
    val arr = NDArray(Array.fill(24)(0.0), Array(2, 3, 4))
    assertEquals(arr.ndim, 3)
    assertEquals(arr.ndim, arr.shape.length)
  }

  test("layout string") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    val l = arr.layout
    assert(l.contains("ndim"))
    assert(l.contains("shape"))
    assert(l.contains("strides"))
    assert(l.contains("offset"))
  }

  // ---- Validation tests (bounds checking) ----

  test("rejects shape/data size mismatch") {
    intercept[InvalidNDArray] {
      NDArray(Array(1.0, 2.0, 3.0), Array(2, 3))
    }
  }

  test("rejects negative dimensions") {
    intercept[InvalidNDArray] {
      NDArray(Array(1.0, 2.0), Array(-1, 2), Array(1, -1), 0)
    }
  }

  test("rejects empty shape") {
    intercept[InvalidNDArray] {
      NDArray.zeros[Double](Array())
    }
  }

  test("rejects stride/shape rank mismatch") {
    intercept[InvalidNDArray] {
      NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3), Array(1), 0)
    }
  }

  test("rejects offset out of bounds") {
    intercept[java.lang.IndexOutOfBoundsException] {
      NDArray(Array(1.0, 2.0, 3.0), Array(1, 3), Array(1, 1), 10)
    }
  }

  test("bounds check can be disabled") {
    import BoundsCheck.DoBoundsCheck.no
    // Invalid: product of shape (6) != data.length (3), but no exception expected
    val arr = NDArray(Array(1.0, 2.0, 3.0), Array(2, 3))(using no)
    assertEquals(arr.shape.toSeq, Seq(2, 3))
  }

  // ---- Consistency with Matrix ----

  test("2D NDArray strides match Matrix column-major convention") {
    // For shape [r, c], column-major strides = [1, r]
    val r = 3
    val c = 4
    val arr = NDArray(Array.fill(r * c)(0.0), Array(r, c))
    assertEquals(arr.strides.toSeq, Seq(1, r))
  }

end NDArraySuite
