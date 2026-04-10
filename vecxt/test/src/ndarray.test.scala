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

  test("NDArray.zeros(Array()) creates 0-d zero") {
    val arr = NDArray.zeros[Double](Array())
    assertEquals(arr.ndim, 0)
    assertEquals(arr.numel, 1)
    assertClose(arr.data(0), 0.0)
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

class NDArrayM2Suite extends FunSuite:

  import ndarrayOps.*

  // ── Element read (apply) ─────────────────────────────────────────────────

  test("apply 1D reads correct element") {
    val arr = NDArray.fromArray(Array(10.0, 20.0, 30.0))
    assertEquals(arr(0), 10.0)
    assertEquals(arr(1), 20.0)
    assertEquals(arr(2), 30.0)
  }

  test("apply 2D reads correct element (col-major)") {
    // col-major 2x3: data = [col0row0, col0row1, col1row0, col1row1, col2row0, col2row1]
    //                      = [1, 2, 3, 4, 5, 6]
    // (row=0,col=0)=1, (row=1,col=0)=2, (row=0,col=1)=3 ...
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3))
    assertEquals(arr(0, 0), 1.0)
    assertEquals(arr(1, 0), 2.0)
    assertEquals(arr(0, 1), 3.0)
    assertEquals(arr(1, 1), 4.0)
    assertEquals(arr(0, 2), 5.0)
    assertEquals(arr(1, 2), 6.0)
  }

  test("apply 3D reads correct element") {
    // shape [2,3,4], col-major, data 0..23
    val data = Array.tabulate(24)(_.toDouble)
    val arr = NDArray(data, Array(2, 3, 4))
    // strides = [1, 2, 6]
    assertEquals(arr(0, 0, 0), 0.0)
    assertEquals(arr(1, 0, 0), 1.0)
    assertEquals(arr(0, 1, 0), 2.0)
    assertEquals(arr(0, 0, 1), 6.0)
    assertEquals(arr(1, 2, 3), (1 + 2 * 2 + 3 * 6).toDouble) // 1 + 4 + 18 = 23
  }

  test("apply 4D reads correct element") {
    val data = Array.tabulate(120)(_.toDouble)
    val arr = NDArray(data, Array(2, 3, 4, 5))
    // strides = [1, 2, 6, 24]
    assertEquals(arr(0, 0, 0, 0), 0.0)
    assertEquals(arr(1, 0, 0, 0), 1.0)
    assertEquals(arr(0, 1, 0, 0), 2.0)
    assertEquals(arr(0, 0, 1, 0), 6.0)
    assertEquals(arr(0, 0, 0, 1), 24.0)
  }

  test("apply Array[Int] reads correct element (N-D)") {
    val data = Array.tabulate(24)(_.toDouble)
    val arr = NDArray(data, Array(2, 3, 4))
    assertEquals(arr(Array(1, 2, 3)), arr(1, 2, 3))
    assertEquals(arr(Array(0, 0, 0)), 0.0)
  }

  test("apply on strided/offset view reads correct element") {
    // Slice of row 1 from 2x3 col-major array
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val arr = NDArray(data, Array(2, 3))
    // slice dim=0 (rows) from 1 to 2 → view of row 1
    val row1 = arr.slice(0, 1, 2)
    assertEquals(row1.shape.toSeq, Seq(1, 3))
    assertEquals(row1(0, 0), 2.0)
    assertEquals(row1(0, 1), 4.0)
    assertEquals(row1(0, 2), 6.0)
  }

  // ── Element write (update) ───────────────────────────────────────────────

  test("update 1D mutates element") {
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))
    arr.update(1, 99.0)
    assertEquals(arr(1), 99.0)
    assertEquals(arr(0), 1.0) // others unchanged
  }

  test("update 2D mutates element") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    arr.update(0, 1, 77.0)
    assertEquals(arr(0, 1), 77.0)
    assertEquals(arr(1, 0), 2.0) // others unchanged
  }

  test("mutation through view is visible in original data") {
    val data = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val arr = NDArray(data, Array(2, 3))
    val view = arr.slice(1, 1, 3) // cols 1..2
    view.update(0, 0, 99.0) // modifies arr(0,1)
    assertEquals(arr(0, 1), 99.0)
    assert(view.data eq arr.data) // same backing array
  }

  // ── Slice ────────────────────────────────────────────────────────────────

  test("slice produces correct shape/offset/strides") {
    val arr = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
    val s = arr.slice(1, 1, 3) // cols 1..2
    assertEquals(s.shape.toSeq, Seq(3, 2))
    assertEquals(s.offset, 1 * arr.strides(1)) // offset = start * stride(1) = 1 * 3 = 3
    assertEquals(s.strides.toSeq, arr.strides.toSeq)
  }

  test("slice of slice") {
    val arr = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
    val s1 = arr.slice(0, 1, 3) // rows 1..2
    val s2 = s1.slice(0, 0, 1) // row 1 only from s1
    assertEquals(s2.shape.toSeq, Seq(1, 4))
    assertEquals(s2(0, 0), arr(1, 0))
    assertEquals(s2(0, 3), arr(1, 3))
  }

  test("slice data aliasing — mutation visible through slice") {
    val data = Array.tabulate(6)(_.toDouble)
    val arr = NDArray(data, Array(2, 3))
    val s = arr.slice(0, 0, 1) // rows 0..0
    s.update(0, 2, 999.0)
    assertEquals(arr(0, 2), 999.0)
  }

  // ── Transpose / T ───────────────────────────────────────────────────────

  test("T swaps shape and strides of 2D array") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    val t = arr.T
    assertEquals(t.shape.toSeq, Seq(3, 2))
    assertEquals(t.strides.toSeq, Seq(arr.strides(1), arr.strides(0)).map(identity))
    assert(t.data eq arr.data)
  }

  test("T.T is identity layout") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    val tt = arr.T.T
    assertEquals(tt.shape.toSeq, arr.shape.toSeq)
    assertEquals(tt.strides.toSeq, arr.strides.toSeq)
  }

  test("T element equivalence: arr(i,j) == arr.T(j,i)") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    var i = 0
    while i < arr.shape(0) do
      var j = 0
      while j < arr.shape(1) do
        assertEquals(arr(i, j), arr.T(j, i))
        j += 1
      end while
      i += 1
    end while
  }

  test("transpose with N-D permutation") {
    val data = Array.tabulate(24)(_.toDouble)
    val arr = NDArray(data, Array(2, 3, 4))
    val t = arr.transpose(Array(2, 0, 1)) // (d,r,c) -> (c,d,r)
    assertEquals(t.shape.toSeq, Seq(4, 2, 3))
    assertEquals(t.strides.toSeq, Seq(arr.strides(2), arr.strides(0), arr.strides(1)).map(identity))
    assertEquals(t(2, 1, 0), arr(1, 0, 2))
  }

  test("transpose rejects invalid permutation — duplicate") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    intercept[InvalidNDArray] {
      arr.transpose(Array(0, 0))
    }
  }

  test("transpose rejects invalid permutation — wrong length") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    intercept[InvalidNDArray] {
      arr.transpose(Array(0, 1, 2))
    }
  }

  // ── Reshape ──────────────────────────────────────────────────────────────

  test("reshape contiguous — returns view with new strides") {
    val arr = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
    assert(arr.isContiguous)
    val r = arr.reshape(Array(4, 3))
    assertEquals(r.shape.toSeq, Seq(4, 3))
    assertEquals(r.strides.toSeq, Seq(1, 4))
    assert(r.data eq arr.data) // same backing data
  }

  test("reshape non-contiguous — copies to new contiguous array") {
    val arr = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
    val t = arr.T // row-major strides, NOT col-major
    assert(!t.isColMajor)
    // toArray gives col-major order: elements in col-major traversal
    val r = t.reshape(Array(12))
    assertEquals(r.shape.toSeq, Seq(12))
    // r should contain the same logical elements as t in col-major order
    val expected = t.toArray
    assertEquals(r.toArray.toSeq, expected.toSeq)
  }

  test("reshape rejects wrong numel") {
    val arr = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
    intercept[InvalidNDArray] {
      arr.reshape(Array(5, 5))
    }
  }

  // ── Squeeze / Unsqueeze ──────────────────────────────────────────────────

  test("squeeze removes all size-1 dimensions") {
    val arr = NDArray(Array(1.0, 2.0, 3.0), Array(1, 3, 1))
    val s = arr.squeeze
    assertEquals(s.shape.toSeq, Seq(3))
    assert(s.data eq arr.data)
  }

  test("squeeze(dim) removes specific size-1 dimension") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(1, 2, 3))
    val s = arr.squeeze(0)
    assertEquals(s.shape.toSeq, Seq(2, 3))
    assert(s.data eq arr.data)
  }

  test("squeeze(dim) rejects non-size-1 dimension") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    intercept[InvalidNDArray] {
      arr.squeeze(0)
    }
  }

  test("unsqueeze inserts size-1 dimension at given position") {
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))
    val u = arr.unsqueeze(0)
    assertEquals(u.shape.toSeq, Seq(1, 3))
    assert(u.data eq arr.data)
  }

  test("unsqueeze at end") {
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))
    val u = arr.unsqueeze(1)
    assertEquals(u.shape.toSeq, Seq(3, 1))
    assert(u.data eq arr.data)
  }

  test("unsqueeze.squeeze round-trip") {
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))
    val u = arr.unsqueeze(0)
    val s = u.squeeze
    assertEquals(s.shape.toSeq, arr.shape.toSeq)
    assertEquals(s.strides.toSeq, arr.strides.toSeq)
  }

  test("expandDims is alias for unsqueeze") {
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))
    val e = arr.expandDims(0)
    val u = arr.unsqueeze(0)
    assertEquals(e.shape.toSeq, u.shape.toSeq)
    assertEquals(e.strides.toSeq, u.strides.toSeq)
  }

  // ── Flatten ──────────────────────────────────────────────────────────────

  test("flatten of contiguous array returns view (same data)") {
    val arr = NDArray(Array.tabulate(12)(_.toDouble), Array(3, 4))
    assert(arr.isContiguous)
    val f = arr.flatten
    assertEquals(f.shape.toSeq, Seq(12))
    assertEquals(f.strides.toSeq, Seq(1))
    assert(f.data eq arr.data)
  }

  test("flatten of non-contiguous array returns fresh array") {
    // Build a genuinely non-contiguous array: strides [2, 4] don't match col-major [1, 2] or row-major [3, 1]
    val nc = NDArray(Array.tabulate(12)(_.toDouble), Array(2, 3), Array(2, 4), 0)
    assert(!nc.isContiguous)
    val f = nc.flatten
    assertEquals(f.shape.toSeq, Seq(6))
    // data is a fresh copy (not eq to nc.data)
    assert(!(f.data eq nc.data))
    // values match toArray output (col-major traversal order)
    assertEquals(f.toArray.toSeq, nc.toArray.toSeq)
  }

  // ── toArray ──────────────────────────────────────────────────────────────

  test("toArray of col-major array returns clone") {
    val data = Array.tabulate(6)(_.toDouble)
    val arr = NDArray(data, Array(2, 3))
    assert(arr.isColMajor)
    val result = arr.toArray
    assertEquals(result.toSeq, data.toSeq)
    assert(!(result eq data)) // clone, not same reference
  }

  test("toArray of strided view materialises correctly") {
    // row-major 2x3: strides [3,1], shape [2,3]
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3), Array(3, 1), 0)
    assert(!arr.isColMajor)
    val result = arr.toArray
    // col-major traversal: (0,0)=1,(1,0)=4,(0,1)=2,(1,1)=5,(0,2)=3,(1,2)=6
    assertEquals(result.toSeq, Seq(1.0, 4.0, 2.0, 5.0, 3.0, 6.0))
  }

  // ── Bounds check ────────────────────────────────────────────────────────

  test("apply 1D out-of-range throws") {
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))
    intercept[java.lang.IndexOutOfBoundsException] {
      arr(5)
    }
  }

  test("apply 2D wrong rank throws") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    intercept[InvalidNDArray] {
      arr(0) // expects ndim=1, got ndim=2
    }
  }

  test("apply Array[Int] wrong rank throws") {
    val arr = NDArray(Array(1.0, 2.0, 3.0, 4.0), Array(2, 2))
    intercept[InvalidNDArray] {
      arr(Array(0, 0, 0)) // 3 indices for a 2D array
    }
  }

  test("bounds check can be disabled for apply") {
    import BoundsCheck.DoBoundsCheck.no
    val arr = NDArray.fromArray(Array(1.0, 2.0, 3.0))(using no)
    val v = arr(0)(using no)
    assertEquals(v, 1.0)
  }

  test("T rejects non-2D array") {
    val arr = NDArray(Array.tabulate(24)(_.toDouble), Array(2, 3, 4))
    intercept[InvalidNDArray] {
      arr.T
    }
  }

  test("slice rejects invalid dim") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    intercept[InvalidNDArray] {
      arr.slice(5, 0, 1)
    }
  }

  test("slice rejects start >= end") {
    val arr = NDArray(Array.tabulate(6)(_.toDouble), Array(2, 3))
    intercept[InvalidNDArray] {
      arr.slice(0, 1, 1)
    }
  }

end NDArrayM2Suite
