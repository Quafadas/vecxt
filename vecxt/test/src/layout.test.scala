package vecxt

import munit.FunSuite
import vecxt.matrix.*

class LayoutTest extends FunSuite:

  // ─── helpers ────────────────────────────────────────────────────────────────

  private def colMajorLayout(rows: Int, cols: Int): Layout =
    Layout(rows, cols, 1, rows, 0, rows * cols)

  private def rowMajorLayout(rows: Int, cols: Int): Layout =
    Layout(rows, cols, cols, 1, 0, rows * cols)

  // ─── derived flags ──────────────────────────────────────────────────────────

  test("isDenseColMajor — true for standard col-major layout"):
    val l = colMajorLayout(3, 4)
    assert(l.isDenseColMajor)
    assert(!l.isDenseRowMajor)

  test("isDenseRowMajor — true for standard row-major layout"):
    val l = rowMajorLayout(3, 4)
    assert(l.isDenseRowMajor)
    assert(!l.isDenseColMajor)

  test("isDenseColMajor — false when rowStride != 1"):
    val l = Layout(3, 4, 2, 3, 0, 3 * 4 * 2)
    assert(!l.isDenseColMajor)
    assert(!l.hasSimpleContiguousMemoryLayout)

  test("isDenseColMajor — false when colStride != rows"):
    val l = Layout(3, 4, 1, 2, 0, 3 * 4)
    assert(!l.isDenseColMajor)
    assert(!l.hasSimpleContiguousMemoryLayout)

  test("hasSimpleContiguousMemoryLayout — false when offset != 0"):
    val l = Layout(3, 4, 1, 3, 2, 14)
    assert(!l.hasSimpleContiguousMemoryLayout)

  test("hasSimpleContiguousMemoryLayout — false when dataLength != numel"):
    // Dense col-major strides but extra elements in the backing array
    val l = Layout(3, 4, 1, 3, 0, 13)
    assert(l.isDenseColMajor)
    assert(!l.hasSimpleContiguousMemoryLayout)

  test("hasSimpleContiguousMemoryLayout — true for dense col-major, dataLength == numel"):
    val l = colMajorLayout(3, 4)
    assert(l.hasSimpleContiguousMemoryLayout)

  test("hasSimpleContiguousMemoryLayout — true for dense row-major, dataLength == numel"):
    val l = rowMajorLayout(3, 4)
    assert(l.hasSimpleContiguousMemoryLayout)

  test("numel is rows * cols"):
    val l = colMajorLayout(5, 7)
    assertEquals(l.numel, 35)

  // ─── linearIndex ────────────────────────────────────────────────────────────

  test("linearIndex — col-major agrees with hand-written arithmetic"):
    val l = colMajorLayout(3, 4)
    for i <- 0 until 3 do
      for j <- 0 until 4 do assertEquals(l.linearIndex(i, j), l.offset + i * l.rowStride + j * l.colStride)
    end for

  test("linearIndex — row-major agrees with hand-written arithmetic"):
    val l = rowMajorLayout(3, 4)
    for i <- 0 until 3 do
      for j <- 0 until 4 do assertEquals(l.linearIndex(i, j), l.offset + i * l.rowStride + j * l.colStride)
    end for

  test("linearIndex — strided layout agrees with hand-written arithmetic"):
    val l = Layout(3, 4, 2, 6, 1, 3 * 4 * 2 * 3)
    for i <- 0 until 3 do for j <- 0 until 4 do assertEquals(l.linearIndex(i, j), 1 + i * 2 + j * 6)
    end for

  test("linearIndex — with offset"):
    val l = Layout(2, 2, 1, 2, 5, 14)
    assertEquals(l.linearIndex(0, 0), 5)
    assertEquals(l.linearIndex(1, 0), 6)
    assertEquals(l.linearIndex(0, 1), 7)
    assertEquals(l.linearIndex(1, 1), 8)

  test("linearIndex — transposed layout"):
    val l = colMajorLayout(3, 4)
    val lt = l.transpose
    // After transpose: rows = 4, cols = 3; accessing (j, i) in transposed == (i, j) in original
    for i <- 0 until 3 do for j <- 0 until 4 do assertEquals(lt.linearIndex(j, i), l.linearIndex(i, j))
    end for

  test("linearIndex — negative strides (reversed view)"):
    // row-reversed view: start at index 2, rowStride = -1, one column
    val l = Layout(3, 1, -1, 3, 2, 3)
    assertEquals(l.linearIndex(0, 0), 2)
    assertEquals(l.linearIndex(1, 0), 1)
    assertEquals(l.linearIndex(2, 0), 0)

  test("linearIndex — zero row stride (broadcast rows)"):
    val l = Layout(4, 3, 0, 1, 0, 3)
    // All rows point to the same slice
    for i <- 0 until 4 do for j <- 0 until 3 do assertEquals(l.linearIndex(i, j), j)
    end for

  // ─── transpose ──────────────────────────────────────────────────────────────

  test("transpose swaps rows/cols and strides"):
    val l = colMajorLayout(3, 4)
    val lt = l.transpose
    assertEquals(lt.rows, 4)
    assertEquals(lt.cols, 3)
    assertEquals(lt.rowStride, l.colStride)
    assertEquals(lt.colStride, l.rowStride)
    assertEquals(lt.offset, l.offset)
    assertEquals(lt.dataLength, l.dataLength)

  test("transpose.transpose == original (property)"):
    val l = Layout(5, 7, 2, 10, 3, 100)
    assertEquals(l.transpose.transpose, l)

  test("transpose.linearIndex(j, i) == linearIndex(i, j) (property)"):
    val l = colMajorLayout(4, 6)
    val lt = l.transpose
    for i <- 0 until 4 do for j <- 0 until 6 do assertEquals(lt.linearIndex(j, i), l.linearIndex(i, j))
    end for

  // ─── withDataLength ─────────────────────────────────────────────────────────

  test("withDataLength changes dataLength and nothing else"):
    val l = colMajorLayout(3, 4)
    val l2 = l.withDataLength(999)
    assertEquals(l2.dataLength, 999)
    assertEquals(l2.rows, l.rows)
    assertEquals(l2.cols, l.cols)
    assertEquals(l2.rowStride, l.rowStride)
    assertEquals(l2.colStride, l.colStride)
    assertEquals(l2.offset, l.offset)
    assertEquals(l2.kind, l.kind)

  // ─── equals / hashCode ──────────────────────────────────────────────────────

  test("equals — same fields means equal"):
    val l1 = colMajorLayout(3, 4)
    val l2 = colMajorLayout(3, 4)
    assertEquals(l1, l2)

  test("equals — different rows means not equal"):
    assert(colMajorLayout(3, 4) != colMajorLayout(2, 4))

  test("equals — different cols means not equal"):
    assert(colMajorLayout(3, 4) != colMajorLayout(3, 5))

  test("equals — different rowStride means not equal"):
    val l1 = Layout(3, 4, 1, 3, 0, 12)
    val l2 = Layout(3, 4, 2, 3, 0, 12)
    assert(l1 != l2)

  test("equals — different offset means not equal"):
    val l1 = Layout(3, 4, 1, 3, 0, 14)
    val l2 = Layout(3, 4, 1, 3, 2, 14)
    assert(l1 != l2)

  test("equals — different dataLength means not equal"):
    val l1 = Layout(3, 4, 1, 3, 0, 12)
    val l2 = Layout(3, 4, 1, 3, 0, 13)
    assert(l1 != l2)

  test("hashCode — equal layouts have the same hashCode"):
    val l1 = colMajorLayout(3, 4)
    val l2 = colMajorLayout(3, 4)
    assertEquals(l1.hashCode(), l2.hashCode())

  test("equals — not equal to non-Layout"):
    val l: Any = colMajorLayout(3, 4)
    assert(l != "not a layout")
    assert(l != 42)

  // ─── toString ───────────────────────────────────────────────────────────────

  test("toString matches original Matrix#layout format"):
    val l = Layout(3, 4, 1, 3, 0, 12)
    assertEquals(l.toString, "rows: 3, cols: 4, rowStride: 1, colStride: 3, offset: 0, data length: 12")

  // ─── sameElementOrderAs ─────────────────────────────────────────────────────

  test("sameElementOrderAs — two col-major layouts with same rowStride"):
    val l1 = colMajorLayout(3, 4)
    val l2 = colMajorLayout(3, 4)
    assert(l1.sameElementOrderAs(l2))

  test("sameElementOrderAs — two row-major layouts with same colStride"):
    val l1 = rowMajorLayout(3, 4)
    val l2 = rowMajorLayout(3, 4)
    assert(l1.sameElementOrderAs(l2))

  test("sameElementOrderAs — col-major vs row-major is false"):
    val l1 = colMajorLayout(3, 4)
    val l2 = rowMajorLayout(3, 4)
    assert(!l1.sameElementOrderAs(l2))

  // ─── Matrix.apply(raw, layout) ───────────────────────────────────────────────

  test("Matrix.apply(raw, layout) succeeds when raw.size == layout.dataLength"):
    val raw = Array.tabulate(12)(_.toDouble)
    val l = colMajorLayout(3, 4)
    val m = Matrix(raw, l)
    assertEquals(m.rows, 3)
    assertEquals(m.cols, 4)

  test("Matrix.apply(raw, layout) throws when raw.size != layout.dataLength"):
    val raw = Array.tabulate(10)(_.toDouble)
    val l = colMajorLayout(3, 4) // dataLength = 12
    intercept[IllegalArgumentException]:
      Matrix(raw, l)

  // ─── Matrix forwarders ──────────────────────────────────────────────────────

  test("Matrix forwarders delegate to layout"):
    val raw = Array.tabulate(12)(_.toDouble)
    val l = colMajorLayout(3, 4)
    val m = Matrix(raw, l)
    assertEquals(m.rows, l.rows)
    assertEquals(m.cols, l.cols)
    assertEquals(m.rowStride, l.rowStride)
    assertEquals(m.colStride, l.colStride)
    assertEquals(m.offset, l.offset)
    assertEquals(m.numel, l.numel)
    assertEquals(m.isDenseColMajor, l.isDenseColMajor)
    assertEquals(m.isDenseRowMajor, l.isDenseRowMajor)
    assertEquals(m.hasSimpleContiguousMemoryLayout, l.hasSimpleContiguousMemoryLayout)

  test("Matrix.layoutString delegates to layout.toString"):
    val raw = Array.tabulate(12)(_.toDouble)
    val l = colMajorLayout(3, 4)
    val m = Matrix(raw, l)
    assertEquals(m.layoutString, l.toString)

end LayoutTest
