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

  test("Matrix.apply(raw, layout) throws when stride/offset exceeds array bounds"):
    // 2×2 layout with rowStride=60 — accessing (1, 0) yields index 60, beyond array size 4
    val raw = Array.tabulate(4)(_.toDouble)
    val l = Layout(2, 2, 60, 1, 0, 4)
    intercept[IndexOutOfBoundsException]:
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

  // ─── generated layouts (hand-rolled, no scalacheck) ──────────────────────────

  // Small dimension/stride/offset tuples, deliberately excluding zero strides —
  // those are broadcast layouts and are deliberately non-injective; tested separately below.
  private def generatedLayouts: Seq[Layout] =
    for
      rows <- 1 to 4
      cols <- 1 to 4
      rowStride <- Seq(1, 2, 3)
      offset <- Seq(0, 1, 5)
    yield
      val colStride = rowStride * rows
      val dataLength = offset + rowStride * (rows - 1) + colStride * (cols - 1) + 1
      Layout(rows, cols, rowStride, colStride, offset, dataLength)

  private def generatedBroadcastLayouts: Seq[Layout] =
    for
      rows <- 1 to 4
      cols <- 1 to 4
    yield
      // rowStride == 0 : every row aliases the same slice, deliberately non-injective
      Layout(rows, cols, 0, 1, 0, cols)

  // ─── linearIndex injectivity ──────────────────────────────────────────────────

  test("linearIndex is injective over generated non-broadcast layouts"):
    for l <- generatedLayouts do
      val indices =
        for
          i <- 0 until l.rows
          j <- 0 until l.cols
        yield l.linearIndex(i, j)
      assertEquals(
        indices.distinct.size,
        l.numel,
        s"layout $l produced ${indices.distinct.size} distinct indices for numel=${l.numel}"
      )
    end for

  test("linearIndex is deliberately non-injective for broadcast (zero-stride) layouts"):
    for l <- generatedBroadcastLayouts if l.rows > 1 do
      val indices =
        for
          i <- 0 until l.rows
          j <- 0 until l.cols
        yield l.linearIndex(i, j)
      assert(
        indices.distinct.size < l.numel,
        s"expected broadcast layout $l to alias indices, but all ${l.numel} were distinct"
      )
    end for

  // ─── linearIndex in-bounds ────────────────────────────────────────────────────

  test("linearIndex stays within [0, dataLength) for generated layouts"):
    for l <- (generatedLayouts ++ generatedBroadcastLayouts) do
      for
        i <- 0 until l.rows
        j <- 0 until l.cols
      do
        val idx = l.linearIndex(i, j)
        assert(idx >= 0, s"layout $l produced negative index $idx at ($i, $j)")
        assert(
          idx < l.dataLength,
          s"layout $l produced out-of-bounds index $idx at ($i, $j), dataLength=${l.dataLength}"
        )
      end for
    end for

  // ─── submatrix offset composition ─────────────────────────────────────────────

  // Mirrors the arithmetic in MatrixInstance.submatrix:
  // newOffset = m.offset + newRows.head * m.rowStride + newCols.head * m.colStride
  private def contiguousSubLayout(l: Layout, rowStart: Int, rowSpan: Int, colStart: Int, colSpan: Int): Layout =
    val newOffset = l.offset + rowStart * l.rowStride + colStart * l.colStride
    Layout(rowSpan, colSpan, l.rowStride, l.colStride, newOffset, l.dataLength)
  end contiguousSubLayout

  test("submatrix of a submatrix equals the composed submatrix directly"):
    for l <- generatedLayouts if l.rows >= 3 && l.cols >= 3 do
      // First take rows [1, rows), cols [1, cols) — a contiguous sub-view
      val sub1 = contiguousSubLayout(l, 1, l.rows - 1, 1, l.cols - 1)
      // Then take rows [1, rows-1) of *that sub-view* — i.e. rows [2, rows) of the original
      val sub2 = contiguousSubLayout(sub1, 1, sub1.rows - 1, 1, sub1.cols - 1)

      // Directly composed: rows [2, rows-1), cols [2, cols-1) of the original
      val direct = contiguousSubLayout(l, 2, l.rows - 2, 2, l.cols - 2)

      assertEquals(sub2.offset, direct.offset)
      assertEquals(sub2.rowStride, direct.rowStride)
      assertEquals(sub2.colStride, direct.colStride)
      assertEquals(sub2.rows, direct.rows)
      assertEquals(sub2.cols, direct.cols)
    end for

  // ─── transpose round-trip over generated layouts ──────────────────────────────

  test("transpose.transpose == original over generated layouts"):
    for l <- (generatedLayouts ++ generatedBroadcastLayouts) do assertEquals(l.transpose.transpose, l)
    end for

  test("transpose.linearIndex(j, i) == linearIndex(i, j) over generated layouts"):
    for l <- generatedLayouts do
      val lt = l.transpose
      for
        i <- 0 until l.rows
        j <- 0 until l.cols
      do assertEquals(lt.linearIndex(j, i), l.linearIndex(i, j))
      end for
    end for

end LayoutTest
