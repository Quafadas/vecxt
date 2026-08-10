package vecxt

import all.*
import dimensionExtender.DimensionExtender.Dimension.*

class IntMatrixReduceSuite extends munit.FunSuite:

  test("sum reduction on a strided (non-contiguous) Int matrix"):
    // Dense row-major [[1,4],[2,5],[3,6]] (row stride 2, col stride 1). reduceAlongDimension used to bail out
    // with `???` for any layout that wasn't `hasSimpleContiguousMemoryLayout`, even though it reads every
    // element through `m.layout.linearIndex`, which already handles arbitrary strides correctly.
    val mat1 = Matrix[Int](Array(1, 4, 2, 5, 3, 6), 3, 2, 2, 1, 0)
    assert(mat1.isDenseRowMajor)

    val sumR = mat1.sum(Rows)
    assertMatrixEquals(sumR, Matrix[Int](Array(5, 7, 9), (3, 1)))

    val sumC = mat1.sum(Cols)
    assertMatrixEquals(sumC, Matrix[Int](Array(6, 15), (1, 2)))

  test("max/min/product reduction on a strided Int matrix"):
    val mat1 = Matrix[Int](Array(1, 4, 2, 5, 3, 6), 3, 2, 2, 1, 0)

    assertMatrixEquals(mat1.max(Rows), Matrix[Int](Array(4, 5, 6), (3, 1)))
    assertMatrixEquals(mat1.min(Cols), Matrix[Int](Array(1, 4), (1, 2)))
    assertMatrixEquals(mat1.product(Rows), Matrix[Int](Array(4, 10, 18), (3, 1)))

end IntMatrixReduceSuite
