package vecxt

import munit.FunSuite
import narr.*
import vecxt.BoundsCheck.BoundsCheck
import vecxt.matrix.*

class StrideMatInstantiateCheckTest extends FunSuite:

  // Test with bounds checking enabled
  import BoundsCheck.DoBoundsCheck.yes

  test("strideMatInstantiateCheck - valid matrix configurations should pass"):
    val data = NArray.tabulate(12)(_.toDouble)

    // Standard column-major 3x4 matrix
    strideMatInstantiateCheck[Double](data, 3, 4, 1, 3, 0)

    // Row-major layout
    strideMatInstantiateCheck[Double](data, 3, 4, 4, 1, 0)

    // With offset
    strideMatInstantiateCheck[Double](data, 2, 2, 1, 2, 2)

    // Single row/column
    strideMatInstantiateCheck[Double](data, 1, 12, 12, 1, 0)
    strideMatInstantiateCheck[Double](data, 12, 1, 1, 12, 0)

  test("strideMatInstantiateCheck - invalid dimensions should fail"):
    val data = NArray.tabulate(12)(_.toDouble)

    intercept[InvalidMatrix]:
      strideMatInstantiateCheck[Double](data, 0, 4, 1, 3, 0)

    intercept[InvalidMatrix]:
      strideMatInstantiateCheck[Double](data, 3, -1, 1, 3, 0)

  test("strideMatInstantiateCheck - invalid offset should fail"):
    val data = NArray.tabulate(12)(_.toDouble)

    // Negative offset
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 3, 4, 1, 3, -1)

    // Offset >= array size
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 3, 4, 1, 3, 12)

    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 3, 4, 1, 3, 15)

  test("strideMatInstantiateCheck - out of bounds access should fail"):
    val data = NArray.tabulate(12)(_.toDouble)

    // Matrix would access beyond array bounds
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 4, 4, 1, 4, 0) // needs 16 elements, only have 12

    // Large strides causing out of bounds
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 3, 3, 2, 5, 0) // max index would be 0 + 2*2 + 5*2 = 14

    // Offset + matrix size exceeds bounds
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 2, 2, 1, 2, 10) // max index would be 10 + 1 + 2 = 13

  test("strideMatInstantiateCheck - negative strides should work when valid"):
    val data = NArray.tabulate(12)(_.toDouble) // indices 0-11

    // Valid negative strides (useful for reversed views)
    strideMatInstantiateCheck[Double](data, 3, 4, -1, 3, 2) // start from offset 2, go backwards
    strideMatInstantiateCheck[Double](data, 3, 4, 1, -3, 9) // start from offset 9, columns go backwards

  test("strideMatInstantiateCheck - negative strides with invalid bounds should fail"):
    val data = NArray.tabulate(12)(_.toDouble) // indices 0-11

    // Negative stride would access negative indices
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 3, 4, -1, 3, 1) // min index would be negative

    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 3, 4, 1, -3, 8) // min index would be negative

    // Valid bounds but max exceeds array size
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](data, 3, 4, 1, -3, 11) // max index >= 12

  test("strideMatInstantiateCheck - edge cases"):
    val data = NArray.tabulate(1)(_.toDouble)

    // Single element matrix with valid strides
    strideMatInstantiateCheck[Double](data, 1, 1, 1, 1, 0)
    strideMatInstantiateCheck[Double](data, 1, 1, 0, 0, 0) // broadcast style

    // Invalid strides for 1x1 matrix
    intercept[IllegalArgumentException]:
      strideMatInstantiateCheck[Double](data, 1, 1, 5, 1, 0)

    val emptyData = NArray.ofSize[Double](0)

    // Empty data should fail
    intercept[IndexOutOfBoundsException]:
      strideMatInstantiateCheck[Double](emptyData, 1, 1, 1, 1, 0)

  test("strideMatInstantiateCheck - realistic linear algebra scenarios"):
    val data = NArray.tabulate(20)(_.toDouble)

    // Submatrix view (2x3 starting at position 5 in a conceptual 4x5 matrix)
    strideMatInstantiateCheck[Double](data, 2, 3, 1, 4, 5)

    // Transposed view (using swapped strides)
    strideMatInstantiateCheck[Double](data, 4, 5, 5, 1, 0) // 4x5 matrix, row-major
    strideMatInstantiateCheck[Double](data, 5, 4, 1, 5, 0) // transposed to 5x4, column-major

    // Strided access (every other element in column-major layout)
    strideMatInstantiateCheck[Double](data, 2, 5, 2, 4, 0)

    // Broadcasting scenarios
    strideMatInstantiateCheck[Double](data, 4, 5, 1, 0, 0) // broadcast columns
    strideMatInstantiateCheck[Double](data, 4, 5, 0, 1, 0) // broadcast rows

  test("strideMatInstantiateCheck - bounds checking disabled should skip validation"):
    import BoundsCheck.DoBoundsCheck.no
    val data = NArray.tabulate(12)(_.toDouble)

    // These would normally fail, but should pass with bounds checking disabled
    strideMatInstantiateCheck[Double](data, 0, 4, 1, 3, 0) // invalid dimensions
    strideMatInstantiateCheck[Double](data, 4, 4, 1, 4, 0) // out of bounds
    strideMatInstantiateCheck[Double](data, 3, 4, 1, 3, -1) // negative offset

end StrideMatInstantiateCheckTest
