package vecxt

import vecxt.LongArrays.*

class MatrixIOSuite extends munit.FunSuite:

  test("sumSIMD - empty array"):
    val arr = Array.empty[Long]
    assertEquals(arr.sumSIMD, 0L)


end MatrixIOSuite
