package vecxt

import vecxt.LongArrays.*

class LongArraysSuite extends munit.FunSuite:

  test("sumSIMD - empty array"):
    val arr = Array.empty[Long]
    assertEquals(arr.sumSIMD, 0L)

  test("sumSIMD - single element"):
    val arr = Array(42L)
    assertEquals(arr.sumSIMD, 42L)

  test("sumSIMD - small array (below SIMD threshold)"):
    val arr = Array(1L, 2L, 3L, 4L, 5L)
    assertEquals(arr.sumSIMD, 15L)

  test("sumSIMD - array exactly matching vector length"):
    val arr = Array.fill(LongArrays.length)(10L)
    assertEquals(arr.sumSIMD, 10L * LongArrays.length)

end LongArraysSuite
