package vecxt

import all.*

class LongArraysSharedSuite extends munit.FunSuite:

  test("sumSIMD - empty array"):
    val arr = Array.empty[Long]
    assertEquals(arr.sumSIMD, 0L)

  test("sumSIMD - single element"):
    val arr = Array(42L)
    assertEquals(arr.sumSIMD, 42L)

  test("sumSIMD - small array"):
    val arr = Array(1L, 2L, 3L, 4L, 5L)
    assertEquals(arr.sumSIMD, 15L)

  test("sumSIMD - larger array"):
    val arr = Array.tabulate(100)(i => i.toLong)
    assertEquals(arr.sumSIMD, (0 until 100).sum.toLong)

  test("sumSIMD - negative values"):
    val arr = Array(-5L, 10L, -3L, 8L)
    assertEquals(arr.sumSIMD, 10L)

  test("select - basic indices"):
    val arr = Array(10L, 20L, 30L, 40L)
    val result = arr.select(Array(0, 2, 3))
    assertEquals(result.toSeq, Seq(10L, 30L, 40L))
end LongArraysSharedSuite
