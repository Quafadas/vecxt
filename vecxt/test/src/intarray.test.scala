package vecxt

import all.*

class IntArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  lazy val v_fill = Array[Int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  test("-") {
    val v1 = Array.tabulate[Int](21)(i => i)
    val v2 = Array.tabulate[Int](21)(i => i - 1)
    assertVecEquals((v2 - v1), Array.fill[Int](v1.length)(-1))
  }

  test("+") {
    val v2 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val v1 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assertVecEquals((v2 + v1), Array(2, 4, 6, 8, 10, 12, 14, 16, 18))
  }

  test("dot") {
    val v1 = Array.tabulate[Int](10)(i => i)
    val v2 = Array.tabulate[Int](10)(i => i)
    assertEquals(v1.dot(v2), 285)
  }

  test("sum") {
    val v1 = Array.tabulate[Int](10)(i => i)

    assertEquals(v1.sum, 45)
  }

  test("increments") {

    val v1 = Array[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    v1.increments.foreach(d => assertEquals(d, 1))
    val v2 = Array[Int](0, 2)
    assertVecEquals(v2.increments, v2)

    val v3 = Array[Int](45, 47, 48, 51, 54, 56, 54)
    assertVecEquals(v3.increments, Array(45, 2, 1, 3, 3, 2, -2))
  }

  test("logical comparisons") {
    val check = v_fill < 5
    assertVecEquals(check, Array(true, true, true, true, true, false, false, false, false, false))

    val checkGt = v_fill > 5
    assertVecEquals(checkGt, Array(false, false, false, false, false, false, true, true, true, true))

    val checkGte = v_fill >= 5
    assertVecEquals(checkGte, Array(false, false, false, false, false, true, true, true, true, true))

    val checkLte = v_fill <= 5
    assertVecEquals(checkLte, Array(true, true, true, true, true, true, false, false, false, false))

  }

  test("contiguous") {
    val v1 = Array.tabulate[Int](10)(i => i)
    assert(v1.contiguous)

    val v2 = Array(0, 2)
    assert(!v2.contiguous)
  }

end IntArrayExtensionSuite
