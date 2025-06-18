package vecxt

import narr.*
import all.*

class IntArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  lazy val v_fill = NArray[Int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  test("-") {
    val v1 = NArray.tabulate[Int](21)(i => i)
    val v2 = NArray.tabulate[Int](21)(i => i - 1)
    assertVecEquals((v2 - v1), NArray.fill[Int](v1.length)(-1))
  }

  test("+") {
    val v2 = NArray(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val v1 = NArray(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assertVecEquals((v2 + v1), NArray(2, 4, 6, 8, 10, 12, 14, 16, 18))
  }

  test("dot") {
    val v1 = NArray.tabulate[Int](10)(i => i)
    val v2 = NArray.tabulate[Int](10)(i => i)
    assertEquals(v1.dot(v2), 285)
  }

  test("sum") {
    val v1 = NArray.tabulate[Int](10)(i => i)

    assertEquals(v1.sum, 45)
  }

  test("increments") {

    val v1 = NArray[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    v1.increments.foreach(d => assertEquals(d, 1))
    val v2 = NArray[Int](0, 2)
    assertVecEquals(v2.increments, v2)

    val v3 = NArray[Int](45, 47, 48, 51, 54, 56, 54)
    assertVecEquals(v3.increments, NArray(45, 2, 1, 3, 3, 2, -2))
  }

  test("logical comparisons") {
    val check = v_fill < 5
    assertVecEquals(check, NArray(true, true, true, true, true, false, false, false, false, false))

    val checkGt = v_fill > 5
    assertVecEquals(checkGt, NArray(false, false, false, false, false, false, true, true, true, true))

    val checkGte = v_fill >= 5
    assertVecEquals(checkGte, NArray(false, false, false, false, false, true, true, true, true, true))

    val checkLte = v_fill <= 5
    assertVecEquals(checkLte, NArray(true, true, true, true, true, true, false, false, false, false))

  }

  test("contiguous") {
    val v1 = NArray.tabulate[Int](10)(i => i)
    assert(v1.contiguous)

    val v2 = NArray(0, 2)
    println(v2.mkString(", "))
    assert(!v2.contiguous)
  }

end IntArrayExtensionSuite
