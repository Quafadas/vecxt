package vecxt

import all.*

class IntScalarOpsSuite extends munit.FunSuite:

  test("in-place subtraction -= scalar works and mutates array"):
    val arr = Array(5, 3, 8)
    arr -= 2
    assertEquals(arr.toSeq, Seq(3, 1, 6))

  test("non-mutating - scalar returns new array and leaves original unchanged"):
    val orig = Array(10, 0, -5)
    val out = orig - 3
    assertEquals(out.toSeq, Seq(7, -3, -8))
    assertEquals(orig.toSeq, Seq(10, 0, -5))

  test("subtracting zero does nothing"):
    val a = Array(1, 2, 3)
    val b = a.clone()
    a -= 0
    assertEquals(a.toSeq, b.toSeq)
    val c = b - 0
    assertEquals(c.toSeq, b.toSeq)

  test("works on empty arrays"):
    val e = Array.empty[Int]
    e -= 5
    assertEquals(e.toSeq, Seq())
    val e2 = e - 5
    assertEquals(e2.toSeq, Seq())

end IntScalarOpsSuite
