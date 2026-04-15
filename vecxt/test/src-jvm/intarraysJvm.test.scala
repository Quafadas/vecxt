package vecxt

import jdk.incubator.vector.DoubleVector
import BoundsCheck.DoBoundsCheck.no
import vecxt.all.*

class IntArraysJvmSuite extends munit.FunSuite:

  test("int Array / scalar covers full SIMD blocks on JVM") {
    val n = DoubleVector.SPECIES_PREFERRED.length() * 3 + 1
    val values = Array.tabulate(n)(i => (i + 1) * 3)
    val expected = Array.tabulate(n)(i => i + 1.0)
    val actual = values / 3.0

    var i = 0
    while i < n do
      assertEqualsDouble(actual(i), expected(i), 1e-12, clue = s"at index $i")
      i += 1
    end while
  }

  test("*:*= on dense Int matrix zeroes elements where mask is false") {
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 2, 3),
      Array[Int](4, 5, 6)
    )
    val mask = Matrix.fromRows[Boolean](
      Array[Boolean](true, false, true),
      Array[Boolean](false, true, false)
    )
    mat *:*= mask
    assertEquals(mat(0, 0), 1, "mat(0,0)")
    assertEquals(mat(0, 1), 0, "mat(0,1)")
    assertEquals(mat(0, 2), 3, "mat(0,2)")
    assertEquals(mat(1, 0), 0, "mat(1,0)")
    assertEquals(mat(1, 1), 5, "mat(1,1)")
    assertEquals(mat(1, 2), 0, "mat(1,2)")
  }

  test("*:* on dense Int matrix returns correct Matrix[Int]") {
    val mat = Matrix.fromRows[Int](
      Array[Int](1, 2, 3),
      Array[Int](4, 5, 6)
    )
    val mask = Matrix.fromRows[Boolean](
      Array[Boolean](true, false, true),
      Array[Boolean](false, true, false)
    )
    val result = mat *:* mask
    assertEquals(result(0, 0), 1, "r(0,0)")
    assertEquals(result(0, 1), 0, "r(0,1)")
    assertEquals(result(0, 2), 3, "r(0,2)")
    assertEquals(result(1, 0), 0, "r(1,0)")
    assertEquals(result(1, 1), 5, "r(1,1)")
    assertEquals(result(1, 2), 0, "r(1,2)")
    assertEquals(mat(0, 1), 2, "original unchanged")
  }

  test("*:* on offset Int view uses general layout path") {
    val base = Matrix.fromRows[Int](
      Array[Int](1, 2, 3, 4),
      Array[Int](5, 6, 7, 8),
      Array[Int](9, 10, 11, 12)
    )
    val sub = base(Range.Inclusive(0, 2, 1), Range.Inclusive(1, 2, 1))
    val msk = Matrix.fromRows[Boolean](
      Array[Boolean](false, true),
      Array[Boolean](true, false),
      Array[Boolean](false, true)
    )
    val result = sub *:* msk
    assertEquals(result(0, 0), 0, "r(0,0)")
    assertEquals(result(0, 1), 3, "r(0,1)")
    assertEquals(result(1, 0), 6, "r(1,0)")
    assertEquals(result(1, 1), 0, "r(1,1)")
    assertEquals(result(2, 0), 0, "r(2,0)")
    assertEquals(result(2, 1), 11, "r(2,1)")
  }

end IntArraysJvmSuite
