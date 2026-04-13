package vecxt

import all.*

import jdk.incubator.vector.DoubleVector

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

end IntArraysJvmSuite