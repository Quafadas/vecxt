package vecxt

import all.*

class ZeroWhereJvmSuite extends munit.FunSuite:

  test("zeroWhere! on large Float array matches scalar reference (exercises SIMD + tail)") {
    val n = 100_003
    val rng = new java.util.Random(42)
    val vec = Array.tabulate[Float](n)(_ => rng.nextFloat() * 10 - 5)
    val other = Array.tabulate[Float](n)(_ => rng.nextFloat() * 10 - 5)
    val expected = vec.clone()
    var i = 0
    while i < n do
      if other(i) <= 0.0f then expected(i) = 0.0f
      i += 1
    end while
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LE)
    var j = 0
    while j < n do
      assertEquals(vec(j), expected(j), s"mismatch at index $j")
      j += 1
    end while
  }

  test("zeroWhere! on large Double array matches scalar reference (exercises SIMD + tail)") {
    val n = 100_003
    val rng = new java.util.Random(42)
    val vec = Array.tabulate[Double](n)(_ => rng.nextDouble() * 10 - 5)
    val other = Array.tabulate[Double](n)(_ => rng.nextDouble() * 10 - 5)
    val expected = vec.clone()
    var i = 0
    while i < n do
      if other(i) <= 0.0 then expected(i) = 0.0
      i += 1
    end while
    vec.`zeroWhere!`(other, 0.0, ComparisonOp.LE)
    var j = 0
    while j < n do
      assertEquals(vec(j), expected(j), s"mismatch at index $j")
      j += 1
    end while
  }

end ZeroWhereJvmSuite
