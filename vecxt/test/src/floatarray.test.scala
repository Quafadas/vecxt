package vecxt

import scala.util.chaining.*
import all.*

class FloatArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  private val tolerance = 1e-4f

  private def assertFloatVecEquals(v1: Array[Float], v2: Array[Float])(implicit loc: munit.Location): Unit =
    assert(v1.length == v2.length, s"lengths differ: ${v1.length} != ${v2.length}")
    var i = 0
    while i < v1.length do
      assertEqualsDouble(v1(i).toDouble, v2(i).toDouble, tolerance.toDouble, clue = s"at index $i")
      i += 1
    end while
  end assertFloatVecEquals

  // ===== Basic operations =====

  test("float sum") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    assertEqualsDouble(v1.sum.toDouble, 6.0, tolerance.toDouble)

    val v2 = Array[Float](1.0f, 2.0f, 3.0f, 1.0f, 2.0f, 3.0f, 1.0f, 2.0f, 3.0f)
    assertEqualsDouble(v2.sum.toDouble, 18.0, tolerance.toDouble)
  }

  test("float product") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    assertEqualsDouble(v1.product.toDouble, 6.0, tolerance.toDouble)

    val v2 = Array[Float](1.0f, 2.0f, 3.0f, 1.0f, 2.0f, 3.0f, 1.0f, 2.0f, 3.0f)
    assertEqualsDouble(v2.product.toDouble, 216.0, tolerance.toDouble)
  }

  test("float mean") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    assertEqualsDouble(v1.mean.toDouble, 3.0, tolerance.toDouble)
  }

  test("float variance") {
    val v1 = Array[Float](2.0f, 4.0f, 4.0f, 4.0f, 5.0f, 5.0f, 7.0f, 9.0f)
    // Population variance = 4.0
    assertEqualsDouble(v1.variance.toDouble, 4.0, 0.01)
    // Sample variance = 32/7
    assertEqualsDouble(v1.variance(VarianceMode.Sample).toDouble, 32.0 / 7.0, 0.01)
  }

  test("float std") {
    val v1 = Array[Float](2.0f, 4.0f, 4.0f, 4.0f, 5.0f, 5.0f, 7.0f, 9.0f)
    assertEqualsDouble(v1.std.toDouble, 2.0, 0.01)
  }

  // ===== Scalar arithmetic =====

  test("float Array / scalar") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = v1 / 2.0f
    assertEqualsDouble(v2(0).toDouble, 0.5, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 1.5, tolerance.toDouble)
  }

  test("float Array *= scalar") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    v1 *= 2.0f
    assertEqualsDouble(v1(0).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(v1(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v1(2).toDouble, 6.0, tolerance.toDouble)
  }

  test("float Array * scalar") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = v1 * 2.0f
    assertEqualsDouble(v2(0).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 6.0, tolerance.toDouble)
  }

  test("float Array + scalar") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = v1 + 1.0f
    assertEqualsDouble(v2(0).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 4.0, tolerance.toDouble)
  }

  test("float Array - scalar") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = v1 - 1.0f
    assertEqualsDouble(v2(0).toDouble, 0.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 2.0, tolerance.toDouble)
  }

  test("float scalar / Array") {
    val v1 = Array[Float](1.0f, 2.0f, 4.0f)
    val v2 = 4.0f / v1
    assertEqualsDouble(v2(0).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 1.0, tolerance.toDouble)
  }

  test("float scalar - Array") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = 5.0f - v1
    assertEqualsDouble(v2(0).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 2.0, tolerance.toDouble)
  }

  test("float scalar * Array") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = 2.0f * v1
    assertEqualsDouble(v2(0).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 6.0, tolerance.toDouble)
  }

  test("float scalar + Array") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = 1.0f + v1
    assertFloatVecEquals(v2, Array(v1.map(1.0f + _).toArray*))
  }

  // ===== Array-Array arithmetic =====

  test("float Array + Array") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = Array[Float](3.0f, 2.0f, 1.0f)
    val v3 = v1 + v2
    assertEqualsDouble(v3(0).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v3(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v3(2).toDouble, 4.0, tolerance.toDouble)
  }

  test("float Array += Array") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    v1 += Array[Float](3.0f, 2.0f, 1.0f)
    assertEqualsDouble(v1(0).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v1(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v1(2).toDouble, 4.0, tolerance.toDouble)
  }

  test("float Array - Array") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = Array[Float](3.0f, 2.0f, 1.0f)
    val v3 = v1 - v2
    assertEqualsDouble(v3(0).toDouble, -2.0, tolerance.toDouble)
    assertEqualsDouble(v3(1).toDouble, 0.0, tolerance.toDouble)
    assertEqualsDouble(v3(2).toDouble, 2.0, tolerance.toDouble)
  }

  test("float Array -= Array") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f)
    v1 -= Array[Float](3.0f, 2.0f, 1.0f, 0.0f, 0.0f, 0.0f)
    assertEqualsDouble(v1(0).toDouble, -2.0, tolerance.toDouble)
    assertEqualsDouble(v1(1).toDouble, 0.0, tolerance.toDouble)
    assertEqualsDouble(v1(2).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(v1(5).toDouble, 6.0, tolerance.toDouble)
  }

  test("float Array * Array elementwise") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val v2 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val v3 = v1 * v2
    assertEqualsDouble(v3(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(v3(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v3(2).toDouble, 9.0, tolerance.toDouble)
    assertEqualsDouble(v3(3).toDouble, 16.0, tolerance.toDouble)
    assertEqualsDouble(v3(4).toDouble, 25.0, tolerance.toDouble)
  }

  test("float Array / Array elementwise") {
    val v1 = Array[Float](1.0f, 4.0f, 9.0f)
    val v2 = Array[Float](1.0f, 2.0f, 3.0f)
    val v3 = v1 / v2
    assertEqualsDouble(v3(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(v3(1).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(v3(2).toDouble, 3.0, tolerance.toDouble)
  }

  test("float unary negation") {
    val v1 = Array[Float](1.0f, 2.0f, -3.0f)
    val v2 = -v1
    assertEqualsDouble(v2(0).toDouble, -1.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, -2.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 3.0, tolerance.toDouble)
  }

  // ===== Math operations =====

  test("float unary ops") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)

    val expResult = v1.exp
    for i <- 0 until v1.length do
      assertEqualsDouble(expResult(i).toDouble, Math.exp(v1(i).toDouble), 1e-4)

    val logResult = v1.log
    for i <- 0 until v1.length do
      assertEqualsDouble(logResult(i).toDouble, Math.log(v1(i).toDouble), 1e-4)

    val sqrtResult = v1.sqrt
    for i <- 0 until v1.length do
      assertEqualsDouble(sqrtResult(i).toDouble, Math.sqrt(v1(i).toDouble), 1e-4)
  }

  test("float sin/cos") {
    val v1 = Array[Float](0.0f, 0.5f, 1.0f)

    val sinResult = v1.sin
    for i <- 0 until v1.length do
      assertEqualsDouble(sinResult(i).toDouble, Math.sin(v1(i).toDouble), 1e-4)

    val cosResult = v1.cos
    for i <- 0 until v1.length do
      assertEqualsDouble(cosResult(i).toDouble, Math.cos(v1(i).toDouble), 1e-4)
  }

  test("float abs") {
    val v1 = Array[Float](-1.0f, 2.0f, -3.0f, 4.0f, -5.0f)
    val result = v1.abs
    assertEqualsDouble(result(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(3).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(result(4).toDouble, 5.0, tolerance.toDouble)

    // In-place
    v1.`abs!`
    assertEqualsDouble(v1(0).toDouble, 1.0, tolerance.toDouble)
  }

  test("float fma") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val result = v1.fma(2.0f, 1.0f)
    assertEqualsDouble(result(0).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 5.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 7.0, tolerance.toDouble)
    assertEqualsDouble(result(3).toDouble, 9.0, tolerance.toDouble)
    assertEqualsDouble(result(4).toDouble, 11.0, tolerance.toDouble)

    // Original unchanged
    assertEqualsDouble(v1(0).toDouble, 1.0, tolerance.toDouble)

    // In-place
    v1.`fma!`(2.0f, 1.0f)
    assertEqualsDouble(v1(3).toDouble, 9.0, tolerance.toDouble)
  }

  test("float power") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f)
    val result = v1 ** 2.0f
    assertEqualsDouble(result(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 9.0, tolerance.toDouble)
    assertEqualsDouble(result(3).toDouble, 16.0, tolerance.toDouble)
  }

  // ===== Clamping =====

  test("float clampMin") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val result = v1.clampMin(3.0f)
    assertEqualsDouble(result(0).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(3).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(result(4).toDouble, 5.0, tolerance.toDouble)
  }

  test("float clampMax") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val result = v1.clampMax(3.0f)
    assertEqualsDouble(result(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(3).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(4).toDouble, 3.0, tolerance.toDouble)
  }

  test("float clamp range") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val result = v1.clamp(2.0f, 4.0f)
    assertEqualsDouble(result(0).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(3).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(result(4).toDouble, 4.0, tolerance.toDouble)
  }

  // ===== Min/Max/ArgMax/ArgMin =====

  test("float argmax") {
    val v1 = Array[Float](1.0f, 5.0f, 3.0f, 2.0f, 4.0f)
    assertEquals(v1.argmax, 1)

    val empty = Array[Float]()
    assertEquals(empty.argmax, -1)
  }

  test("float argmin") {
    val v1 = Array[Float](1.0f, 5.0f, 3.0f, 0.5f, 4.0f)
    assertEquals(v1.argmin, 3)

    val empty = Array[Float]()
    assertEquals(empty.argmin, -1)
  }

  test("float min/max") {
    val v1 = Array[Float](3.0f, 1.0f, 4.0f, 1.0f, 5.0f, 9.0f, 2.0f, 6.0f)
    assertEqualsDouble(v1.min.toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(v1.max.toDouble, 9.0, tolerance.toDouble)
  }

  // ===== Linear algebra =====

  test("float dot product") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = Array[Float](4.0f, 5.0f, 6.0f)
    assertEqualsDouble(v1.dot(v2).toDouble, 32.0, tolerance.toDouble)
  }

  test("float norm") {
    val v1 = Array[Float](3.0f, 4.0f)
    assertEqualsDouble(v1.norm.toDouble, 5.0, tolerance.toDouble)
  }

  test("float outer product") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = Array[Float](4.0f, 5.0f)
    import BoundsCheck.DoBoundsCheck.no
    val m = v1.outer(v2)
    assertEquals(m.rows, 3)
    assertEquals(m.cols, 2)
    assertEqualsDouble(m(0, 0).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(m(1, 0).toDouble, 8.0, tolerance.toDouble)
    assertEqualsDouble(m(2, 0).toDouble, 12.0, tolerance.toDouble)
    assertEqualsDouble(m(0, 1).toDouble, 5.0, tolerance.toDouble)
    assertEqualsDouble(m(1, 1).toDouble, 10.0, tolerance.toDouble)
    assertEqualsDouble(m(2, 1).toDouble, 15.0, tolerance.toDouble)
  }

  // ===== Cumulative/Incremental =====

  test("float cumsum") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f).cumsum
    assertEqualsDouble(v1(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(v1(1).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(v1(2).toDouble, 6.0, tolerance.toDouble)
  }

  test("float increments") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val inc = v1.increments
    assertEqualsDouble(inc(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(inc(1).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(inc(2).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(inc(3).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(inc(4).toDouble, 1.0, tolerance.toDouble)
  }

  // ===== Comparisons =====

  test("float comparisons") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)

    val lt = v1 < 3.0f
    assert(lt(0))
    assert(lt(1))
    assert(!lt(2))
    assert(!lt(3))
    assert(!lt(4))

    val gt = v1 > 3.0f
    assert(!gt(0))
    assert(!gt(1))
    assert(!gt(2))
    assert(gt(3))
    assert(gt(4))

    val lte = v1 <= 3.0f
    assert(lte(0))
    assert(lte(1))
    assert(lte(2))
    assert(!lte(3))
    assert(!lte(4))

    val gte = v1 >= 3.0f
    assert(!gte(0))
    assert(!gte(1))
    assert(gte(2))
    assert(gte(3))
    assert(gte(4))
  }

  // ===== Select/Unique/Argsort =====

  test("float select") {
    val v1 = Array[Float](10.0f, 20.0f, 30.0f, 40.0f, 50.0f)
    val indices = Array[Int](0, 2, 4)
    val result = v1.select(indices)
    assertEquals(result.length, 3)
    assertEqualsDouble(result(0).toDouble, 10.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 30.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 50.0, tolerance.toDouble)
  }

  test("float unique") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 2.0f, 1.0f, 4.0f)
    val result = v1.unique
    assertEquals(result.length, 4)
    assertEqualsDouble(result(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 2.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(3).toDouble, 4.0, tolerance.toDouble)

    val empty = Array[Float]()
    assertEquals(empty.unique.length, 0)
  }

  test("float argsort") {
    val v1 = Array[Float](3.0f, 1.0f, 4.0f, 1.0f, 5.0f)
    val idx = v1.argsort
    assertEquals(idx.length, 5)
    // Verify sorted order
    for i <- 0 until idx.length - 1 do
      assert(v1(idx(i)) <= v1(idx(i + 1)), s"argsort failed at $i: ${v1(idx(i))} > ${v1(idx(i + 1))}")
    end for
  }

  // ===== Statistics =====

  test("float productExceptSelf") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f)
    val v2 = v1.productExceptSelf
    assertEqualsDouble(v2(0).toDouble, 24.0, tolerance.toDouble)
    assertEqualsDouble(v2(1).toDouble, 12.0, tolerance.toDouble)
    assertEqualsDouble(v2(2).toDouble, 8.0, tolerance.toDouble)
    assertEqualsDouble(v2(3).toDouble, 6.0, tolerance.toDouble)
  }

  test("float logSumExp") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val expected = Math.log(Math.exp(1) + Math.exp(2) + Math.exp(3) + Math.exp(4) + Math.exp(5))
    assertEqualsDouble(v1.logSumExp.toDouble, expected, 1e-4)
  }

  test("float +:+") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f) +:+ 2.0f
    assertEqualsDouble(v1(0).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(v1(1).toDouble, 4.0, tolerance.toDouble)
    assertEqualsDouble(v1(2).toDouble, 5.0, tolerance.toDouble)
    assertEqualsDouble(v1(3).toDouble, 6.0, tolerance.toDouble)
    assertEqualsDouble(v1(4).toDouble, 7.0, tolerance.toDouble)
  }

  test("float pearsonCorrelationCoefficient") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val v2 = Array[Float](2.0f, 4.0f, 6.0f, 8.0f, 10.0f)
    assertEqualsDouble(v1.pearsonCorrelationCoefficient(v2).toDouble, 1.0, 1e-4)

    val v3 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val v4 = Array[Float](5.0f, 4.0f, 3.0f, 2.0f, 1.0f)
    assertEqualsDouble(v3.pearsonCorrelationCoefficient(v4).toDouble, -1.0, 1e-4)
  }

  test("float covariance") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val v2 = Array[Float](2.0f, 4.0f, 6.0f, 8.0f, 10.0f)
    // Covariance of perfectly correlated data: sample cov should be 5.0
    assert(v1.covariance(v2) > 0, "covariance of positively correlated data should be positive")
  }

  test("float elementRanks") {
    val v1 = Array[Float](3.0f, 1.0f, 4.0f, 1.0f, 5.0f)
    val ranks = v1.elementRanks
    assertEquals(ranks.length, 5)
    // Verify that ties get average rank
    assertEqualsDouble(ranks(1).toDouble, ranks(3).toDouble, tolerance.toDouble)
  }

  test("float boolean indexing (mask)") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val mask = Array[Boolean](true, false, true, false, true)
    val result = v1.mask(mask)
    assertEquals(result.length, 3)
    assertEqualsDouble(result(0).toDouble, 1.0, tolerance.toDouble)
    assertEqualsDouble(result(1).toDouble, 3.0, tolerance.toDouble)
    assertEqualsDouble(result(2).toDouble, 5.0, tolerance.toDouble)
  }

  test("float in-place operations") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)

    // exp!
    val v2 = v1.clone()
    v2.`exp!`
    for i <- 0 until v1.length do assertEqualsDouble(v2(i).toDouble, Math.exp(v1(i).toDouble), 1e-4)

    // log!
    val v3 = v1.clone()
    v3.`log!`
    for i <- 0 until v1.length do assertEqualsDouble(v3(i).toDouble, Math.log(v1(i).toDouble), 1e-4)

    // sqrt!
    val v4 = v1.clone()
    v4.`sqrt!`
    for i <- 0 until v1.length do assertEqualsDouble(v4(i).toDouble, Math.sqrt(v1(i).toDouble), 1e-4)

    // -!
    val v5 = v1.clone()
    v5.`-!`
    for i <- 0 until v1.length do assertEqualsDouble(v5(i).toDouble, -v1(i).toDouble, tolerance.toDouble)
  }

  test("float meanAndVariance") {
    val v1 = Array[Float](2.0f, 4.0f, 4.0f, 4.0f, 5.0f, 5.0f, 7.0f, 9.0f)
    val (m, v) = v1.meanAndVariance
    assertEqualsDouble(m.toDouble, 5.0, 0.01)
    assertEqualsDouble(v.toDouble, 4.0, 0.01)

    val (ms, vs) = v1.meanAndVariance(VarianceMode.Sample)
    assertEqualsDouble(ms.toDouble, 5.0, 0.01)
    assertEqualsDouble(vs.toDouble, 32.0 / 7.0, 0.01)
  }

  test("float spearmansRankCorrelation") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val v2 = Array[Float](5.0f, 4.0f, 3.0f, 2.0f, 1.0f)
    assertEqualsDouble(v1.spearmansRankCorrelation(v2).toDouble, -1.0, 1e-4)
  }

  test("float operator precedence") {
    val v1 = Array[Float](1.0f, 2.0f, 3.0f)
    val v2 = Array[Float](3.0f, 2.0f, 1.0f)

    val v3 = v1 + v2 * 2.0f
    val v4 = v2 * 2.0f + v1

    assertEqualsDouble(v3(0).toDouble, 7.0, tolerance.toDouble)
    assertEqualsDouble(v4(0).toDouble, 7.0, tolerance.toDouble)
  }

  test("float large array operations") {
    val size = 1000
    val v1 = Array.tabulate[Float](size)(i => (i + 1).toFloat)
    val v2 = Array.tabulate[Float](size)(i => (i + 1).toFloat)

    // sum of 1..1000 = 500500
    assertEqualsDouble(v1.sum.toDouble, 500500.0, 1.0)

    // dot product
    val dotExpected = (1 to size).map(i => i.toFloat * i.toFloat).sum.toDouble
    assertEqualsDouble(v1.dot(v2).toDouble, dotExpected, size.toDouble)

    // comparisons
    val ltResult = v1 < 500.0f
    var trueCount = 0
    for b <- ltResult do if b then trueCount += 1
    assertEquals(trueCount, 499)
  }

end FloatArrayExtensionSuite
