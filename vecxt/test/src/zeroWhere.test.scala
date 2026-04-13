package vecxt

import scala.util.chaining.*
import all.*

class ZeroWhereSuite extends munit.FunSuite:

  // ===== Float tests =====

  test("zeroWhere! zeros elements where other <= threshold (Float)") {
    val vec = Array[Float](1.0f, 2.0f, 3.0f, 4.0f, 5.0f)
    val other = Array[Float](0.0f, -1.0f, 1.0f, 0.0f, 2.0f)
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LE)
    assertEquals(vec(0), 0.0f)
    assertEquals(vec(1), 0.0f)
    assertEquals(vec(2), 3.0f)
    assertEquals(vec(3), 0.0f)
    assertEquals(vec(4), 5.0f)
  }

  test("zeroWhere returns new array without mutating source (Float)") {
    val vec = Array[Float](1.0f, 2.0f, 3.0f)
    val other = Array[Float](-1.0f, 1.0f, -1.0f)
    val result = vec.zeroWhere(other, 0.0f, ComparisonOp.LE)
    assertEquals(result(0), 0.0f)
    assertEquals(result(1), 2.0f)
    assertEquals(result(2), 0.0f)
    // source unchanged
    assertEquals(vec(0), 1.0f)
    assertEquals(vec(1), 2.0f)
    assertEquals(vec(2), 3.0f)
  }

  test("zeroWhere! respects all ComparisonOp variants (Float)") {
    val other = Array[Float](1.0f, 2.0f, 3.0f)

    val vecLT = Array[Float](10.0f, 20.0f, 30.0f)
    vecLT.`zeroWhere!`(other, 2.0f, ComparisonOp.LT)
    assertEquals(vecLT(0), 0.0f, "LT index 0")
    assertEquals(vecLT(1), 20.0f, "LT index 1")
    assertEquals(vecLT(2), 30.0f, "LT index 2")

    val vecLE = Array[Float](10.0f, 20.0f, 30.0f)
    vecLE.`zeroWhere!`(other, 2.0f, ComparisonOp.LE)
    assertEquals(vecLE(0), 0.0f, "LE index 0")
    assertEquals(vecLE(1), 0.0f, "LE index 1")
    assertEquals(vecLE(2), 30.0f, "LE index 2")

    val vecGT = Array[Float](10.0f, 20.0f, 30.0f)
    vecGT.`zeroWhere!`(other, 2.0f, ComparisonOp.GT)
    assertEquals(vecGT(0), 10.0f, "GT index 0")
    assertEquals(vecGT(1), 20.0f, "GT index 1")
    assertEquals(vecGT(2), 0.0f, "GT index 2")

    val vecGE = Array[Float](10.0f, 20.0f, 30.0f)
    vecGE.`zeroWhere!`(other, 2.0f, ComparisonOp.GE)
    assertEquals(vecGE(0), 10.0f, "GE index 0")
    assertEquals(vecGE(1), 0.0f, "GE index 1")
    assertEquals(vecGE(2), 0.0f, "GE index 2")

    val vecEQ = Array[Float](10.0f, 20.0f, 30.0f)
    vecEQ.`zeroWhere!`(other, 2.0f, ComparisonOp.EQ)
    assertEquals(vecEQ(0), 10.0f, "EQ index 0")
    assertEquals(vecEQ(1), 0.0f, "EQ index 1")
    assertEquals(vecEQ(2), 30.0f, "EQ index 2")

    val vecNE = Array[Float](10.0f, 20.0f, 30.0f)
    vecNE.`zeroWhere!`(other, 2.0f, ComparisonOp.NE)
    assertEquals(vecNE(0), 0.0f, "NE index 0")
    assertEquals(vecNE(1), 20.0f, "NE index 1")
    assertEquals(vecNE(2), 0.0f, "NE index 2")
  }

  test("zeroWhere! handles non-SIMD-aligned lengths (Float)") {
    val n = 19
    val vec = Array.tabulate[Float](n)(i => (i + 1).toFloat)
    val other = Array.tabulate[Float](n)(i => if i % 3 == 0 then -1.0f else 1.0f)
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LE)
    var i = 0
    while i < n do
      if i % 3 == 0 then assertEquals(vec(i), 0.0f, s"index $i should be zeroed")
      else assertEquals(vec(i), (i + 1).toFloat, s"index $i should be kept")
      end if
      i += 1
    end while
  }

  test("zeroWhere! on empty arrays is a no-op (Float)") {
    val vec = Array.empty[Float]
    val other = Array.empty[Float]
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LE)
    assertEquals(vec.length, 0)
  }

  test("zeroWhere! zeros all elements when all satisfy condition (Float)") {
    val vec = Array[Float](1.0f, 2.0f, 3.0f)
    val other = Array[Float](-1.0f, -2.0f, -3.0f)
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LT)
    assertEquals(vec(0), 0.0f)
    assertEquals(vec(1), 0.0f)
    assertEquals(vec(2), 0.0f)
  }

  test("zeroWhere! keeps all elements when none satisfy condition (Float)") {
    val vec = Array[Float](1.0f, 2.0f, 3.0f)
    val other = Array[Float](10.0f, 20.0f, 30.0f)
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LT)
    assertEquals(vec(0), 1.0f)
    assertEquals(vec(1), 2.0f)
    assertEquals(vec(2), 3.0f)
  }

  test("zeroWhere! handles NaN in other array (Float)") {
    val vec = Array[Float](1.0f, 2.0f, 3.0f)
    val other = Array[Float](Float.NaN, 1.0f, Float.NaN)
    // NaN comparisons are always false (IEEE 754)
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LE)
    assertEquals(vec(0), 1.0f)
    assertEquals(vec(1), 2.0f)
    assertEquals(vec(2), 3.0f)
  }

  test("zeroWhere! handles threshold at Float boundaries") {
    val vec = Array[Float](1.0f, 2.0f)
    val other = Array[Float](Float.NegativeInfinity, Float.PositiveInfinity)
    vec.`zeroWhere!`(other, 0.0f, ComparisonOp.LE)
    assertEquals(vec(0), 0.0f)
    assertEquals(vec(1), 2.0f)
  }

  test("zeroWhere! works when vec and other are the same array (Float)") {
    val arr = Array[Float](-2.0f, 3.0f, -1.0f, 4.0f)
    arr.`zeroWhere!`(arr, 0.0f, ComparisonOp.LT)
    assertEquals(arr(0), 0.0f)
    assertEquals(arr(1), 3.0f)
    assertEquals(arr(2), 0.0f)
    assertEquals(arr(3), 4.0f)
  }

  // ===== Double tests =====

  test("zeroWhere! zeros elements where other <= threshold (Double)") {
    val vec = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val other = Array[Double](0.0, -1.0, 1.0, 0.0, 2.0)
    vec.`zeroWhere!`(other, 0.0, ComparisonOp.LE)
    assertEquals(vec(0), 0.0)
    assertEquals(vec(1), 0.0)
    assertEquals(vec(2), 3.0)
    assertEquals(vec(3), 0.0)
    assertEquals(vec(4), 5.0)
  }

  test("zeroWhere returns new array without mutating source (Double)") {
    val vec = Array[Double](1.0, 2.0, 3.0)
    val other = Array[Double](-1.0, 1.0, -1.0)
    val result = vec.zeroWhere(other, 0.0, ComparisonOp.LE)
    assertEquals(result(0), 0.0)
    assertEquals(result(1), 2.0)
    assertEquals(result(2), 0.0)
    // source unchanged
    assertEquals(vec(0), 1.0)
    assertEquals(vec(1), 2.0)
    assertEquals(vec(2), 3.0)
  }

  test("zeroWhere! respects all ComparisonOp variants (Double)") {
    val other = Array[Double](1.0, 2.0, 3.0)

    val vecLT = Array[Double](10.0, 20.0, 30.0)
    vecLT.`zeroWhere!`(other, 2.0, ComparisonOp.LT)
    assertEquals(vecLT(0), 0.0, "LT index 0")
    assertEquals(vecLT(1), 20.0, "LT index 1")
    assertEquals(vecLT(2), 30.0, "LT index 2")

    val vecLE = Array[Double](10.0, 20.0, 30.0)
    vecLE.`zeroWhere!`(other, 2.0, ComparisonOp.LE)
    assertEquals(vecLE(0), 0.0, "LE index 0")
    assertEquals(vecLE(1), 0.0, "LE index 1")
    assertEquals(vecLE(2), 30.0, "LE index 2")

    val vecGT = Array[Double](10.0, 20.0, 30.0)
    vecGT.`zeroWhere!`(other, 2.0, ComparisonOp.GT)
    assertEquals(vecGT(0), 10.0, "GT index 0")
    assertEquals(vecGT(1), 20.0, "GT index 1")
    assertEquals(vecGT(2), 0.0, "GT index 2")

    val vecGE = Array[Double](10.0, 20.0, 30.0)
    vecGE.`zeroWhere!`(other, 2.0, ComparisonOp.GE)
    assertEquals(vecGE(0), 10.0, "GE index 0")
    assertEquals(vecGE(1), 0.0, "GE index 1")
    assertEquals(vecGE(2), 0.0, "GE index 2")

    val vecEQ = Array[Double](10.0, 20.0, 30.0)
    vecEQ.`zeroWhere!`(other, 2.0, ComparisonOp.EQ)
    assertEquals(vecEQ(0), 10.0, "EQ index 0")
    assertEquals(vecEQ(1), 0.0, "EQ index 1")
    assertEquals(vecEQ(2), 30.0, "EQ index 2")

    val vecNE = Array[Double](10.0, 20.0, 30.0)
    vecNE.`zeroWhere!`(other, 2.0, ComparisonOp.NE)
    assertEquals(vecNE(0), 0.0, "NE index 0")
    assertEquals(vecNE(1), 20.0, "NE index 1")
    assertEquals(vecNE(2), 0.0, "NE index 2")
  }

  test("zeroWhere! on empty arrays is a no-op (Double)") {
    val vec = Array.empty[Double]
    val other = Array.empty[Double]
    vec.`zeroWhere!`(other, 0.0, ComparisonOp.LE)
    assertEquals(vec.length, 0)
  }

  test("zeroWhere! handles NaN in other array (Double)") {
    val vec = Array[Double](1.0, 2.0, 3.0)
    val other = Array[Double](Double.NaN, 1.0, Double.NaN)
    // NaN comparisons are always false (IEEE 754)
    vec.`zeroWhere!`(other, 0.0, ComparisonOp.LE)
    assertEquals(vec(0), 1.0)
    assertEquals(vec(1), 2.0)
    assertEquals(vec(2), 3.0)
  }

  test("zeroWhere! handles threshold at Double boundaries") {
    val vec = Array[Double](1.0, 2.0)
    val other = Array[Double](Double.NegativeInfinity, Double.PositiveInfinity)
    vec.`zeroWhere!`(other, 0.0, ComparisonOp.LE)
    assertEquals(vec(0), 0.0)
    assertEquals(vec(1), 2.0)
  }

end ZeroWhereSuite
