package vecxt

import all.*

class ClampSuite extends munit.FunSuite:

  test("clamp") {
    val v1 = Array[Double](1.0, 5.0, 3.0, 8.0, 2.0)
    v1.`clamp!`(2.0, 4.0)
    assertEquals(v1.length, 5)
    assertEqualsDouble(v1(0), 2.0, 0.0001) // clamped from 1.0
    assertEqualsDouble(v1(1), 4.0, 0.0001) // clamped from 5.0
    assertEqualsDouble(v1(2), 3.0, 0.0001) // unchanged
    assertEqualsDouble(v1(3), 4.0, 0.0001) // clamped from 8.0
    assertEqualsDouble(v1(4), 2.0, 0.0001) // clamped from 2.0

  }

  test("clampMax should clamp values above the threshold") {
    val v1 = Array[Double](1.0, 5.0, 3.0, 8.0, 2.0)
    val clamped = v1.clampMax(4.0)

    assertEquals(clamped.length, 5)
    assertEqualsDouble(clamped(0), 1.0, 0.0001) // unchanged
    assertEqualsDouble(clamped(1), 4.0, 0.0001) // clamped from 5.0
    assertEqualsDouble(clamped(2), 3.0, 0.0001) // unchanged
    assertEqualsDouble(clamped(3), 4.0, 0.0001) // clamped from 8.0
    assertEqualsDouble(clamped(4), 2.0, 0.0001) // unchanged
  }

  test("clampMax should handle edge case with exact threshold values") {
    val v1 = Array[Double](3.0, 5.0, 5.0, 7.0)
    val clamped = v1.clampMax(5.0)

    assertEqualsDouble(clamped(0), 3.0, 0.0001) // unchanged
    assertEqualsDouble(clamped(1), 5.0, 0.0001) // unchanged (exactly at threshold)
    assertEqualsDouble(clamped(2), 5.0, 0.0001) // unchanged (exactly at threshold)
    assertEqualsDouble(clamped(3), 5.0, 0.0001) // clamped from 7.0
  }

  test("clampMin should clamp values below the threshold") {
    val v1 = Array[Double](1.0, 5.0, 3.0, 0.5, 8.0)
    val clamped = v1.clampMin(2.0)

    assertEquals(clamped.length, 5)
    assertEqualsDouble(clamped(0), 2.0, 0.0001) // clamped from 1.0
    assertEqualsDouble(clamped(1), 5.0, 0.0001) // unchanged
    assertEqualsDouble(clamped(2), 3.0, 0.0001) // unchanged
    assertEqualsDouble(clamped(3), 2.0, 0.0001) // clamped from 0.5
    assertEqualsDouble(clamped(4), 8.0, 0.0001) // unchanged
  }

  test("clampMin should handle edge case with exact threshold values") {
    val v1 = Array[Double](1.0, 3.0, 3.0, 5.0)
    val clamped = v1.clampMin(3.0)

    assertEqualsDouble(clamped(0), 3.0, 0.0001) // clamped from 1.0
    assertEqualsDouble(clamped(1), 3.0, 0.0001) // unchanged (exactly at threshold)
    assertEqualsDouble(clamped(2), 3.0, 0.0001) // unchanged (exactly at threshold)
    assertEqualsDouble(clamped(3), 5.0, 0.0001) // unchanged
  }

  test("clampMax should handle infinity values") {
    val v1 = Array[Double](1.0, Double.PositiveInfinity, 3.0, Double.NegativeInfinity)
    val clamped = v1.clampMax(5.0)

    assertEqualsDouble(clamped(0), 1.0, 0.0001)
    assertEqualsDouble(clamped(1), 5.0, 0.0001) // PositiveInfinity clamped to 5.0
    assertEqualsDouble(clamped(2), 3.0, 0.0001)
    assertEquals(clamped(3), Double.NegativeInfinity) // NegativeInfinity unchanged
  }

  test("clampMin should handle infinity values") {
    val v1 = Array[Double](1.0, Double.PositiveInfinity, 3.0, Double.NegativeInfinity)
    val clamped = v1.clampMin(2.0)

    assertEqualsDouble(clamped(0), 2.0, 0.0001) // clamped from 1.0
    assertEquals(clamped(1), Double.PositiveInfinity) // PositiveInfinity unchanged
    assertEqualsDouble(clamped(2), 3.0, 0.0001)
    assertEqualsDouble(clamped(3), 2.0, 0.0001) // NegativeInfinity clamped to 2.0
  }

  test("maxClamp alias should work identically to clampMax") {
    val v1 = Array[Double](1.0, 5.0, 3.0, 8.0)
    val clampedMax = v1.clampMax(4.0)
    val maxClamped = v1.maxClamp(4.0)

    assertVecEquals(clampedMax, maxClamped)
  }

  test("aliases should compiled") {
    val v1 = Array[Double](1.0, 5.0, 3.0, 0.5)
    assertVecEquals(v1.clampMax(2.0), v1.maxClamp(2.0))
    assertVecEquals(v1.clampMin(2.0), v1.minClamp(2.0))
  }

  test("empty array should work with both clamp methods") {
    val empty = Array[Double]()
    val clampedMax = empty.clampMax(5.0)
    val clampedMin = empty.clampMin(2.0)

    assertEquals(clampedMax.length, 0)
    assertEquals(clampedMin.length, 0)
  }

  test("single element array should work correctly") {
    val single = Array[Double](3.0)

    val clampedMax = single.clampMax(2.0)
    assertEqualsDouble(clampedMax(0), 2.0, 0.0001) // clamped down
  }
end ClampSuite
