package vecxt

import munit.FunSuite
import narr.*
import all.*
import BoundsCheck.DoBoundsCheck.yes

class RandSuite extends FunSuite:

  test("rand(rows, cols) creates matrix with correct dimensions") {
    val mat = Matrix.rand(3, 4)
    assertEquals(mat.rows, 3)
    assertEquals(mat.cols, 4)
    assertEquals(mat.raw.length, 12)
  }

  test("rand(dim: RowCol) creates matrix with correct dimensions") {
    val mat = Matrix.rand((5, 2))
    assertEquals(mat.rows, 5)
    assertEquals(mat.cols, 2)
    assertEquals(mat.raw.length, 10)
  }

  test("rand creates Double values between 0.0 and 1.0") {
    val mat = Matrix.rand(10, 10)
    var i = 0
    while i < mat.raw.length do
      assert(mat.raw(i) >= 0.0, s"Value ${mat.raw(i)} at index $i is less than 0.0")
      assert(mat.raw(i) < 1.0, s"Value ${mat.raw(i)} at index $i is not less than 1.0")
      i += 1
    end while
  }

  test("rand creates different values (not all same)") {
    val mat = Matrix.rand(5, 5)
    val firstValue = mat.raw(0)
    var allSame = true
    var i = 1
    while i < mat.raw.length do
      if mat.raw(i) != firstValue then allSame = false
      end if
      i += 1
    end while
    assert(!allSame, "All random values are the same, which is highly unlikely")
  }

  test("randInt(rows, cols) creates matrix with correct dimensions") {
    val mat = Matrix.randInt(3, 4)
    assertEquals(mat.rows, 3)
    assertEquals(mat.cols, 4)
    assertEquals(mat.raw.length, 12)
  }

  test("randInt(dim: RowCol) creates matrix with correct dimensions") {
    val mat = Matrix.randInt((5, 2))
    assertEquals(mat.rows, 5)
    assertEquals(mat.cols, 2)
    assertEquals(mat.raw.length, 10)
  }

  test("randInt without bounds creates values between 0 and 100") {
    val mat = Matrix.randInt(10, 10)
    var i = 0
    while i < mat.raw.length do
      assert(mat.raw(i) >= 0, s"Value ${mat.raw(i)} at index $i is less than 0")
      assert(mat.raw(i) < 100, s"Value ${mat.raw(i)} at index $i is not less than 100")
      i += 1
    end while
  }

  test("randInt with custom bounds creates values in range") {
    val minVal = 50
    val maxVal = 150
    val mat = Matrix.randInt(10, 10, minVal, maxVal)
    var i = 0
    while i < mat.raw.length do
      assert(
        mat.raw(i) >= minVal,
        s"Value ${mat.raw(i)} at index $i is less than $minVal"
      )
      assert(
        mat.raw(i) < maxVal,
        s"Value ${mat.raw(i)} at index $i is not less than $maxVal"
      )
      i += 1
    end while
  }

  test("randInt with dim and custom bounds creates values in range") {
    val minVal = -50
    val maxVal = 50
    val mat = Matrix.randInt((8, 8), minVal, maxVal)
    var i = 0
    while i < mat.raw.length do
      assert(
        mat.raw(i) >= minVal,
        s"Value ${mat.raw(i)} at index $i is less than $minVal"
      )
      assert(
        mat.raw(i) < maxVal,
        s"Value ${mat.raw(i)} at index $i is not less than $maxVal"
      )
      i += 1
    end while
  }

  test("randInt creates different values (not all same)") {
    val mat = Matrix.randInt(5, 5, 0, 1000)
    val firstValue = mat.raw(0)
    var allSame = true
    var i = 1
    while i < mat.raw.length do
      if mat.raw(i) != firstValue then allSame = false
      end if
      i += 1
    end while
    assert(!allSame, "All random integer values are the same, which is highly unlikely")
  }

  test("randInt with negative range works correctly") {
    val mat = Matrix.randInt(10, 10, -100, -50)
    var i = 0
    while i < mat.raw.length do
      assert(mat.raw(i) >= -100, s"Value ${mat.raw(i)} at index $i is less than -100")
      assert(mat.raw(i) < -50, s"Value ${mat.raw(i)} at index $i is not less than -50")
      i += 1
    end while
  }

  test("rand creates matrices with simple contiguous memory layout") {
    val mat = Matrix.rand(4, 4)
    assert(mat.hasSimpleContiguousMemoryLayout)
    assert(mat.isDenseColMajor)
  }

  test("randInt creates matrices with simple contiguous memory layout") {
    val mat = Matrix.randInt(4, 4)
    assert(mat.hasSimpleContiguousMemoryLayout)
    assert(mat.isDenseColMajor)
  }

end RandSuite
