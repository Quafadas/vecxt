
package vecxt

import narr.*
import scala.util.chaining.*
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

class ArgmaxSuite extends munit.FunSuite:

  test("argmax simple case") {
    val v1 = NArray[Double](1.0, 3.0, 2.0, 5.0, 4.0)
    assertEquals(v1.argmax, 3)
  }

  test("argmax first element is max") {
    val v1 = NArray[Double](5.0, 3.0, 2.0, 1.0, 4.0)
    assertEquals(v1.argmax, 0)
  }

  test("argmax last element is max") {
    val v1 = NArray[Double](1.0, 3.0, 2.0, 4.0, 5.0)
    assertEquals(v1.argmax, 4)
  }

  test("argmax with duplicates returns first occurrence") {
    val v1 = NArray[Double](1.0, 5.0, 3.0, 5.0, 2.0)
    assertEquals(v1.argmax, 1)
  }

  test("argmax single element") {
    val v1 = NArray[Double](42.0)
    assertEquals(v1.argmax, 0)
  }

  test("argmax with negative values") {
    val v1 = NArray[Double](-5.0, -1.0, -3.0, -2.0)
    assertEquals(v1.argmax, 1)
  }

  test("argmax with all equal values") {
    val v1 = NArray[Double](2.0, 2.0, 2.0, 2.0)
    assertEquals(v1.argmax, 0)
  }

  test("argmax with large array") {
    val n = 10000
    val v1 = NArray.tabulate(n)(i => math.sin(i.toDouble))
    val maxIdx = v1.argmax
    val maxValue = v1(maxIdx)
    
    // Verify no other element is larger
    var isCorrect = true
    var i = 0
    while i < n do
      if v1(i) > maxValue then isCorrect = false
      i += 1
    end while
    
    assert(isCorrect)
  }

  test("argmax with NaN handling") {
    // Assuming your implementation handles NaN in some specific way
    val v1 = NArray[Double](1.0, Double.NaN, 3.0, 2.0)
    val result = v1.argmax
    // This test depends on your NaN handling policy
    // Typically argmax should return index of non-NaN maximum
    assert(result == 2) // Depends on implementation
  }

  test("argmax with infinity") {
    val v1 = NArray[Double](1.0, Double.PositiveInfinity, 3.0, 2.0)
    assertEquals(v1.argmax, 1)
    
    val v2 = NArray[Double](Double.NegativeInfinity, 1.0, 3.0, 2.0)
    assertEquals(v2.argmax, 2)
  }

  test("argmax consistency with manual search") {
    val v1 = NArray[Double](3.14, 2.71, 1.41, 1.73, 0.57)
    val argmaxResult = v1.argmax
    
    // Manual verification
    var maxIdx = 0
    var maxVal = v1(0)
    var i = 1
    while i < v1.length do
      if v1(i) > maxVal then
        maxVal = v1(i)
        maxIdx = i
      i += 1
    end while
    
    assertEquals(argmaxResult, maxIdx)
  }

  test("argmax with floating point precision") {
    val v1 = NArray[Double](1.0000001, 1.0000002, 1.0000000)
    assertEquals(v1.argmax, 1)
  }

end ArgmaxSuite