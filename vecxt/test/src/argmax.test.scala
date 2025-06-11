package vecxt

import narr.*
import vecxt.all.*

class ArgmaxSuite extends munit.FunSuite:

  test("argmax simple case") {
    val v1 = NArray[Double](1.0, 3.0, 2.0, 5.0, 4.0, -1, 0.0, -1.0, 2.0, 3.0)
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

  test("argmax empty") {
    val v1 = NArray[Double]()
    assertEquals(v1.argmax, -1)
  }

  test("argmax with NaN handling") {
    // Assuming your implementation handles NaN in some specific way
    val v1 = NArray[Double](1.0, Double.NaN, 3.0, 2.0)
    val result = v1.argmax
    // Typically argmax should return index of non-NaN maximum
    assert(result == 2)
  }

end ArgmaxSuite

class ArgminSuite extends munit.FunSuite:

  test("argmin simple case") {
    val v1 = NArray[Double](1.0, 3.0, 2.0, 5.0, 4.0, -1, 0.0, -1.0, 2.0, 3.0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    assertEquals(v1.argmin, 5)
  }

  test("argmin first element is min") {
    val v1 = NArray[Double](-5.0, 3.0, 2.0, 1.0, 4.0)
    assertEquals(v1.argmin, 0)
  }

  test("argmin last element is min") {
    val v1 = NArray[Double](1.0, 3.0, 2.0, -4.0, -5.0)
    assertEquals(v1.argmin, 4)
  }

  test("argmin with duplicates returns first occurrence") {
    val v1 = NArray[Double](1.0, -5.0, 3.0, -5.0, 2.0)
    assertEquals(v1.argmin, 1)
  }

  test("argmin single element") {
    val v1 = NArray[Double](42.0)
    assertEquals(v1.argmin, 0)
  }

  test("argmin empty") {
    val v1 = NArray[Double]()
    assertEquals(v1.argmin, -1)
  }

end ArgminSuite
