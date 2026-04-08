package vecxt

import munit.FunSuite

import all.*
import BoundsCheck.DoBoundsCheck.yes

class NDArrayFloatReductionsSuite extends FunSuite:

  test("NDArray[Float] sum") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f), Array(4))
    assertEqualsDouble(a.sum.toDouble, 10.0, 1e-6)
  }

  test("NDArray[Float] mean") {
    val a = NDArray(Array(2.0f, 4.0f, 6.0f, 8.0f), Array(4))
    assertEqualsDouble(a.mean.toDouble, 5.0, 1e-6)
  }

  test("NDArray[Float] min / max") {
    val a = NDArray(Array(3.0f, 1.0f, 4.0f, 1.0f, 5.0f, 9.0f), Array(6))
    assertEqualsDouble(a.min.toDouble, 1.0, 1e-6)
    assertEqualsDouble(a.max.toDouble, 9.0, 1e-6)
  }

  test("NDArray[Float] product") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f), Array(4))
    assertEqualsDouble(a.product.toDouble, 24.0, 1e-6)
  }

  test("NDArray[Float] norm") {
    val a = NDArray(Array(3.0f, 4.0f), Array(2))
    assertEqualsDouble(a.norm.toDouble, 5.0, 1e-6)
  }

  test("NDArray[Float] argmax / argmin") {
    val a = NDArray(Array(3.0f, 1.0f, 4.0f, 1.0f, 5.0f, 9.0f), Array(6))
    assertEquals(a.argmax, 5)
    assertEquals(a.argmin, 1)
  }

  test("NDArray[Float] sum(axis=0) on 2×3") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val result = a.sum(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(3.0f, 7.0f, 11.0f))
  }

  test("NDArray[Float] max(axis=0) on 2×3") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), Array(2, 3))
    val result = a.max(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(2.0f, 4.0f, 6.0f))
  }

  test("NDArray[Float] argmax(axis=0) on 2×3") {
    val a = NDArray(Array(1.0f, 4.0f, 3.0f, 2.0f, 5.0f, 6.0f), Array(2, 3))
    val result = a.argmax(0)
    assertEquals(result.shape.toSeq, Seq(3))
    assertEquals(result.toArray.toSeq, Seq(1, 0, 1))
  }

  test("NDArray[Float] axis out of range throws") {
    val a = NDArray(Array(1.0f, 2.0f, 3.0f), Array(3))
    intercept[InvalidNDArray](a.sum(-1))
    intercept[InvalidNDArray](a.sum(1))
  }

end NDArrayFloatReductionsSuite
