/*
 * Copyright 2023 quafadas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package vecxt

import scala.util.chaining.*
import all.*

class ArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  lazy val v_fill = Array[Double](0, 1, 2, 3, 4)

  test("linspace") {
    val v1 = linspace(0.0, 1.0, 5)
    assertEquals(v1.length, 5)
    assertEqualsDouble(v1(0), 0.0, 0.0001)
    assertEqualsDouble(v1(1), 0.25, 0.0001)
    assertEqualsDouble(v1(2), 0.5, 0.0001)
    assertEqualsDouble(v1(3), 0.75, 0.0001)
    assertEqualsDouble(v1(4), 1.0, 0.0001)
  }

  test("print") {
    val v1 = Array[Double](1, 2, 3)
    val v2 = Array[Double](4, 5, 6)
    val out = (v1 + v2).printArr
    assert(out.contains("5"))
  }

  test("unique") {
    val v1 = Array[Double](1, 2, 3, 2, 1, 4)
    val unique = v1.unique
    assertEquals(unique.length, 4)
    assertEquals(unique(0), 1.0)
    assertEquals(unique(1), 2.0)
    assertEquals(unique(2), 3.0)
    assertEquals(unique(3), 4.0)

    val v2 = Array[Double](1, 1, 1, 1, 1)
    val unique2 = v2.unique
    assertEquals(unique2.length, 1)

    val v3 = Array[Double]()
    val unique3 = v3.unique
    assertEquals(unique3.length, 0)

    val v4 = Array[Double](1, 2, 3, 4, 5)
    val unique4 = v4.unique
    assertEquals(unique4.length, 5)

  }

  test("fma") {
    val v1 = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val result = v1.fma(2.0, 1.0)
    assertEqualsDouble(result(0), 3.0, 0.0001)
    assertEqualsDouble(result(1), 5.0, 0.0001)
    assertEqualsDouble(result(2), 7.0, 0.0001)
    assertEqualsDouble(result(3), 9.0, 0.0001)
    assertEqualsDouble(result(4), 11.0, 0.0001)

    // We didnt' change the original vector
    assertEqualsDouble(v1(0), 1.0, 0.0001)

    // now we did
    v1.`fma!`(2.0, 1.0)
    assertEqualsDouble(v1(3), 9.0, 0.0001)
  }

  test("Array horizontal sum") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val v2 = v1 * 2.0
    val v3 = v1 * 3.0

    val l = Array(v1, v2, v3)
    val summed = l.horizontalSum

    assert(summed(0) == 1 + 2 + 3)
    assert(summed(1) == 2 + 4 + 6)
    assert(summed(2) == 3 + 6 + 9)

  }

  test("urnary ops") {
    val v1 = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)

    assertVecEquals(v1.exp, Array(v1.map(Math.exp).toArray*))
    assertVecEquals(v1.log, Array(v1.map(Math.log).toArray*))

  }

  test("Double - array") {
    val v1: Array[Double] = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val v2: Array[Double] = 2.0 - v1
    assertVecEquals(v2, Array(v1.map(2.0 - _).toArray*))
  }

  test("Double * array") {
    val v1: Array[Double] = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val v2: Array[Double] = 2.0 * v1
    assertVecEquals(v2, Array(v1.map(2.0 * _).toArray*))
  }

  test("Double + array") {
    val v1: Array[Double] = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val v2: Array[Double] = 2.0 + v1
    assertVecEquals(v2, Array(v1.map(2.0 + _).toArray*))
  }

  test("Double / array") {
    val v1: Array[Double] = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val v2: Array[Double] = 2.0 / v1
    assertVecEquals(v2, Array(v1.map(2.0 / _).toArray*))
  }

  test("Array / elementwise") {
    val v1 = Array[Double](1.0, 2.0, 3.0)

    val v2 = v1 / 2

    assertEqualsDouble(v2(0), 0.5, 0.00001)
    assertEqualsDouble(v2(1), 1.0, 0.00001)
    assertEqualsDouble(v2(2), 1.5, 0.00001)
  }

  test("Array *= elementwise") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    v1 *= 2

    assertEqualsDouble(v1(0), 2, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 6, 0.00001)

  }

  test("Array * elementwise") {
    val v1 = Array[Double](1.0, 2.0, 3.0)

    val v2 = v1 * 2

    assertEqualsDouble(v2(0), 2.0, 0.00001)
    assertEqualsDouble(v2(1), 4, 0.00001)
    assertEqualsDouble(v2(2), 6, 0.00001)
  }

  test("array indexing") {
    // val v1 = Array[Double](1.0, 2.0, 3.0)
    // val vIdx = Array[Boolean](true, false, true)
    // val afterIndex = v1(vIdx)
    // assertEqualsDouble(afterIndex(0), 1.0, 0.0001)
    // assertEqualsDouble(afterIndex(1), 3.0, 0.0001)

    val v2 = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    val vIdx2 = Array[Boolean](true, false, true, true, false, true, false, true, false)
    val afterIndex2 = v2(vIdx2)
    assertEqualsDouble(afterIndex2(4), 8.0, 0.0001)

    val v3 = Array[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
    val vIdx3 = Array[Boolean](true, false, true, true, false, true, false, true, false)
    val afterIndex3 = v3(vIdx3)
    assertEquals(afterIndex3(4), 8)

  }

  test("check vector operator precendance") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val v2 = Array[Double](3.0, 2.0, 1.0)

    val v3 = v1 + v2 * 2.0
    val v4 = v2 * 2.0 + v1

    assertEqualsDouble(v3(0), 7, 0.00001)
    assertEqualsDouble(v4(0), 7, 0.00001)

  }

  test("sum") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    assertEqualsDouble(v1.sum, 6.0, 0.0001)

    val v2 = Array[Double](1.0, 2.0, 3.0, 1.0, 2.0, 3.0, 1.0, 2.0, 3.0)
    assertEqualsDouble(v2.sum, 18.0, 0.0001)
  }

  test("product") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    assertEqualsDouble(v1.productSIMD, 6.0, 0.0001)

    val v2 = Array[Double](1.0, 2.0, 3.0, 1.0, 2.0, 3.0, 1.0, 2.0, 3.0)
    assertEqualsDouble(v2.productSIMD, 216.0, 0.0001)
  }

  test("product except self") {
    val v1 = Array[Double](1.0, 2.0, 3.0, 4.0)
    val v2 = v1.productExceptSelf
    assertEqualsDouble(v2(0), 24.0, 0.0001)
    assertEqualsDouble(v2(1), 12.0, 0.0001)
    assertEqualsDouble(v2(2), 8.0, 0.0001)
    assertEqualsDouble(v2(3), 6.0, 0.0001)
  }

  test("logSumExp") {
    val v1 = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    assertEqualsDouble(
      v1.logSumExp,
      Math.log(Math.exp(1) + Math.exp(2) + Math.exp(3) + Math.exp(4) + Math.exp(5)),
      0.0001
    )
  }

  test("cumsum") {
    val v1 = Array[Double](1.0, 2.0, 3.0).cumsum
    assert(v1(0) == 1)
    assert(v1(1) == 3)
    assert(v1(2) == 6)
  }

  test("increments") {
    val v1 = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    v1.increments.foreach(d => assertEqualsDouble(d, 1.0, 0.0001))
    val v2 = Array[Double](0.0, 2.0)
    assertVecEquals(v2.increments, v2)
  }

  test("Array scalar +:+ ") {
    val v1 = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0) +:+ 2.0

    assertEqualsDouble(v1(0), 3, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 5, 0.00001)
    assertEqualsDouble(v1(3), 6, 0.00001)
    assertEqualsDouble(v1(4), 7, 0.00001)

  }

  test("Array += ") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    v1 += Array[Double](3.0, 2.0, 1.0)

    assertEqualsDouble(v1(0), 4, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 4, 0.00001)

  }

  test("Array + ") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val v2 = Array[Double](3.0, 2.0, 1.0)

    val v3 = v1 + v2

    assertEqualsDouble(v3(0), 4, 0.00001)
    assertEqualsDouble(v3(1), 4, 0.00001)
    assertEqualsDouble(v3(2), 4, 0.00001)

  }

  test("Array -= ") {
    val v1 = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    v1 -= Array[Double](3.0, 2.0, 1.0, 0.0, 0.0, 0.0)

    assertEqualsDouble(v1(0), -2, 0.00001)
    assertEqualsDouble(v1(1), 0, 0.00001)
    assertEqualsDouble(v1(2), 2, 0.00001)
    assertEqualsDouble(v1(5), 6, 0.00001)

  }

  test("Array - ") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val v2 = Array[Double](3.0, 2.0, 1.0)

    val v3 = v1 - v2

    assertEqualsDouble(v3(0), -2, 0.00001)
    assertEqualsDouble(v3(1), 0, 0.00001)
    assertEqualsDouble(v3(2), 2, 0.00001)
  }

  test("Array * Array elementwise") {
    val v1: Array[Double] = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val v2: Array[Double] = Array[Double](1.0, 2.0, 3.0, 4.0, 5.0)

    val v3: Array[Double] = v1 * v2

    assertEqualsDouble(v3(0), 1.0, 0.00001)
    assertEqualsDouble(v3(1), 4.0, 0.00001)
    assertEqualsDouble(v3(2), 9.0, 0.00001)
    assertEqualsDouble(v3(3), 16.0, 0.00001)
    assertEqualsDouble(v3(4), 25.0, 0.00001)

    val vAdd: Array[Double] = v1 + v2
    assertEqualsDouble(vAdd(0), 2.0, 0.00001)
    assertEqualsDouble(vAdd(1), 4.0, 0.00001)
    assertEqualsDouble(vAdd(2), 6.0, 0.00001)
    assertEqualsDouble(vAdd(3), 8.0, 0.00001)
    assertEqualsDouble(vAdd(4), 10.0, 0.00001)

    val vdiv: Array[Double] = v1 / v2
    assertEqualsDouble(vdiv(0), 1.0, 0.00001)
    assertEqualsDouble(vdiv(1), 1.0, 0.00001)
    assertEqualsDouble(vdiv(2), 1.0, 0.00001)
    assertEqualsDouble(vdiv(3), 1.0, 0.00001)
    assertEqualsDouble(vdiv(4), 1.0, 0.00001)

    val vSub: Array[Double] = v1 - v2
    assertEqualsDouble(vSub(0), 0.0, 0.00001)
    assertEqualsDouble(vSub(1), 0.0, 0.00001)
    assertEqualsDouble(vSub(2), 0.0, 0.00001)
    assertEqualsDouble(vSub(3), 0.0, 0.00001)
    assertEqualsDouble(vSub(4), 0.0, 0.00001)

  }

  test("exp") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val v2 = v1.exp
    assertEqualsDouble(v2(0), Math.exp(1.0), 0.00001)
    assertEqualsDouble(v2(1), Math.exp(2.0), 0.00001)
    assertEqualsDouble(v2(2), Math.exp(3.0), 0.00001)
  }

  test("log") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val v2 = v1.log
    assertEqualsDouble(v2(0), Math.log(1.0), 0.00001)
    assertEqualsDouble(v2(1), Math.log(2.0), 0.00001)
    assertEqualsDouble(v2(2), Math.log(3.0), 0.00001)
  }

  // Check the vector loop
  test("countTrue") {
    val arrLong = Array.fill(100)(true)
    assertEquals(arrLong.trues, 100)

    arrLong(50) = false
    assertEquals(arrLong.trues, 99)
  }

  test("<= big") {
    val n = 50000
    val rand = scala.util.Random
    val vec = Array.tabulate(n)(_ => rand.nextDouble())
    assertEqualsDouble((vec <= 0.2).trues / n.toDouble, 0.2, 0.01)
  }

  test("<=") {
    val v_idx2 = v_fill < 2.5
    assertEquals(v_idx2.trues, 3)
  }

  test("<") {
    val v_idx2 = v_fill < 3.0
    assertEquals(v_idx2.trues, 3)
  }

  test(">") {
    val v_idx2 = v_fill > 2.5
    assertEquals(v_idx2.trues, 2)
  }

  test(">=") {
    val v_idx2 = v_fill >= 3.0
    assertEquals(v_idx2.trues, 2)
  }

  test("&&") {
    val v_idx2 = (v_fill < 3.0) && (v_fill > 1.0)
    assertEquals(v_idx2.trues, 1)
  }

  test("||") {
    val v_idx2 = (v_fill < 3.0) || (v_fill > 1.0)
    assertEquals(v_idx2.trues, 5)

    val v_idx3 = (v_fill < 1.0) || (v_fill > 4.0)
    assertEquals(v_idx3.trues, 1)
  }

  test("outer product") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val v2 = Array[Double](4.0, 5.0)
    val outer = v1.outer(v2)
    val shouldBe = Matrix.fromRows[Double](
      Array(4.0, 5.0),
      Array(8.0, 10.0),
      Array(12.0, 15.0)
    )
    assertEquals(outer.rows, 3)
    assertEquals(outer.cols, 2)

    assertVecEquals[Double](outer.raw, shouldBe.raw)

  }

  test("norm") {
    assertEqualsDouble(v_fill.norm, Math.sqrt(1 + 4 + 9 + 16), 0.00001)
  }

  test("Array indexing") {
    val v1 = Array[Double](1.0, 2.0, 3.0)
    val vIdx = Array[Boolean](true, false, true)
    val afterIndex = v1(vIdx)

    assertEquals(afterIndex.length, 2)
    assertEqualsDouble(afterIndex.head, 1, 0.0001)
    assertEqualsDouble(afterIndex.last, 3.0, 0.0001)
  }

  test("variance") {
    // https://www.storyofmathematics.com/sample-variance/#:~:text=7.%20Divide%20the%20number%20you%20get%20in%20step%206%20by example 3
    val ages = Array[Double](26.0, 48.0, 67.0, 39.0, 25.0, 25.0, 36.0, 44.0, 44.0, 47.0, 53.0, 52.0, 52.0, 51.0, 52.0,
      40.0, 77.0, 44.0, 40.0, 45.0, 48.0, 49.0, 19.0, 54.0, 82.0)
    val variance = ages.variance
    assertEqualsDouble(variance, 216.82, 0.01)
  }

  test("tvar") {
    import vecxt.reinsurance.tVar
    val v1 = Array.tabulate(100)(_.toDouble)
    val tvar = v1.tVar(0.95)
    assertEqualsDouble(tvar, 2, 0.0001)
  }

  test("qdep") {
    import vecxt.reinsurance.qdep
    val v1: Array[Double] = Array.tabulate[Double](100)(_.toDouble)
    val v2 = v1 * 2
    val qdep = v1.qdep(0.95, v2)
    assertEqualsDouble(qdep, 1, 0.0001)

    // val v3 = v1.copy // doesn't work ... not sure why.
    // v3(1) = 100
    // assertEqualsDouble(v1.qdep(0.95, v3), 0.8, 0.0001)
  }

  test("tvar index") {
    import vecxt.reinsurance.tVarIdx
    val v1 = Array.tabulate[Double](100)(_.toDouble)
    val tvar = v1.tVarIdx(0.95)
    assertEquals(tvar.trues, 5)
    for i <- 0 until 5 do assert(tvar(i))
    end for
    for i <- 5 until 100 do assert(!tvar(i))
    end for
  }

  test("VaR") {
    import vecxt.reinsurance.VaR
    val v1 = Array.tabulate(100)(_.toDouble)
    val var95 = v1.VaR(0.95)
    // At 95% confidence, we expect the 5th value (0-indexed: 4) in sorted array
    assertEqualsDouble(var95, 4.0, 0.0001)

    // Test different confidence levels
    val var90 = v1.VaR(0.90)
    assertEqualsDouble(var90, 9.0, 0.0001)

    val var99 = v1.VaR(0.99)
    assertEqualsDouble(var99, 0.0, 0.0001)
  }

  test("tVarWithVaR") {
    import vecxt.reinsurance.tVarWithVaR
    val v1 = Array.tabulate(100)(_.toDouble)
    val result = v1.tVarWithVaR(0.95)

    // TVaR should be the average of the tail (first 5 values: 0,1,2,3,4)
    assertEqualsDouble(result.TVaR, 2.0, 0.0001)
    // VaR should be the threshold value (4th value)
    assertEqualsDouble(result.VaR, 4.0, 0.0001)
    // cl should be 1 - alpha = 0.05
    assertEqualsDouble(result.cl, 0.05, 0.0001)
  }

  test("tVarWithVaRBatch") {
    import vecxt.reinsurance.tVarWithVaRBatch
    val v1 = Array.tabulate(100)(_.toDouble)
    val alphas = Array[Double](0.90, 0.95, 0.99)
    val results = v1.tVarWithVaRBatch(alphas)

    assertEquals(results.length, 3)

    // Check 90% confidence level
    val result90 = results(0)
    assertEqualsDouble(result90.cl, 0.10, 0.0001)
    assertEqualsDouble(result90.TVaR, 4.5, 0.0001) // avg of 0-9
    assertEqualsDouble(result90.VaR, 9.0, 0.0001)

    // Check 95% confidence level
    val result95 = results(1)
    assertEqualsDouble(result95.cl, 0.05, 0.0001)
    assertEqualsDouble(result95.TVaR, 2.0, 0.0001) // avg of 0-4
    assertEqualsDouble(result95.VaR, 4.0, 0.0001)

    // Check 99% confidence level
    val result99 = results(2)
    assertEqualsDouble(result99.cl, 0.01, 0.0001)
    assertEqualsDouble(result99.TVaR, 0.0, 0.0001) // avg of 0
    assertEqualsDouble(result99.VaR, 0.0, 0.0001)
  }

  test("VaR with unsorted data") {
    import vecxt.reinsurance.{VaR, tVarWithVaR}
    // Test with shuffled data to ensure sorting works correctly
    val v1 = Array[Double](45.0, 12.0, 89.0, 3.0, 67.0, 23.0, 56.0, 8.0, 34.0, 91.0)
    val var90 = v1.VaR(0.90)
    // Sorted: 3, 8, 12, 23, 34, 45, 56, 67, 89, 91
    // 90% confidence means tail size = 10 * (1-0.9) = 1
    // VaR should be value at index 0 (3.0)
    assertEqualsDouble(var90, 3.0, 0.0001)

    val result = v1.tVarWithVaR(0.90)
    assertEqualsDouble(result.TVaR, 3.0, 0.0001)
    assertEqualsDouble(result.VaR, 3.0, 0.0001)
    assertEqualsDouble(result.cl, 0.10, 0.0001)
  }

  test("tvar index 2") {
    import vecxt.reinsurance.tVarIdx
    val v1 = Array.from(Array(6.0, 2.0, 5.0, 5.0, 10.0, 1.0, 2.0, 3.0, 5.0, 8.0))
    val tvar = v1.tVarIdx(0.9)

    assertEquals(tvar.trues, 1)
    assert(tvar(5))
  }

  test("tvar index 3") {
    import vecxt.reinsurance.tVarIdx
    val v1 = Array.from(Array(6.0, 8.0, 5.0, 5.0, 10.0, 10.0, 2.0, 3.0, 5.0, 1.0))
    val tvar = v1.tVarIdx(0.8)
    assertEquals(tvar.trues, 2)
    assert(tvar(9))
    assert(tvar(6))

    val v4 = v1(tvar)
    assertEquals(v4.length, 2)
    assertEquals(v4(0), 2.0)
    assertEquals(v4(1), 1.0)
  }

end ArrayExtensionSuite
