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

import narr.*
import scala.util.chaining.*
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

class ArrayExtensionSuite extends munit.FunSuite:
  import BoundsCheck.DoBoundsCheck.yes

  lazy val v_fill = NArray[Double](0, 1, 2, 3, 4)

  test("print") {
    val v1 = NArray[Double](1, 2, 3)
    val v2 = NArray[Double](4, 5, 6)
    val out = (v1 + v2).printArr
    assert(out.contains("5"))
  }

  test("Array horizontal sum") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = v1 * 2.0
    val v3 = v1 * 3.0

    val l = Array(v1, v2, v3)
    val summed = l.horizontalSum

    assert(summed(0) == 1 + 2 + 3)
    assert(summed(1) == 2 + 4 + 6)
    assert(summed(2) == 3 + 6 + 9)

  }

  test("urnary ops") {
    val v1 = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0)

    assertVecEquals(v1.exp, NArray(v1.map(Math.exp).toArray*))
    assertVecEquals(v1.log, NArray(v1.map(Math.log).toArray*))

  }

  test("array indexing") {
    // val v1 = NArray[Double](1.0, 2.0, 3.0)
    // val vIdx = NArray[Boolean](true, false, true)
    // val afterIndex = v1(vIdx)
    // assertEqualsDouble(afterIndex(0), 1.0, 0.0001)
    // assertEqualsDouble(afterIndex(1), 3.0, 0.0001)

    val v2 = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
    val vIdx2 = NArray[Boolean](true, false, true, true, false, true, false, true, false)
    val afterIndex2 = v2(vIdx2)
    assertEqualsDouble(afterIndex2(4), 8.0, 0.0001)

    val v3 = NArray[Int](1, 2, 3, 4, 5, 6, 7, 8, 9)
    val vIdx3 = NArray[Boolean](true, false, true, true, false, true, false, true, false)
    val afterIndex3 = v3(vIdx3)
    assertEquals(afterIndex3(4), 8)

  }

  test("check vector operator precendance") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = NArray[Double](3.0, 2.0, 1.0)

    val v3 = v1 + v2 * 2.0
    val v4 = v2 * 2.0 + v1

    assertEqualsDouble(v3(0), 7, 0.00001)
    assertEqualsDouble(v4(0), 7, 0.00001)

  }

  test("sum") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    assertEqualsDouble(v1.sum, 6.0, 0.0001)

    val v2 = NArray[Double](1.0, 2.0, 3.0, 1.0, 2.0, 3.0, 1.0, 2.0, 3.0)
    assertEqualsDouble(v2.sum, 18.0, 0.0001)
  }

  test("product") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    assertEqualsDouble(v1.product, 6.0, 0.0001)

    val v2 = NArray[Double](1.0, 2.0, 3.0, 1.0, 2.0, 3.0, 1.0, 2.0, 3.0)
    assertEqualsDouble(v2.product, 216.0, 0.0001)
  }

  test("product except self") {
    val v1 = NArray[Double](1.0, 2.0, 3.0, 4.0)
    val v2 = v1.productExceptSelf
    assertEqualsDouble(v2(0), 24.0, 0.0001)
    assertEqualsDouble(v2(1), 12.0, 0.0001)
    assertEqualsDouble(v2(2), 8.0, 0.0001)
    assertEqualsDouble(v2(3), 6.0, 0.0001)
  }

  test("logSumExp") {
    val v1 = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    assertEqualsDouble(
      v1.logSumExp,
      Math.log(Math.exp(1) + Math.exp(2) + Math.exp(3) + Math.exp(4) + Math.exp(5)),
      0.0001
    )
  }

  test("cumsum") {
    val v1 = NArray[Double](1.0, 2.0, 3.0).tap(_.cumsum)
    assert(v1(0) == 1)
    assert(v1(1) == 3)
    assert(v1(2) == 6)
  }

  test("increments") {
    val v1 = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    v1.increments.foreach(d => assertEqualsDouble(d, 1.0, 0.0001))
    val v2 = NArray[Double](0.0, 2.0)
    assertVecEquals(v2.increments, v2)
  }

  test("Array scalar +:+ ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0) +:+ 2.0

    assertEqualsDouble(v1(0), 3, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 5, 0.00001)
    assertEqualsDouble(v1(3), 6, 0.00001)
    assertEqualsDouble(v1(4), 7, 0.00001)

  }

  test("Array += ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    v1 += NArray[Double](3.0, 2.0, 1.0)

    assertEqualsDouble(v1(0), 4, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 4, 0.00001)

  }

  test("Array + ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = NArray[Double](3.0, 2.0, 1.0)

    val v3 = v1 + v2

    assertEqualsDouble(v3(0), 4, 0.00001)
    assertEqualsDouble(v3(1), 4, 0.00001)
    assertEqualsDouble(v3(2), 4, 0.00001)

  }

  test("Array -= ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    v1 -= NArray[Double](3.0, 2.0, 1.0, 0.0, 0.0, 0.0)

    assertEqualsDouble(v1(0), -2, 0.00001)
    assertEqualsDouble(v1(1), 0, 0.00001)
    assertEqualsDouble(v1(2), 2, 0.00001)
    assertEqualsDouble(v1(5), 6, 0.00001)

  }

  test("Array - ") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = NArray[Double](3.0, 2.0, 1.0)

    val v3 = v1 - v2

    assertEqualsDouble(v3(0), -2, 0.00001)
    assertEqualsDouble(v3(1), 0, 0.00001)
    assertEqualsDouble(v3(2), 2, 0.00001)
  }

  test("Array /= elementwise") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    v1 /= 2

    assertEqualsDouble(v1(0), 0.5, 0.00001)
    assertEqualsDouble(v1(1), 1.0, 0.00001)
    assertEqualsDouble(v1(2), 1.5, 0.00001)

  }

  test("Array / elementwise") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)

    val v2 = v1 / 2

    assertEqualsDouble(v2(0), 0.5, 0.00001)
    assertEqualsDouble(v2(1), 1.0, 0.00001)
    assertEqualsDouble(v2(2), 1.5, 0.00001)
  }

  test("Array *= elementwise") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    v1 *= 2

    assertEqualsDouble(v1(0), 2, 0.00001)
    assertEqualsDouble(v1(1), 4, 0.00001)
    assertEqualsDouble(v1(2), 6, 0.00001)

  }

  test("Array * elementwise") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)

    val v2 = v1 * 2

    assertEqualsDouble(v2(0), 2.0, 0.00001)
    assertEqualsDouble(v2(1), 4, 0.00001)
    assertEqualsDouble(v2(2), 6, 0.00001)
  }

  test("Array * Array elementwise") {
    val v1: NArray[Double] = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0)
    val v2: NArray[Double] = NArray[Double](1.0, 2.0, 3.0, 4.0, 5.0)

    val v3: NArray[Double] = v1 * v2

    assertEqualsDouble(v3(0), 1.0, 0.00001)
    assertEqualsDouble(v3(1), 4.0, 0.00001)
    assertEqualsDouble(v3(2), 9.0, 0.00001)
    assertEqualsDouble(v3(3), 16.0, 0.00001)
    assertEqualsDouble(v3(4), 25.0, 0.00001)

    val vAdd: NArray[Double] = v1 + v2
    assertEqualsDouble(vAdd(0), 2.0, 0.00001)
    assertEqualsDouble(vAdd(1), 4.0, 0.00001)
    assertEqualsDouble(vAdd(2), 6.0, 0.00001)
    assertEqualsDouble(vAdd(3), 8.0, 0.00001)
    assertEqualsDouble(vAdd(4), 10.0, 0.00001)

    val vdiv: NArray[Double] = v1 / v2
    assertEqualsDouble(vdiv(0), 1.0, 0.00001)
    assertEqualsDouble(vdiv(1), 1.0, 0.00001)
    assertEqualsDouble(vdiv(2), 1.0, 0.00001)
    assertEqualsDouble(vdiv(3), 1.0, 0.00001)
    assertEqualsDouble(vdiv(4), 1.0, 0.00001)

    val vSub: NArray[Double] = v1 - v2
    assertEqualsDouble(vSub(0), 0.0, 0.00001)
    assertEqualsDouble(vSub(1), 0.0, 0.00001)
    assertEqualsDouble(vSub(2), 0.0, 0.00001)
    assertEqualsDouble(vSub(3), 0.0, 0.00001)
    assertEqualsDouble(vSub(4), 0.0, 0.00001)

  }

  test("exp") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = v1.exp
    assertEqualsDouble(v2(0), Math.exp(1.0), 0.00001)
    assertEqualsDouble(v2(1), Math.exp(2.0), 0.00001)
    assertEqualsDouble(v2(2), Math.exp(3.0), 0.00001)
  }

  test("log") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = v1.log
    assertEqualsDouble(v2(0), Math.log(1.0), 0.00001)
    assertEqualsDouble(v2(1), Math.log(2.0), 0.00001)
    assertEqualsDouble(v2(2), Math.log(3.0), 0.00001)
  }

  // Check the vector loop
  test("countTrue") {
    val arrLong = NArray.fill(100)(true)
    assertEquals(arrLong.trues, 100)

    arrLong(50) = false
    assertEquals(arrLong.trues, 99)
  }

  test("<= big") {
    val n = 50000
    val rand = scala.util.Random
    val vec = NArray.tabulate(n)(_ => rand.nextDouble())
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
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val v2 = NArray[Double](4.0, 5.0)
    val outer = v1.outer(v2)
    val shouldBe = Matrix.fromRows[Double](
      NArray(4.0, 5.0),
      NArray(8.0, 10.0),
      NArray(12.0, 15.0)
    )
    assertEquals(outer.rows, 3)
    assertEquals(outer.cols, 2)

    assertVecEquals(outer.raw, shouldBe.raw)

  }

  test("norm") {
    assertEqualsDouble(v_fill.norm, Math.sqrt(1 + 4 + 9 + 16), 0.00001)
  }

  test("Array indexing") {
    val v1 = NArray[Double](1.0, 2.0, 3.0)
    val vIdx = NArray[Boolean](true, false, true)
    val afterIndex = v1(vIdx)

    assertEquals(afterIndex.length, 2)
    assertEqualsDouble(afterIndex.head, 1, 0.0001)
    assertEqualsDouble(afterIndex.last, 3.0, 0.0001)
  }

  test("variance") {
    // https://www.storyofmathematics.com/sample-variance/#:~:text=7.%20Divide%20the%20number%20you%20get%20in%20step%206%20by example 3
    val ages = NArray[Double](26.0, 48.0, 67.0, 39.0, 25.0, 25.0, 36.0, 44.0, 44.0, 47.0, 53.0, 52.0, 52.0, 51.0, 52.0,
      40.0, 77.0, 44.0, 40.0, 45.0, 48.0, 49.0, 19.0, 54.0, 82.0)
    val variance = ages.variance
    assertEqualsDouble(variance, 216.82, 0.01)
  }

  test("tvar") {
    import vecxt.reinsurance.tVar
    val v1 = NArray.tabulate(100)(_.toDouble)
    val tvar = v1.tVar(0.95)
    assertEqualsDouble(tvar, 2, 0.0001)
  }

  test("qdep") {
    import vecxt.reinsurance.qdep
    val v1: NArray[Double] = NArray.tabulate[Double](100)(_.toDouble)
    val v2 = v1 * 2
    val qdep = v1.qdep(0.95, v2)
    assertEqualsDouble(qdep, 1, 0.0001)

    // val v3 = v1.copy // doesn't work ... not sure why.
    // v3(1) = 100
    // assertEqualsDouble(v1.qdep(0.95, v3), 0.8, 0.0001)
  }

  test("tvar index") {
    import vecxt.reinsurance.tVarIdx
    val v1 = NArray.tabulate[Double](100)(_.toDouble)
    val tvar = v1.tVarIdx(0.95)
    assertEquals(tvar.trues, 5)
    for i <- 0 until 5 do assert(tvar(i))
    end for
    for i <- 5 until 100 do assert(!tvar(i))
    end for
  }

  test("tvar index 2") {
    import vecxt.reinsurance.tVarIdx
    val v1 = NArray.from(Array(6.0, 2.0, 5.0, 5.0, 10.0, 1.0, 2.0, 3.0, 5.0, 8.0))
    val tvar = v1.tVarIdx(0.9)

    assertEquals(tvar.trues, 1)
    assert(tvar(5))
  }

  test("tvar index 3") {
    import vecxt.reinsurance.tVarIdx
    val v1 = NArray.from(Array(6.0, 8.0, 5.0, 5.0, 10.0, 10.0, 2.0, 3.0, 5.0, 1.0))
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
