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

import Retentions.*
import Limits.*
import scala.scalajs.js.typedarray.Float64Array
import scala.util.chaining.*

extension [A <: AnyRef](o: A) def some = Some(o)

class ReinsurancePricingSuite extends munit.FunSuite:

  def v_8_11_16 = Float64Array(3).tap(a =>
    a.update(0, 8)
    a.update(1, 11)
    a.update(2,16)
  )

  test("reinsurance function - ret and limit") {
    val v = v_8_11_16
    v.reinsuranceFunction(Some(Limit(5.0)), Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 1.0)
    assert(v(2) == 5.0)
  }
  test("reinsurance function - ret only") {
    val v = v_8_11_16
    v.reinsuranceFunction(None, Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 1.0)
    assert(v(2) == 6.0)
  }
  test("reinsurance function - limit only") {
    val v = v_8_11_16
    v.reinsuranceFunction(Some(Limit(15.0)), None)
    assert(v(0) == 8.0)
    assert(v(1) == 11.0)
    assert(v(2) == 15.0)
  }
  test("reinsurance function - no ret or limit") {
    val v = v_8_11_16
    v.reinsuranceFunction(None, None)
    assert(v(0) == 8.0)
    assert(v(1) == 11.0)
    assert(v(2) == 16.0)
  }

  test("franchise function - ret and limit") {
    val v = v_8_11_16
    v.franchiseFunction(Some(Limit(5.0)), Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 11.0)
    assert(v(2) == 15.0)
  }

  test("franchise function - ret only") {
    val v = v_8_11_16
    v.franchiseFunction(None, Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 11.0)
    assert(v(2) == 16.0)
  }

  test("franchise function - Limit only") {
    val v = v_8_11_16
    v.franchiseFunction(Some(Limit(10.0)), None)
    assert(v(0) == 8.0)
    assert(v(1) == 10.0)
    assert(v(2) == 10.0)
  }

  test("franchise function - No ret or limit") {
    val v = v_8_11_16
    v.franchiseFunction(None, None)
    assert(v(0) == 8.0)
    assert(v(1) == 11.0)
    assert(v(2) == 16.0)
  }

  // ----- STOP

  // test("Simple Agg 10") {
  //   val l_10Agg = makeLayer(1.0, aggLimit = 10.0.some)
  //   val losses = Array[Double](7, 2, 2, 2)
  //   val coveredLosses = l_10Agg.coveredLosses(losses)
  //   assert(coveredLosses(0) == 7)
  //   assert(coveredLosses(1) == 2)
  //   assert(coveredLosses(2) == 1)
  //   assert(coveredLosses(3) == 0)
  // }

  // test("Simple Agg ret 10") {
  //   val l_10Agg = makeLayer(1.0, aggRetention = 10.0.some)
  //   val losses = Array[Double](7, 2, 2, 2)
  //   val coveredLosses = l_10Agg.coveredLosses(losses)
  //   assertEqualsDouble(coveredLosses(0), 0, 0.0001)
  //   assert(coveredLosses(1) == 0)
  //   assert(coveredLosses(2) == 1.0)
  //   assert(coveredLosses(3) == 2.0)
  // }

  // test("Simple occ ret 10") {
  //   val l_10Agg = makeLayer(1.0, occRetention = 10.0.some)
  //   val losses = Array[Double](9, 11)
  //   val coveredLosses = l_10Agg.coveredLosses(losses)
  //   assert(coveredLosses(0) == 0)
  //   assert(coveredLosses(1) == 1)
  // }

  // test("share") {
  //   val l_10Agg = makeLayer(0.5)
  //   val losses = Array[Double](1, 2)
  //   val coveredLosses = l_10Agg.coveredLosses(losses)
  //   assert(coveredLosses(0) == 0.5)
  //   assert(coveredLosses(1) == 1)
  // }

  // test("Simple Occ 10") {
  //   val l_10Agg = makeLayer(1.0, occLimit = 10.0.some)
  //   val losses = Array[Double](8, 12)
  //   val coveredLosses = l_10Agg.coveredLosses(losses)
  //   assert(coveredLosses(0) == 8)
  //   assert(coveredLosses(1) == 10)
  // }

  // test("Simple Agg 10 xs 5") {
  //   val l_Agg_10xs5 = makeLayer(1.0, aggLimit = 5.0.some, aggRetention = 5.0.some)
  //   val losses = Array[Double](7, 2, 2, 2)
  //   val coveredLosses = l_Agg_10xs5.coveredLosses(losses)
  //   assert(coveredLosses(0) == 2)
  //   assert(coveredLosses(1) == 2)
  //   assert(coveredLosses(2) == 1)
  //   assert(coveredLosses(3) == 0)
  // }

  // test("Simple Occ 10 xs 5") {
  //   val l_Occ_10xs5 = makeLayer(1.0, occLimit = 10.0.some, occRetention = 5.0.some)
  //   // pprint.pprintln(l_Occ_10xs5)
  //   val losses = Array[Double](3, 11, 16)
  //   val coveredLosses = l_Occ_10xs5.coveredLosses(losses)
  //   assert(coveredLosses(0) == 0)
  //   assert(coveredLosses(1) == 6)
  //   assert(coveredLosses(2) == 10)
  // }
end ReinsurancePricingSuite
