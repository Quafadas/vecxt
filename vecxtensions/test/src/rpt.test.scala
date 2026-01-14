package vecxt.reinsurance

import vecxt.reinsurance.rpt.*
import Retentions.Retention
import Limits.Limit

class ReinsurancePricingSuite extends munit.FunSuite:

  def long_v = Array[Double](8, 11, 16, 8, 11, 16, 8, 11, 16)

  test("reinsurance function - ret and limit") {
    val v = long_v
    val l = v.length
    v.reinsuranceFunction(Some(Limit(5.0)), Some(Retention(10.0)))
    assertEqualsDouble(v(0), 0.0, 0.0001)
    assertEqualsDouble(v(1), 1.0, 0.0001)
    assertEqualsDouble(v(2), 5.0, 0.0001)
    assertEqualsDouble(v(l - 3), 0.0, 0.0001)
    assertEqualsDouble(v(l - 2), 1.0, 0.0001)
    assertEqualsDouble(v(l - 1), 5.0, 0.0001)
  }
  test("reinsurance function - ret only") {
    val v = long_v
    v.reinsuranceFunction(None, Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 1.0)
    assert(v(2) == 6.0)
    assert(v(v.length - 3) == 0.0)
    assert(v(v.length - 2) == 1.0)
    assert(v(v.length - 1) == 6.0)
  }
  test("reinsurance function - limit only") {
    val v = long_v
    v.reinsuranceFunction(Some(Limit(15.0)), None)
    assert(v(0) == 8.0)
    assert(v(1) == 11.0)
    assert(v(2) == 15.0)
    assert(v(v.length - 3) == 8.0)
    assert(v(v.length - 2) == 11.0)
    assert(v(v.length - 1) == 15.0)
  }
  test("reinsurance function - no ret or limit") {
    val v = Array[Double](8, 11, 16)
    v.reinsuranceFunction(None, None)
    assert(v(0) == 8.0)
    assert(v(1) == 11.0)
    assert(v(2) == 16.0)
  }

  test("franchise function - ret and limit") {
    val v = Array[Double](8, 11, 16, 10.0, 11.0)
    v.franchiseFunction(Some(Limit(5.0)), Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 11.0)
    assert(v(2) == 15.0)
    assert(v(3) == 10.0)

  }

  test("franchise function - ret only") {
    val v = Array[Double](8, 11, 16, 10.0, 11.0)
    v.franchiseFunction(None, Some(Retention(10.0)))
    assert(v(0) == 0.0)
    assert(v(1) == 11.0)
    assert(v(2) == 16.0)
    assert(v(3) == 10.0)
  }

  test("franchise function - Limit only") {
    val v = Array[Double](8, 11, 16, 10.0, 11.0)
    v.franchiseFunction(Some(Limit(10.0)), None)
    assert(v(0) == 8.0)
    assert(v(1) == 10.0)
    assert(v(2) == 10.0)
    assert(v(3) == 10.0)
  }

  test("franchise function - No ret or limit") {
    val v = Array[Double](8, 11, 16)
    v.franchiseFunction(None, None)
    assert(v(0) == 8.0)
    assert(v(1) == 11.0)
    assert(v(2) == 16.0)
  }

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

class ReinsuranceShareSuite extends munit.FunSuite:

  def test_v = Array[Double](8, 11, 16, 10.0)

  test("reinsurance function with share - ret and limit") {
    val v = Array[Double](8, 11, 16, 10.0)
    val share = 0.5
    v.reinsuranceFunction(Some(Limit(5.0)), Some(Retention(10.0)), share)
    assertEqualsDouble(v(0), 0.0, 0.0001, "First element") // (8-10).max(0).min(5) * 0.5 = 0
    assertEqualsDouble(v(1), 0.5, 0.0001, "Second element") // (11-10).max(0).min(5) * 0.5 = 0.5
    assertEqualsDouble(v(2), 2.5, 0.0001, "Third element") // (16-10).max(0).min(5) * 0.5 = 2.5
    assertEqualsDouble(v(3), 0.0, 0.0001, "Fourth element") // (10-10).max(0).min(5) * 0.5 = 0
  }

  test("reinsurance function with share - ret only") {
    val v = Array[Double](8, 11, 16, 10.0)
    val share = 0.25
    v.reinsuranceFunction(None, Some(Retention(10.0)), share)
    assertEqualsDouble(v(0), 0.0, 0.0001, "First element") // (8-10).max(0) * 0.25 = 0
    assertEqualsDouble(v(1), 0.25, 0.0001, "Second element") // (11-10).max(0) * 0.25 = 0.25
    assertEqualsDouble(v(2), 1.5, 0.0001, "Third element") // (16-10).max(0) * 0.25 = 1.5
    assertEqualsDouble(v(3), 0.0, 0.0001, "Fourth element") // (10-10).max(0) * 0.25 = 0
  }

  test("reinsurance function with share - limit only") {
    val v = Array[Double](8, 11, 16, 10.0)
    val share = 0.8
    v.reinsuranceFunction(Some(Limit(12.0)), None, share)
    assertEqualsDouble(v(0), 6.4, 0.0001, "First element") // min(8, 12) * 0.8 = 6.4
    assertEqualsDouble(v(1), 8.8, 0.0001, "Second element") // min(11, 12) * 0.8 = 8.8
    assertEqualsDouble(v(2), 9.6, 0.0001, "Third element") // min(16, 12) * 0.8 = 9.6
    assertEqualsDouble(v(3), 8.0, 0.0001, "Fourth element") // min(10, 12) * 0.8 = 8.0
  }

  test("reinsurance function with share - no ret or limit") {
    val v = Array[Double](8, 11, 16, 10.0)
    val share = 0.3
    v.reinsuranceFunction(None, None, share)
    assertEqualsDouble(v(0), 2.4, 0.0001, "First element") // 8 * 0.3 = 2.4
    assertEqualsDouble(v(1), 3.3, 0.0001, "Second element") // 11 * 0.3 = 3.3
    assertEqualsDouble(v(2), 4.8, 0.0001, "Third element") // 16 * 0.3 = 4.8
    assertEqualsDouble(v(3), 3.0, 0.0001, "Fourth element") // 10 * 0.3 = 3.0
  }

  test("reinsurance function with share = 1.0 should match default") {
    val v1 = Array[Double](8, 11, 16, 10.0)
    val v2 = Array[Double](8, 11, 16, 10.0)

    v1.reinsuranceFunction(Some(Limit(5.0)), Some(Retention(10.0)))
    v2.reinsuranceFunction(Some(Limit(5.0)), Some(Retention(10.0)), 1.0)

    for i <- 0 until v1.length do assertEqualsDouble(v1(i), v2(i), 0.0001, s"Element $i")
    end for
  }

  test("reinsurance function with share = 0.0 should give zeros") {
    val v = Array[Double](8, 11, 16, 10.0)
    v.reinsuranceFunction(Some(Limit(5.0)), Some(Retention(10.0)), 0.0)

    for i <- 0 until v.length do assertEqualsDouble(v(i), 0.0, 0.0001, s"Element $i")
    end for
  }

end ReinsuranceShareSuite
