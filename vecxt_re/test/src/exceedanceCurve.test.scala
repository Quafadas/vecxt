package vecxt_re

import munit.FunSuite

class ExceedanceCurveSuite extends FunSuite:

  // 10 iterations: ranked 100, 90, ..., 10 at return periods 10, 5, 10/3, 2.5, 2, 5/3, 10/7, 1.25, 10/9, 1
  val curve = ExceedanceCurve.fromIterationValues(Array(30.0, 10.0, 100.0, 50.0, 20.0, 90.0, 40.0, 80.0, 70.0, 60.0))

  test("fromIterationValues ranks largest first and leaves its input alone") {
    val in = Array(1.0, 3.0, 2.0)
    val c = ExceedanceCurve.fromIterationValues(in)
    assertEquals(c.amounts.toList, List(3.0, 2.0, 1.0))
    assertEquals(in.toList, List(1.0, 3.0, 2.0))
    assertEquals(c.returnPeriods.toList, List(3.0, 1.5, 1.0))
  }

  test("lossAt is exact on the grid of return periods") {
    assertEquals(curve.lossAt(10), 100.0)
    assertEquals(curve.lossAt(5), 90.0)
    assertEquals(curve.lossAt(10.0 / 3.0), 80.0)
    assertEquals(curve.lossAt(2), 60.0)
    assertEquals(curve.lossAt(10.0 / 7.0), 40.0)
    assertEquals(curve.lossAt(10.0 / 9.0), 20.0)
    assertEquals(curve.lossAt(1), 10.0)
  }

  test("lossAt off the grid takes the next point up the curve") {
    assertEquals(curve.lossAt(4), 90.0) // between 10/3 (80) and 5 (90)
    assertEquals(curve.lossAt(2.1), 70.0) // between 2 (60) and 2.5 (70)
    assertEquals(curve.lossAt(0.5), 10.0)
  }

  test("lossAt agrees with a brute-force search on a large catalogue") {
    val n = 10_000
    val rng = scala.util.Random(7)
    val c = ExceedanceCurve.fromIterationValues(Array.fill(n)(rng.nextDouble()))
    // includes rps like n / k whose floor(n / rp) can round to k - 1
    val rps = Seq(1.0, 2, 5, 10, 25, 50, 100, 200, 250, 500, 1000, 10000) ++ (1 to 200).map(k => n.toDouble / k)
    rps.foreach { rp =>
      val expected = c.points.reverseIterator.find(_.returnPeriod >= rp).get.amount
      assertEquals(c.lossAt(rp), expected, s"rp = $rp")
    }
  }

  test("lossAt refuses return periods the curve can't support") {
    intercept[IllegalArgumentException](curve.lossAt(11))
    intercept[IllegalArgumentException](curve.lossAt(0))
    intercept[IllegalArgumentException](curve.lossAt(-1))
  }

  test("returnPeriodAt counts the iterations reaching the loss") {
    assertEquals(curve.returnPeriodAt(100), Some(10.0))
    assertEquals(curve.returnPeriodAt(95), Some(10.0))
    assertEquals(curve.returnPeriodAt(90), Some(5.0))
    assertEquals(curve.returnPeriodAt(55), Some(2.0))
    assertEquals(curve.returnPeriodAt(10), Some(1.0))
    assertEquals(curve.returnPeriodAt(-5), Some(1.0))
    assertEquals(curve.returnPeriodAt(100.01), None)
  }

  test("returnPeriodAt with ties uses every tied iteration") {
    val c = ExceedanceCurve.fromIterationValues(Array(5.0, 5.0, 5.0, 1.0))
    assertEquals(c.returnPeriodAt(5.0), Some(4.0 / 3.0))
    assertEquals(c.lossAt(4.0 / 3.0), 5.0)
    assertEquals(c.lossAt(4.0), 5.0)
  }

  test("lossAt and returnPeriodAt are consistent on the grid") {
    (1 to 10).foreach { k =>
      val rp = 10.0 / k
      assertEquals(curve.returnPeriodAt(curve.lossAt(rp)), Some(rp))
    }
  }

  test("ExceedanceType picks the right curve") {
    val sc = Scenarr(Array(1, 1, 2), Array(1, 2, 1), Array(3.0, 4.0, 5.0), Array(1L, 2L, 3L), 2, isSorted = true)
    assertEquals(ExceedanceType.OEP.of(sc).amounts.toList, List(5.0, 4.0))
    assertEquals(ExceedanceType.AEP.of(sc).amounts.toList, List(7.0, 5.0))
  }
end ExceedanceCurveSuite

class EventKeySuite extends FunSuite:

  test("eventKey round trips, including negative and extreme values") {
    for
      src <- Seq(0, 1, 42, -1, Int.MaxValue, Int.MinValue)
      ev <- Seq(0, 1, 123456, -1, Int.MaxValue, Int.MinValue)
    do
      val k = Scenarr.eventKey(src, ev)
      assertEquals(Scenarr.eventKeySource(k), src)
      assertEquals(Scenarr.eventKeyEventId(k), ev)
  }

  test("same vendor event id from different sources no longer merges") {
    val a = Scenarr(Array(1), Array(1), Array(10.0), Array(7L), 1, isSorted = true, name = "a")
    val b = Scenarr(Array(1), Array(2), Array(20.0), Array(7L), 1, isSorted = true, name = "b")
    // Same id on different days: the naive combine refuses
    intercept[IllegalArgumentException](Scenarr.combineAll(Seq(a, b)))
    val c = Scenarr.combineAll(Seq(a.withIdSource(1), b.withIdSource(2)))
    assertEquals(c.amounts.toList, List(10.0, 20.0))
    assertEquals(c.ids.toList, List(Scenarr.eventKey(1, 7), Scenarr.eventKey(2, 7)))
  }

  test("withIdSource refuses ids that don't fit in 32 bits") {
    val a = Scenarr(Array(1), Array(1), Array(10.0), Array(1L << 40), 1, isSorted = true)
    intercept[IllegalArgumentException](a.withIdSource(1))
  }
end EventKeySuite
