package vecxt_re

import java.time.LocalDate

class ScenarioSuite extends munit.FunSuite:

  test("Events") {

    val event = Event.random

  }

  test("Random Scenario") {
    val numItr = 10
    val s = Scenario(
      Vector.fill(10)(Event.random(maxIter = numItr)),
      numItr
    )

    assertEquals(s.iterations.length, 10)
    assertEquals(s.amounts.length, 10)

    assert(s.hasOccurence)
  }

  test("Some scenario stats") {
    val e1 = Event(1, 15.0)
    val e2 = Event(4, 25.0)
    val e3 = Event(4, 1.0)
    val e4 = Event(4, 1.0)
    val e5 = Event(4, 1.0)
    val numItr = 5

    val s = Scenario(
      Vector(e2, e3, e4, e5, e1),
      numItr
    )

    assertVecEquals(s.freq, Array(1, 0, 0, 4, 0))
    assertVecEquals(s.agg, Array(15.0, 0, 0, 28.0, 0))
    assertEqualsDouble(s.meanFreq, (1 + 4) / 5.0, 0.00000001)
    assertEqualsDouble(s.clusterCoeff, 2.0, 0.000001)
    assertEqualsDouble(s.varianceMeanRatio, 3, 0.00001)

  }

  test("scaleAmntBy doubles amounts and threshold, preserves other fields"):
    val base = Scenarr(
      iterations = Array(1, 1, 2),
      days = Array(1, 2, 3),
      amounts = Array(100.0, 200.0, 300.0),
      numberIterations = 2,
      threshold = 50.0
    )

    val scaled = base.scaleAmntBy(2.0)

    assertEquals(scaled.amounts.toSeq, Seq(200.0, 400.0, 600.0))
    assertEquals(scaled.threshold, 100.0)
    // other fields unchanged
    assertEquals(scaled.iterations.toSeq, base.iterations.toSeq)
    assertEquals(scaled.days.toSeq, base.days.toSeq)
    assertEquals(scaled.numberIterations, base.numberIterations)
    assertEquals(scaled.name, base.name)
    assertEquals(scaled.isSorted, base.isSorted)

  test("scaleAmntBy with zero scale results in zero amounts and zero threshold"):
    val base = Scenarr(Array(1), Array(1), Array(123.0), numberIterations = 1, threshold = 7.5)
    val scaled0 = base.scaleAmntBy(0.0)
    assertEquals(scaled0.amounts.toSeq, Seq(0.0))
    assertEquals(scaled0.threshold, 0.0)

  test("scaleAmntBy supports negative scaling and does not mutate original"):
    val originalAmounts = Array(10.0, 20.0, 30.0)
    val base = Scenarr(Array(1, 1, 1), Array(1, 2, 3), originalAmounts.clone(), numberIterations = 1, threshold = 5.0)
    val scaled = base.scaleAmntBy(-1.5)
    assertEquals(scaled.amounts.toSeq, Seq(-15.0, -30.0, -45.0))
    assertEquals(scaled.threshold, -7.5)
    // original remains unchanged
    assertEquals(base.amounts.toSeq, originalAmounts.toSeq)
    assertEquals(base.threshold, 5.0)

  test("applyThreshold increases threshold and filters claims"):
    val base = Scenarr(
      iterations = Array(1, 2, 3),
      days = Array(10, 20, 30),
      amounts = Array(10.0, 20.0, 30.0),
      numberIterations = 3,
      threshold = 5.0
    )

    val applied = base.applyThreshold(15.0)

    assertEquals(applied.amounts.toSeq, Seq(20.0, 30.0))
    assertEquals(applied.iterations.toSeq, Seq(2, 3))
    assertEquals(applied.days.toSeq, Seq(20, 30))
    assertEquals(applied.threshold, 15.0)
    // original remains unchanged
    assertEquals(base.amounts.toSeq, Seq(10.0, 20.0, 30.0))
    assertEquals(base.threshold, 5.0)

  test("applyThreshold throws if newThresh is not greater than current threshold"):
    val base2 = Scenarr(Array(1), Array(1), Array(100.0), numberIterations = 1, threshold = 50.0)
    val ex = intercept[Exception](base2.applyThreshold(50.0))
    assert(ex.getMessage.contains("Threshold may only be increased"))

  test("applyThreshold may result in no claims"):
    val base3 = Scenarr(Array(1, 1), Array(1, 2), Array(10.0, 20.0), numberIterations = 1, threshold = 5.0)
    val appliedEmpty = base3.applyThreshold(100.0)
    assertEquals(appliedEmpty.amounts.toSeq, Seq())
    assertEquals(appliedEmpty.iterations.toSeq, Seq())
    assertEquals(appliedEmpty.days.toSeq, Seq())
    assertEquals(appliedEmpty.threshold, 100.0)

  test("claimDates maps day 1 to day1 property"):
    val base = Scenarr(
      iterations = Array(1, 2),
      days = Array(1, 100),
      amounts = Array(10.0, 20.0),
      numberIterations = 2,
      threshold = 1.0,
      day1 = LocalDate.of(2019, 1, 1),
      name = "claim-date-test"
    )

    // base claimDates: first should be day1
    assertEquals(base.claimDates.head, base.day1)

end ScenarioSuite
