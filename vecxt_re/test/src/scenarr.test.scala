package vecxt_re

import vecxt.all.*
import munit.FunSuite
import java.time.{LocalDate, Month}
import scala.util.Random

class ScenarrSuite extends FunSuite:

  test("constructor should enforce array length equality") {
    intercept[AssertionError] {
      Scenarr(Array(1), Array(1, 2), Array(1.0))
    }
  }

  test("freq, meanFreq, agg computed correctly for sorted scenario") {
    val iterations = Array(1, 1, 1, 2, 3)
    val days = Array(1, 2, 3, 4, 5)
    val amounts = Array(10.0, 20.0, 30.0, 40.0, 50.0)
    val sc = Scenarr(iterations, days, amounts, numberIterations = 3, isSorted = true)

    // Expected counts per iteration 1..3 => [3,1,1]
    val expectedFreq = Array(3, 1, 1)
    assertEquals(sc.freq.toList, expectedFreq.toList)

    val expectedMean = expectedFreq.sum.toDouble / expectedFreq.length
    assert(math.abs(sc.meanFreq - expectedMean) < 1e-12)

    // Agg: sum amounts per iteration: iter1 -> 10+20+30 = 60, iter2 -> 40, iter3 -> 50
    val expectedAgg = Array(60.0, 40.0, 50.0)
    assertEquals(sc.agg.toList, expectedAgg.toList)

    // meanLoss = amounts.sum / numberIterations = 150 / 3 = 50
    assert(math.abs(sc.meanLoss - 50.0) < 1e-12)
  }

  test("clusterCoeff and varianceMeanRatio compute from sample variance") {
    val iterations = Array(1, 2, 1, 3, 1)
    val days = Array(1, 2, 3, 4, 5)
    val amounts = Array(10.0, 20.0, 30.0, 40.0, 50.0)
    val sc = Scenarr(iterations, days, amounts, numberIterations = 3)

    val sortedScen = sc.sorted

    // freq = [3,1,1]
    val freqArr = sortedScen.freq
    val (m, v) = freqArr.meanAndVariance(VarianceMode.Sample)
    val expectedCluster = (v - m) / (m * m)
    val expectedVMR = v / m

    assertEqualsDouble(sortedScen.clusterCoeff, expectedCluster, 1e-6)
    assertEqualsDouble(sortedScen.varianceMeanRatio, expectedVMR, 1e-6)
  }

  test("claimDates and monthYear mapping") {
    val days = Array(1, 2)
    val sc = Scenarr(Array(1, 1), days, Array(10.0, 20.0), numberIterations = 1)
    val claimDates = sc.claimDates
    assertEquals(claimDates(0), LocalDate.of(2019, 1, 1))
    assertEquals(claimDates(1), LocalDate.of(2019, 1, 2))

    val my = sc.monthYear
    assertEquals(my(0).month, Month.JANUARY)
    assertEquals(my(0).year, 2019)
  }

  test("numSeasons accounts for days spanning multiple years") {
    val sc = Scenarr(Array(1, 1), Array(1, 400), Array(1.0, 2.0), numberIterations = 1)
    println(sc.numSeasons)
    assertEquals(sc.numSeasons, 2)
  }

  test("itrDayAmount and period produce expected tuples") {
    val days = Array(10, 100, 365, 366)
    val sc = Scenarr(Array(1, 1, 1, 1), days, Array(5.0, 6.0, 7.0, 8.0), numberIterations = 1)
    val (itr, d, a) = sc.itrDayAmount
    assertEquals(itr.toList, Array(1, 1, 1, 1).toList)
    assertEquals(d.toList, days.toList)
    assertEquals(a.toList, Array(5.0, 6.0, 7.0, 8.0).toList)

    val (firstLoss, lastLoss) = sc.period
    assertEquals(firstLoss, LocalDate.of(2019, 1, 10))
    assertEquals(lastLoss, LocalDate.of(2020, 1, 1)) // day 366 -> Jan 1 2020 from 2019-01-01
  }

  test("hasOccurence false for empty amounts") {
    val sc = Scenarr(Array.emptyIntArray, Array.emptyIntArray, Array.emptyDoubleArray, numberIterations = 0)
    assertEquals(sc.hasOccurence, false)
  }

  test("sorted extension reorders by iteration then day and sets isSorted") {
    val iter = Array(2, 1, 2)
    val days = Array(10, 5, 8)
    val amts = Array(20.0, 10.0, 15.0)
    val sc = Scenarr(iter, days, amts, numberIterations = 2, isSorted = false)

    val ssorted = sc.sorted
    assertEquals(ssorted.isSorted, true)
    assertEquals(ssorted.iterations.toList, Array(1, 2, 2).toList)
    assertEquals(ssorted.days.toList, Array(5, 8, 10).toList)
    assertEquals(ssorted.amounts.toList, Array(10.0, 15.0, 20.0).toList)
  }

  test("scaleAmntBy multiplies amounts and threshold") {
    val sc = Scenarr(Array(1, 1), Array(1, 2), Array(10.0, 20.0), numberIterations = 1, threshold = 100.0)
    val scaled = sc.scaleAmntBy(2.0)
    assertEquals(scaled.threshold, 200.0)
    assertEquals(scaled.amounts.toList, Array(20.0, 40.0).toList)
  }

  test("iteration selects events for given iteration number") {
    val iters = Array(2, 1, 2, 1)
    val days = Array(1, 2, 3, 4)
    val amts = Array(10.0, 11.0, 12.0, 13.0)
    val sc = Scenarr(iters, days, amts, numberIterations = 2)
    val only2 = sc.iteration(2)
    assert(only2.iterations.forall(_ == 2))
    assertEquals(only2.amounts.toList, Array(10.0, 12.0).toList)
  }

  test("applyThreshold filters amounts and only allows increasing threshold") {
    val sc = Scenarr(Array(1, 1, 1), Array(1, 2, 3), Array(10.0, 50.0, 200.0), numberIterations = 1, threshold = 0.0)
    val filtered = sc.applyThreshold(49.0)
    // keep > 49 => 50 and 200
    assertEquals(filtered.amounts.toList, Array(50.0, 200.0).toList)
    assertEquals(filtered.threshold, 49.0)

    intercept[Exception] {
      sc.applyThreshold(0.0) // not strictly greater
    }
    intercept[Exception] {
      sc.applyThreshold(-1.0) // decreasing
    }
  }

end ScenarrSuite
