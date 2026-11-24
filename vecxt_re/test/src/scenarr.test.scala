package vecxt_re

import vecxt.all.*
import munit.FunSuite
import java.time.{LocalDate, Month}

class ScenarrSuite extends FunSuite:

  test("constructor should enforce array length equality") {
    intercept[AssertionError] {
      Scenarr.withGeneratedIds(Array(1), Array(1, 2), Array(1.0), 2)
    }
  }

  test("freq, meanFreq, agg computed correctly for sorted scenario") {
    val iterations = Array(1, 1, 1, 2, 3)
    val days = Array(1, 2, 3, 4, 5)
    val amounts = Array(10.0, 20.0, 30.0, 40.0, 50.0)
    val sc = Scenarr.withGeneratedIds(iterations, days, amounts, numberIterations = 3, isSorted = true)

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
    val sc = Scenarr.withGeneratedIds(iterations, days, amounts, numberIterations = 3)

    val sortedScen = sc.sorted

    // freq = [3,1,1]
    val freqArr = sortedScen.freq
    val stats = freqArr.meanAndVariance(VarianceMode.Sample)
    val m = stats.mean
    val v = stats.variance
    val expectedCluster = (v - m) / (m * m)
    val expectedVMR = v / m

    assertEqualsDouble(sortedScen.clusterCoeff, expectedCluster, 1e-6)
    assertEqualsDouble(sortedScen.varianceMeanRatio, expectedVMR, 1e-6)
  }

  test("claimDates and monthYear mapping") {
    val days = Array(1, 2)
    val sc = Scenarr.withGeneratedIds(Array(1, 1), days, Array(10.0, 20.0), numberIterations = 1)
    val claimDates = sc.claimDates
    assertEquals(claimDates(0), LocalDate.of(2019, 1, 1))
    assertEquals(claimDates(1), LocalDate.of(2019, 1, 2))

    val my = sc.monthYear
    assertEquals(my(0).month, Month.JANUARY)
    assertEquals(my(0).year, 2019)
  }

  test("numSeasons accounts for days spanning multiple years") {
    val sc = Scenarr.withGeneratedIds(Array(1, 1), Array(1, 400), Array(1.0, 2.0), numberIterations = 1)
    println(sc.numSeasons)
    assertEquals(sc.numSeasons, 2)
  }

  test("itrDayAmount and period produce expected tuples") {
    val days = Array(10, 100, 365, 366)
    val sc = Scenarr.withGeneratedIds(Array(1, 1, 1, 1), days, Array(5.0, 6.0, 7.0, 8.0), numberIterations = 1)
    val itda = sc.itrDayAmount
    assertVecEquals(itda.map(_.itr), Array(1, 1, 1, 1))
    assertVecEquals(itda.map(_.day), days)
    assertVecEquals(itda.map(_.amnt), Array(5.0, 6.0, 7.0, 8.0))

    val (firstLoss, lastLoss) = sc.period
    assertEquals(firstLoss, LocalDate.of(2019, 1, 10))
    assertEquals(lastLoss, LocalDate.of(2020, 1, 1)) // day 366 -> Jan 1 2020 from 2019-01-01
  }

  test("hasOccurence false for empty amounts") {
    val sc =
      Scenarr.withGeneratedIds(Array.emptyIntArray, Array.emptyIntArray, Array.emptyDoubleArray, numberIterations = 0)
    assertEquals(sc.hasOccurence, false)
  }

  test("sorted extension reorders by iteration then day and sets isSorted") {
    val iter = Array(2, 1, 2)
    val days = Array(10, 5, 8)
    val amts = Array(20.0, 10.0, 15.0)
    val sc = Scenarr.withGeneratedIds(iter, days, amts, numberIterations = 2, isSorted = false)

    val ssorted = sc.sorted
    assertEquals(ssorted.isSorted, true)
    assertEquals(ssorted.iterations.toList, Array(1, 2, 2).toList)
    assertEquals(ssorted.days.toList, Array(5, 8, 10).toList)
    assertEquals(ssorted.amounts.toList, Array(10.0, 15.0, 20.0).toList)
  }

  /** Reference: the original stable comparison sort by (iteration, day) */
  private def referencePerm(it: Array[Int], d: Array[Int]): List[Int] =
    it.indices.sortBy(i => (it(i), d(i))).toList

  private val sortShapes: Seq[(String, Int, scala.util.Random => (Int, Int))] = Seq(
    ("dense iterations, days over a year", 2000, r => (1 + r.nextInt(100), 1 + r.nextInt(365))),
    ("one iteration, many events", 3000, r => (1, 1 + r.nextInt(365))),
    ("sparse iterations over 10m", 40, r => (1 + r.nextInt(10000000), 1 + r.nextInt(365))),
    ("sparse days", 40, r => (1 + r.nextInt(5), r.nextInt(Int.MaxValue))),
    ("negative and zero keys", 500, r => (r.nextInt(7) - 3, r.nextInt(11) - 5)),
    ("heavy ties for stability", 1000, r => (1 + r.nextInt(3), 1 + r.nextInt(2))),
    ("extreme Int keys", 200, r => (if r.nextBoolean() then Int.MinValue else Int.MaxValue, r.nextInt(3))),
    ("two events", 2, r => (1 + r.nextInt(2), 1 + r.nextInt(2))),
    ("one event", 1, r => (1, 1)),
    ("no events", 0, r => (1, 1))
  )

  test("sortPermutation and the merge-sort fallback match a stable sort by (iteration, day)") {
    for (label, n, gen) <- sortShapes; seed <- 1 to 3 do
      val r = scala.util.Random(seed * 31 + n)
      val pairs = Array.fill(n)(gen(r))
      val it = pairs.map(_._1)
      val d = pairs.map(_._2)
      val expected = referencePerm(it, d)
      assertEquals(Scenarr.sortPermutation(it, d).toList, expected, s"sortPermutation: $label, seed $seed")
      assertEquals(Scenarr.mergeSortPermutation(it, d).toList, expected, s"mergeSort: $label, seed $seed")
    end for
  }

  test("sorted reorders every column consistently and keeps tied events in their original order") {
    val r = scala.util.Random(7)
    val n = 3000
    val it = Array.fill(n)(1 + r.nextInt(50))
    val d = Array.fill(n)(1 + r.nextInt(4)) // many (iteration, day) ties
    val amts = Array.tabulate(n)(_.toDouble)
    val ids = Array.tabulate(n)(_.toLong)
    val sc = Scenarr(it, d, amts, ids, numberIterations = 50)

    val s = sc.sorted
    val perm = referencePerm(it, d)
    assert(s.isSorted)
    assertEquals(s.iterations.toList, perm.map(it))
    assertEquals(s.days.toList, perm.map(d))
    assertEquals(s.amounts.toList, perm.map(amts))
    assertEquals(s.ids.toList, perm.map(_.toLong))
    assertEquals(s.id, sc.id)
    assertEquals(sc.iterations.toList, it.toList, "input left untouched")
  }

  test("sorted skips the work for sorted input: flagged returns itself, unflagged-but-ordered shares arrays") {
    val sc = Scenarr.withGeneratedIds(Array(1, 1, 2), Array(3, 3, 1), Array(1.0, 2.0, 3.0), numberIterations = 2)
    val s = sc.sorted
    assert(s.isSorted)
    assert(s.iterations eq sc.iterations)
    assert(s.amounts eq sc.amounts)
    assert(s.sorted eq s)
  }

  test("freq, agg and oep refuse an unsorted scenario") {
    // Iteration 1 is split across two runs: the unchecked group kernels would silently drop the first run.
    val sc = Scenarr.withGeneratedIds(Array(1, 2, 1), Array(1, 2, 3), Array(1.0, 1.0, 1.0), numberIterations = 2)
    intercept[AssertionError](sc.freq)
    intercept[AssertionError](sc.agg)
    intercept[AssertionError](sc.oep)
    assertEquals(sc.sorted.agg.toList, List(2.0, 1.0))
  }

  test("oep counts iterations with no events as 0") {
    val sc = Scenarr.withGeneratedIds(
      Array(1, 1, 3),
      Array(1, 2, 3),
      Array(5.0, 7.0, 3.0),
      numberIterations = 4,
      isSorted = true
    )
    assertEquals(sc.oep.amounts.toList, List(7.0, 3.0, 0.0, 0.0))
    assertEquals(sc.oep.returnPeriods.toList, List(4.0, 2.0, 4.0 / 3.0, 1.0))
    assertEquals(sc.oep.points.map(_.amount).toList, List(7.0, 3.0, 0.0, 0.0))
  }

  test("scaleAmntBy multiplies amounts and threshold") {
    val sc =
      Scenarr.withGeneratedIds(Array(1, 1), Array(1, 2), Array(10.0, 20.0), numberIterations = 1, threshold = 100.0)
    val scaled = sc.scaleAmntBy(2.0)
    assertEquals(scaled.threshold, 200.0)
    assertEquals(scaled.amounts.toList, Array(20.0, 40.0).toList)
  }

  test("iteration selects events for given iteration number") {
    val iters = Array(2, 1, 2, 1)
    val days = Array(1, 2, 3, 4)
    val amts = Array(10.0, 11.0, 12.0, 13.0)
    val sc = Scenarr.withGeneratedIds(iters, days, amts, numberIterations = 2)
    val only2 = sc.iteration(2)
    assert(only2.iterations.forall(_ == 2))
    assertEquals(only2.amounts.toList, Array(10.0, 12.0).toList)
  }

  test("applyThreshold filters amounts and only allows increasing threshold") {
    val sc = Scenarr.withGeneratedIds(
      Array(1, 1, 1),
      Array(1, 2, 3),
      Array(10.0, 50.0, 200.0),
      numberIterations = 1,
      threshold = 0.0
    )
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

  test("applyThreshold keeps the sorted flag and id") {
    val sc = Scenarr
      .withGeneratedIds(
        Array(2, 1, 1, 2),
        Array(1, 2, 1, 3),
        Array(10.0, 50.0, 200.0, 5.0),
        numberIterations = 2
      )
      .sorted
    val filtered = sc.applyThreshold(7.0)
    assert(filtered.isSorted)
    assertEquals(filtered.id, sc.id)
    assertEquals(filtered.agg.toList, List(250.0, 10.0))

    val unsorted = Scenarr.withGeneratedIds(Array(2, 1), Array(1, 1), Array(10.0, 20.0), numberIterations = 2)
    assert(!unsorted.applyThreshold(1.0).isSorted)
  }

end ScenarrSuite
