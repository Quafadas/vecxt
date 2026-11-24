package vecxt_re

import munit.FunSuite
import ReReporting.*

class LossCalcSuite extends FunSuite:

  test("ReportDenominator.FirstLimit uses occLimit when present") {
    val layer = Layer(occLimit = Some(10.0), aggLimit = Some(20.0))
    assertEqualsDouble(ReportDenominator.FirstLimit.fromlayer(layer), 10.0, 0.0)
  }

  test("ReportDenominator.FirstLimit falls back to aggLimit when occLimit missing") {
    val layer = Layer(occLimit = None, aggLimit = Some(30.0))
    assertEqualsDouble(ReportDenominator.FirstLimit.fromlayer(layer), 30.0, 0.0)
  }

  test("ReportDenominator.FirstLimit returns PositiveInfinity when no limits") {
    val layer = Layer()
    assertEqualsDouble(ReportDenominator.FirstLimit.fromlayer(layer), Double.PositiveInfinity, 0.0)
  }

  test("ReportDenominator.AggLimit returns aggLimit when present") {
    val layer = Layer(aggLimit = Some(40.0))
    assertEqualsDouble(ReportDenominator.AggLimit.fromlayer(layer), 40.0, 0.0)
  }

  test("ReportDenominator.AggLimit returns PositiveInfinity when aggLimit missing") {
    val layer = Layer(aggLimit = None)
    assertEqualsDouble(ReportDenominator.AggLimit.fromlayer(layer), Double.PositiveInfinity, 0.0)
  }

  test("ReportDenominator.Custom returns provided denominator") {
    val layer = Layer()
    assertEqualsDouble(ReportDenominator.Custom(55.5).fromlayer(layer), 55.5, 0.0)
  }
end LossCalcSuite

class LossReportSuite extends FunSuite:

  test("lossReport computes EL correctly") {
    // 5 iterations, total loss = 10 + 0 + 20 + 5 + 15 = 50
    // EL = 50 / 5 = 10
    val layer = Layer(occLimit = Some(100.0))
    val years = Array(1, 1, 3, 4, 5, 5)
    val cededToLayer = Array(5.0, 5.0, 20.0, 5.0, 10.0, 5.0)
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val numIterations = 5
    val reportLimit = ReportDenominator.FirstLimit.fromlayer(layer)
    val report = calcd.lossReport(numIterations, years, ReportDenominator.FirstLimit)

    // Total = 10 + 20 + 5 + 15 = 50, EL = 50/5 = 10, normalized = 10/100 = 0.1
    assertEqualsDouble(report.el, 0.1, 0.0001, "EL should be 10/100 = 0.1")
    // Compare against single metric calculation
    val singleMetricEL = calcd.expectedLoss(numIterations) / reportLimit
    assertEqualsDouble(report.el, singleMetricEL, 0.0001, "lossReport EL should match expectedLoss")
  }

  test("lossReport computes attachment probability correctly") {
    // 5 iterations: iter 1 has loss, iter 2 has 0 loss, iter 3 has loss, iter 4 has loss, iter 5 has loss
    // Attachment = 4/5 = 0.8
    val layer = Layer(occLimit = Some(100.0))
    val years = Array(1, 1, 3, 4, 5, 5)
    val cededToLayer = Array(5.0, 5.0, 20.0, 5.0, 10.0, 5.0)
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val numIterations = 5
    val report = calcd.lossReport(numIterations, years, ReportDenominator.FirstLimit)

    // Iterations 1,3,4,5 have losses, iteration 2 has 0
    assertEqualsDouble(report.attachProb, 0.8, 0.0001, "Attachment probability should be 4/5 = 0.8")
    // Compare against single metric calculation
    val singleMetricAttach = calcd.attachmentProbability(numIterations, years)
    assertEqualsDouble(
      report.attachProb,
      singleMetricAttach,
      0.0001,
      "lossReport attachProb should match attachmentProbability"
    )
  }

  test("lossReport computes exhaustion probability correctly") {
    // Layer with aggLimit of 10, 5 iterations
    // iter 1: 10 (exhausted), iter 2: 0, iter 3: 20 (exhausted), iter 4: 5, iter 5: 15 (exhausted)
    // Exhaustion = 3/5 = 0.6
    val layer = Layer(occLimit = Some(100.0), aggLimit = Some(10.0))
    val years = Array(1, 1, 3, 4, 5, 5)
    val cededToLayer = Array(5.0, 5.0, 20.0, 5.0, 10.0, 5.0)
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val numIterations = 5
    val report = calcd.lossReport(numIterations, years, ReportDenominator.FirstLimit)

    // Per-iteration sums: 1->10, 2->0, 3->20, 4->5, 5->15
    // exhaust threshold = aggLimit * share = 10 (less a 1e-9 relative tolerance)
    // Iterations 1, 3, 5 reach 10, so exhaustion = 3/5 = 0.6
    assertEqualsDouble(report.exhaustProb, 0.6, 0.0001, "Exhaustion probability should be 3/5 = 0.6")
    // Compare against single metric calculation
    val singleMetricExhaust = calcd.exhaustionProbability(numIterations, years)
    assertEqualsDouble(
      report.exhaustProb,
      singleMetricExhaust,
      0.0001,
      "lossReport exhaustProb should match exhaustionProbability"
    )
  }

  /** exhaustProb from lossReport, after checking it agrees with the single-metric exhaustionProbability */
  private def exhaustProb(
      layer: Layer,
      years: Array[Int],
      ceded: Array[Double],
      numIterations: Int
  ): Double =
    val calcd = (layer = layer, cededToLayer = ceded)
    val report = calcd.lossReport(numIterations, years, ReportDenominator.Custom(1.0))
    assertEqualsDouble(
      report.exhaustProb,
      calcd.exhaustionProbability(numIterations, years),
      0.0,
      "lossReport exhaustProb should match exhaustionProbability"
    )
    report.exhaustProb
  end exhaustProb

  test("exhaustion probability uses aggLimit * share, given ceded amounts after share") {
    // aggLimit 10 at 50% share: the most this layer can cede in a year is 5
    val layer = Layer(aggLimit = Some(10.0), share = 0.5)
    // Per-iteration sums: 1 -> 5 (exhausted), 2 -> 0, 3 -> 5 (exhausted), 4 -> 4.9
    val years = Array(1, 1, 3, 4)
    val ceded = Array(2.5, 2.5, 5.0, 4.9)
    assertEqualsDouble(exhaustProb(layer, years, ceded, 4), 0.5, 0.0)
  }

  test("exhaustion probability with share, end to end through splitAmntFast") {
    import SplitLosses.*
    // losses: iteration 1 -> 6 + 6 = 12, iteration 2 -> 4, iteration 3 -> 30, iteration 4 -> none
    val years = Array(1, 1, 2, 3)
    val losses = Array(6.0, 6.0, 4.0, 30.0)

    // Retention agg terms: 10 agg limit, 25% share => yearly ceded 2.5, 1.0, 2.5, 0
    val retLayer = Layer(aggLimit = Some(10.0), share = 0.25)
    // 2 xs agg retention: (12 - 2) = 10 -> 2.5 exhausted, (4 - 2) = 2 -> 0.5, (30 - 2) capped at 10 -> 2.5 exhausted
    val aggRetLayer = Layer(aggLimit = Some(10.0), aggRetention = Some(2.0), share = 0.25)
    // Franchise agg terms: from 5 pays min(x, 10 + 5) => 12 -> 12 (not exhausted), 4 -> 0, 30 -> 15 (exhausted)
    val franchiseLayer =
      Layer(aggLimit = Some(10.0), aggRetention = Some(5.0), aggType = DeductibleType.Franchise, share = 0.6)

    val (_, _, splits) = Tower(IndexedSeq(retLayer, aggRetLayer, franchiseLayer)).splitAmntFast(years, losses)

    def yearly(ceded: Array[Double]) =
      Array.tabulate(4)(it => ceded.indices.filter(i => years(i) == it + 1).map(ceded).sum)

    for (layer, ceded) <- splits.take(2) do
      val y = yearly(ceded)
      assert(y.forall(_ <= 10.0 * layer.share + 1e-12), s"ceded above aggLimit * share: ${y.toList}")
      assertEqualsDouble(exhaustProb(layer, years, ceded, 4), 0.5, 0.0, s"share ${layer.share}")
    end for

    val franchiseCeded = splits(2).cededToLayer
    val fy = yearly(franchiseCeded)
    assertEqualsDouble(fy(0), 12.0 * 0.6, 1e-12)
    assertEqualsDouble(fy(1), 0.0, 0.0)
    assertEqualsDouble(fy(2), 15.0 * 0.6, 1e-12)
    assertEqualsDouble(exhaustProb(franchiseLayer, years, franchiseCeded, 4), 0.25, 0.0, "franchise, share 0.6")
  }

  test("exhaustion probability tolerates rounding in many small losses with share") {
    import SplitLosses.*
    // 1000 losses of 0.1 in a single iteration: the cumulative sum passes 10 with rounding error, is capped at 10, then
    // 30% share applied and diffed back per loss. Their sum is 3 up to rounding and must still count as exhausted.
    val n = 1000
    val years = Array.fill(n)(1)
    val losses = Array.fill(n)(0.1)
    val layer = Layer(aggLimit = Some(10.0), share = 0.3)
    val (_, _, splits) = Tower(IndexedSeq(layer)).splitAmntFast(years, losses)
    assertEqualsDouble(exhaustProb(layer, years, splits.head.cededToLayer, 1), 1.0, 0.0)
  }

  test("exhaustion probability with limits in large units counts loss-free iterations as not exhausted") {
    // e.g. figures in billions: aggLimit 0.005 = 5m. A fixed absolute tolerance of 0.01 would put the threshold below
    // zero and count iteration 2 (no loss) as exhausted.
    val layer = Layer(aggLimit = Some(0.005), share = 0.4)
    val years = Array(1)
    val ceded = Array(0.002)
    assertEqualsDouble(exhaustProb(layer, years, ceded, 2), 0.5, 0.0)
  }

  test("exhaustion probability is 0 without an aggregate limit") {
    val layer = Layer(occLimit = Some(10.0), share = 0.5)
    assertEqualsDouble(exhaustProb(layer, Array(1, 2), Array(5.0, 1e300), 2), 0.0, 0.0)
  }

  test("lossReport ratios are share independent for limit denominators; Custom is not share adjusted") {
    import SplitLosses.*
    val years = Array(1, 1, 2, 3, 3, 3, 5)
    val losses = Array(8.0, 9.0, 3.0, 20.0, 1.0, 6.0, 12.0)
    def layerAt(share: Double) =
      Layer(occRetention = Some(2.0), occLimit = Some(6.0), aggLimit = Some(10.0), share = share)

    def report(share: Double, denom: ReportDenominator) =
      val layer = layerAt(share)
      val (_, _, splits) = Tower(IndexedSeq(layer)).splitAmntFast(years, losses)
      (layer = layer, cededToLayer = splits.head.cededToLayer).lossReport(5, years, denom)
    end report

    for denom <- Seq(ReportDenominator.FirstLimit, ReportDenominator.AggLimit) do
      val full = report(1.0, denom)
      assert(full.el > 0 && full.stdDev > 0 && full.exhaustProb > 0, s"degenerate fixture: $full")
      for share <- Seq(0.4, 0.075) do
        val part = report(share, denom)
        assertEqualsDouble(part.el, full.el, 1e-12, s"$denom el at share $share")
        assertEqualsDouble(part.stdDev, full.stdDev, 1e-12, s"$denom stdDev at share $share")
        assertEqualsDouble(part.attachProb, full.attachProb, 0.0, s"$denom attachProb at share $share")
        assertEqualsDouble(part.exhaustProb, full.exhaustProb, 0.0, s"$denom exhaustProb at share $share")
        assertEqualsDouble(part.limit, full.limit, 0.0, s"$denom reported limit is the 100% limit")
      end for
    end for

    // Custom divides by exactly the given value, so it reports the ceded (post-share) figures
    val rawFull = report(1.0, ReportDenominator.Custom(1.0))
    val rawPart = report(0.4, ReportDenominator.Custom(1.0))
    assertEqualsDouble(rawPart.el, 0.4 * rawFull.el, 1e-12)
    assertEqualsDouble(rawPart.stdDev, 0.4 * rawFull.stdDev, 1e-12)
    assertEqualsDouble(rawPart.limit, 1.0, 0.0)
  }

  test("lossReport computes stdDev correctly") {
    // 5 iterations with per-iteration sums: 10, 0, 20, 5, 15
    // Mean = (10 + 0 + 20 + 5 + 15) / 5 = 50 / 5 = 10
    // Variance = ((10-10)^2 + (0-10)^2 + (20-10)^2 + (5-10)^2 + (15-10)^2) / 5
    //          = (0 + 100 + 100 + 25 + 25) / 5 = 250 / 5 = 50
    // StdDev = sqrt(50) ≈ 7.071
    val layer = Layer(occLimit = Some(100.0))
    val years = Array(1, 1, 3, 4, 5, 5)
    val cededToLayer = Array(5.0, 5.0, 20.0, 5.0, 10.0, 5.0)
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val numIterations = 5
    val reportLimit = ReportDenominator.FirstLimit.fromlayer(layer)
    val report = calcd.lossReport(numIterations, years, ReportDenominator.FirstLimit)

    val expectedStdDev = Math.sqrt(50.0) / 100.0 // normalized by limit
    assertEqualsDouble(report.stdDev, expectedStdDev, 0.0001, s"StdDev should be sqrt(50)/100 = $expectedStdDev")
    // Compare against single metric calculation
    val singleMetricStd = calcd.std(numIterations, years) / reportLimit
    assertEqualsDouble(report.stdDev, singleMetricStd, 0.0001, "lossReport stdDev should match std")
  }

  test("lossReport with all zero losses") {
    val layer = Layer(occLimit = Some(100.0), aggLimit = Some(10.0))
    val years = Array[Int]()
    val cededToLayer = Array[Double]()
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val numIterations = 5
    val reportLimit = ReportDenominator.FirstLimit.fromlayer(layer)
    val report = calcd.lossReport(numIterations, years, ReportDenominator.FirstLimit)

    assertEqualsDouble(report.el, 0.0, 0.0001, "EL should be 0")
    assertEqualsDouble(report.stdDev, 0.0, 0.0001, "StdDev should be 0")
    assertEqualsDouble(report.attachProb, 0.0, 0.0001, "Attachment probability should be 0")
    assertEqualsDouble(report.exhaustProb, 0.0, 0.0001, "Exhaustion probability should be 0")
    // Compare against single metric calculations
    assertEqualsDouble(
      report.el,
      calcd.expectedLoss(numIterations) / reportLimit,
      0.0001,
      "lossReport EL should match expectedLoss"
    )
    assertEqualsDouble(
      report.attachProb,
      calcd.attachmentProbability(numIterations, years),
      0.0001,
      "lossReport attachProb should match attachmentProbability"
    )
    assertEqualsDouble(
      report.exhaustProb,
      calcd.exhaustionProbability(numIterations, years),
      0.0001,
      "lossReport exhaustProb should match exhaustionProbability"
    )
    assertEqualsDouble(
      report.stdDev,
      calcd.std(numIterations, years) / reportLimit,
      0.0001,
      "lossReport stdDev should match std"
    )
  }

  test("lossReport returns correct layer name") {
    val layer = Layer(occLimit = Some(100.0), layerName = Some("Test Layer"))
    val years = Array(1)
    val cededToLayer = Array(10.0)
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val report = calcd.lossReport(1, years, ReportDenominator.FirstLimit)

    assertEquals(report.name, "Test Layer")
  }

  test("lossReport returns correct limit") {
    val layer = Layer(occLimit = Some(100.0), aggLimit = Some(200.0))
    val years = Array(1)
    val cededToLayer = Array(10.0)
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val report = calcd.lossReport(1, years, ReportDenominator.FirstLimit)

    assertEqualsDouble(report.limit, 100.0, 0.0001)
  }

  test("lossReport with single iteration") {
    val layer = Layer(occLimit = Some(50.0))
    val years = Array(1, 1, 1)
    val cededToLayer = Array(10.0, 15.0, 25.0) // Total = 50
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val numIterations = 1
    val reportLimit = ReportDenominator.FirstLimit.fromlayer(layer)
    val report = calcd.lossReport(numIterations, years, ReportDenominator.FirstLimit)

    assertEqualsDouble(report.el, 1.0, 0.0001, "EL should be 50/50 = 1.0")
    assertEqualsDouble(report.attachProb, 1.0, 0.0001, "Attachment should be 1.0")
    assertEqualsDouble(report.stdDev, 0.0, 0.0001, "StdDev should be 0 with single iteration")
    // Compare against single metric calculations
    assertEqualsDouble(
      report.el,
      calcd.expectedLoss(numIterations) / reportLimit,
      0.0001,
      "lossReport EL should match expectedLoss"
    )
    assertEqualsDouble(
      report.attachProb,
      calcd.attachmentProbability(numIterations, years),
      0.0001,
      "lossReport attachProb should match attachmentProbability"
    )
    assertEqualsDouble(
      report.stdDev,
      calcd.std(numIterations, years) / reportLimit,
      0.0001,
      "lossReport stdDev should match std"
    )
  }

  test("lossReport matches all single metrics for complex scenario") {
    // A more complex test case with many iterations and varied losses
    val layer = Layer(occLimit = Some(50.0), aggLimit = Some(30.0), layerName = Some("Complex Layer"))
    val years = Array(1, 1, 2, 3, 3, 3, 5, 7, 7, 10)
    val cededToLayer = Array(10.0, 5.0, 25.0, 8.0, 12.0, 5.0, 40.0, 3.0, 7.0, 15.0)
    val calcd = (layer = layer, cededToLayer = cededToLayer)
    val numIterations = 10
    val reportLimit = ReportDenominator.FirstLimit.fromlayer(layer)
    val report = calcd.lossReport(numIterations, years, ReportDenominator.FirstLimit)

    // Compare all metrics against single metric calculations
    val singleMetricEL = calcd.expectedLoss(numIterations) / reportLimit
    val singleMetricAttach = calcd.attachmentProbability(numIterations, years)
    val singleMetricExhaust = calcd.exhaustionProbability(numIterations, years)
    val singleMetricStd = calcd.std(numIterations, years) / reportLimit

    assertEqualsDouble(report.el, singleMetricEL, 0.0001, "lossReport EL should match expectedLoss")
    assertEqualsDouble(
      report.attachProb,
      singleMetricAttach,
      0.0001,
      "lossReport attachProb should match attachmentProbability"
    )
    assertEqualsDouble(
      report.exhaustProb,
      singleMetricExhaust,
      0.0001,
      "lossReport exhaustProb should match exhaustionProbability"
    )
    assertEqualsDouble(report.stdDev, singleMetricStd, 0.0001, "lossReport stdDev should match std")
    assertEquals(report.name, "Complex Layer")
    assertEqualsDouble(report.limit, 50.0, 0.0001)
  }

  test("lossReport rejects out-of-order years instead of dropping losses") {
    val calcd = (layer = Layer(occLimit = Some(100.0)), cededToLayer = Array(5.0, 1.0))
    val ex = intercept[IllegalArgumentException](calcd.lossReport(2, Array(2, 1), ReportDenominator.FirstLimit))
    assert(ex.getMessage.contains("non-decreasing"), ex.getMessage)
  }

  test("lossReport rejects 0-based years") {
    val calcd = (layer = Layer(occLimit = Some(100.0)), cededToLayer = Array(5.0, 1.0))
    val ex = intercept[IllegalArgumentException](calcd.lossReport(2, Array(0, 1), ReportDenominator.FirstLimit))
    assert(ex.getMessage.contains("1-based"), ex.getMessage)
  }

  test("lossReport rejects years beyond numIterations") {
    val calcd = (layer = Layer(occLimit = Some(100.0)), cededToLayer = Array(5.0, 1.0))
    val ex = intercept[IllegalArgumentException](calcd.lossReport(2, Array(1, 3), ReportDenominator.FirstLimit))
    assert(ex.getMessage.contains("1..2"), ex.getMessage)
  }

  test("lossReport rejects mismatched years and cededToLayer lengths") {
    val calcd = (layer = Layer(occLimit = Some(100.0)), cededToLayer = Array(5.0, 1.0, 2.0))
    val ex = intercept[IllegalArgumentException](calcd.lossReport(2, Array(1, 2), ReportDenominator.FirstLimit))
    assert(ex.getMessage.contains("same length"), ex.getMessage)
  }

  // Adjacent layers, contiguous per occurrence but capped only in aggregate (B: unl xs 2.1 occ, 0.8 agg;
  // A: unl xs 2.9 occ, 0.7 agg). B's agg limit can be used up by several events below 2.9, so
  // P(exhaust B) >= P(attach A), not ==.
  private val contigA = Layer(occRetention = Some(2.9), aggLimit = Some(0.7))
  private val contigB = Layer(occRetention = Some(2.1), aggLimit = Some(0.8))

  test("contiguous occ layers with agg limits: two events below A's retention exhaust B without attaching A") {
    import SplitLosses.*
    val years = Array(1, 1)
    val losses = Array(2.5, 2.5)
    val (ceded, retained, splits) = Tower(IndexedSeq(contigA, contigB)).splitAmntFast(years, losses)

    assertEquals(splits(0).cededToLayer.toSeq, Seq(0.0, 0.0))
    assertEqualsDouble(splits(1).cededToLayer.sum, 0.8, 1e-12)
    assertEqualsDouble(splits(0).lossReport(1, years, ReportDenominator.FirstLimit).attachProb, 0.0, 0.0)
    assertEqualsDouble(splits(1).lossReport(1, years, ReportDenominator.FirstLimit).exhaustProb, 1.0, 0.0)
    for i <- losses.indices do assertEqualsDouble(ceded(i) + retained(i), losses(i), 1e-12, s"no leak ($i)")
    end for
  }

  test("contiguous occ layers with agg limits: A attached implies B exhausted, per iteration") {
    import SplitLosses.*
    val rng = scala.util.Random(42)
    val n = 2000
    // Random losses in 1.5 .. 4.0, plus one final year with a single loss of exactly 2.9: it exhausts B without
    // attaching A (attachment needs ceded > 0), so the two probabilities differ even with one event per year.
    val years = Array.fill(5000)(rng.nextInt(n - 1) + 1).sorted :+ n
    val losses = Array.fill(years.length - 1)(1.5 + rng.nextDouble() * 2.5) :+ 2.9
    val (_, _, splits) = Tower(IndexedSeq(contigA, contigB)).splitAmntFast(years, losses)

    def yearly(ceded: Array[Double]) =
      val out = new Array[Double](n)
      for i <- ceded.indices do out(years(i) - 1) += ceded(i)
      end for
      out
    end yearly
    val aYear = yearly(splits(0).cededToLayer)
    val bYear = yearly(splits(1).cededToLayer)
    for i <- 0 until n do
      if aYear(i) > 0 then assert(bYear(i) > 0.8 - 1e-9, s"iteration ${i + 1}: A attached but B at ${bYear(i)}")
    end for
    assertEqualsDouble(aYear(n - 1), 0.0, 0.0)
    assertEqualsDouble(bYear(n - 1), 0.8, 1e-12)

    val ra = splits(0).lossReport(n, years, ReportDenominator.FirstLimit)
    val rb = splits(1).lossReport(n, years, ReportDenominator.FirstLimit)
    assert(rb.exhaustProb > ra.attachProb, s"exhaust B ${rb.exhaustProb} <= attach A ${ra.attachProb}")
  }

end LossReportSuite
