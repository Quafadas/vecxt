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
    // exhaust threshold = 10 - 0.01 = 9.99
    // Iterations 1, 3, 5 exceed 9.99, so exhaustion = 3/5 = 0.6
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

end LossReportSuite
