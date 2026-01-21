package vecxt_re

package vecxt_re

import munit.FunSuite

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