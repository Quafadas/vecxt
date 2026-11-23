package vecxt_re

import munit.FunSuite

class SeverityTrendTest extends FunSuite:

  import TrendAnalysis.*

  private val years = Vector(
    2005, 2005, 2006, 2007, 2007, 2007, 2008, 2009, 2010, 2010, 2011, 2012, 2013, 2013, 2014, 2015, 2016, 2017, 2018,
    2018, 2019, 2020, 2021, 2022, 2023
  )
  private val losses = Vector(
    1.2e6, 3.4e6, 2.1e6, 1.5e6, 8.9e6, 1.1e6, 2.7e6, 1.9e6, 4.4e6, 1.3e6, 2.2e6, 6.1e6, 1.7e6, 3.0e6, 2.5e6, 1.4e6,
    5.2e6, 2.9e6, 3.8e6, 1.6e6, 7.5e6, 2.4e6, 4.9e6, 3.3e6, 12.0e6
  )

  /** Reference from R:
    * {{{
    * m <- lm(log(losses) ~ years); summary(m)
    *                Estimate        Std. Error         t value     Pr(>|t|)
    * (Intercept) -82.1431040763408  43.7877593480055  -1.87593759761728  0.0734127554647369
    * years         0.0481924822358   0.0217519747888   2.21554514951602  0.0368987781221027
    * sigma 0.599250127358724, R² 0.175882459879829
    * exp(confint(m)["years",]) - 1 : 0.00320020380081631 0.09767013057694296
    * predict(m, data.frame(years=2010), interval="confidence", level=0.9)
    *   14.7237852176462 14.4891870864765 14.9583833488159
    * }}}
    */
  test("fitSeverityTrend matches R lm on log losses") {
    val r = fitSeverityTrend(years, losses)
    assertEquals(r.nObs, 25)
    assertEquals(r.dfResidual, 23)
    assertEqualsDouble(r.intercept, -82.1431040763408, 1e-8)
    assertEqualsDouble(r.slope, 0.0481924822358144, 1e-11)
    assertEqualsDouble(r.seIntercept, 43.7877593480055, 1e-8)
    assertEqualsDouble(r.seSlope, 0.0217519747888423, 1e-11)
    assertEqualsDouble(r.tIntercept, -1.87593759761728, 1e-9)
    assertEqualsDouble(r.tSlope, 2.21554514951602, 1e-9)
    assertEqualsDouble(r.pValueIntercept, 0.0734127554647369, 1e-9)
    assertEqualsDouble(r.pValueSlope, 0.0368987781221027, 1e-9)
    assertEqualsDouble(r.sigma, 0.599250127358724, 1e-11)
    assertEqualsDouble(r.rSquared, 0.175882459879829, 1e-11)
  }

  test("annual trend and confidence interval match R") {
    val r = fitSeverityTrend(years, losses)
    assertEqualsDouble(r.annualTrend, math.exp(0.0481924822358144) - 1, 1e-11)
    val ci = r.annualTrendCI()
    assertEqualsDouble(ci.lower, 0.00320020380081631, 1e-9)
    assertEqualsDouble(ci.upper, 0.09767013057694296, 1e-9)
  }

  test("fittedLog confidence band matches R predict") {
    val f = fitSeverityTrend(years, losses).fittedLog(2010, alpha = 0.10)
    assertEqualsDouble(f.fit, 14.7237852176462, 1e-9)
    assertEqualsDouble(f.lower, 14.4891870864765, 1e-9)
    assertEqualsDouble(f.upper, 14.9583833488159, 1e-9)
  }

  test("significance depends on alpha") {
    val r = fitSeverityTrend(years, losses)
    assert(r.hasSignificantTrend()) // p ≈ 0.037 < 0.05
    assert(!r.hasSignificantTrend(alpha = 0.01))
  }

  test("trend is invariant to the currency unit") {
    val a = fitSeverityTrend(years, losses)
    val b = fitSeverityTrend(years, losses.map(_ / 1e6))
    assertEqualsDouble(b.slope, a.slope, 1e-12)
    assertEqualsDouble(b.seSlope, a.seSlope, 1e-12)
    assertEqualsDouble(b.pValueSlope, a.pValueSlope, 1e-12)
  }

  test("exact exponential growth is recovered") {
    val ys = (2000 to 2020).toVector
    val ls = ys.map(y => 1e6 * math.pow(1.05, y - 2000))
    val r = fitSeverityTrend(ys, ls)
    assertEqualsDouble(r.annualTrend, 0.05, 1e-10)
    assertEqualsDouble(r.rSquared, 1.0, 1e-10)
  }

  test("constant severity has no trend") {
    val ys = Vector(2000, 2001, 2002, 2003, 2004, 2005)
    val ls = Vector(2.0, 3.0, 2.0, 2.0, 3.0, 2.0) // palindromic about the mean year
    val r = fitSeverityTrend(ys, ls)
    assertEqualsDouble(r.slope, 0.0, 1e-12)
    assertEqualsDouble(r.pValueSlope, 1.0, 1e-12)
    assert(!r.hasSignificantTrend())
  }

  test("input validation") {
    intercept[IllegalArgumentException](fitSeverityTrend(Vector(2000, 2001), Vector(1.0, 2.0)))
    intercept[IllegalArgumentException](fitSeverityTrend(Vector(2000, 2001, 2002), Vector(1.0, 2.0)))
    intercept[IllegalArgumentException](fitSeverityTrend(Vector(2000, 2001, 2002), Vector(1.0, 0.0, 2.0)))
    intercept[IllegalArgumentException](fitSeverityTrend(Vector(2000, 2001, 2002), Vector(1.0, Double.NaN, 2.0)))
    intercept[IllegalArgumentException](fitSeverityTrend(Vector(2000, 2000, 2000), Vector(1.0, 2.0, 3.0)))
  }

  test("summary mentions the annual trend") {
    val s = fitSeverityTrend(years, losses).summary
    assert(s.contains("log(Severity) ~ 1 + Year"))
    assert(s.contains("Annual trend in median severity: 4.94%"), s)
  }

  /** Reference from R: sapply(seq_along(years), function(i) coef(lm(log(losses[-i]) ~ years[-i]))[2]) */
  test("leave-one-out slopes match R") {
    val loo = fitSeverityTrendLeaveOneOut(years, losses)
    assertEquals(loo.length, years.length)
    val slopes = loo.map(_.get.slope)
    assertEqualsDouble(slopes(24), 0.0331570643432764, 1e-11) // without the largest loss
    assertEqualsDouble(slopes.min, 0.0331570643432764, 1e-11)
    assertEquals(slopes.indexOf(slopes.max), 4)
    assertEqualsDouble(slopes.max, 0.0606050964430797, 1e-11)
    assert(!loo(24).get.hasSignificantTrend(), "one loss carries the significance")
  }

  test("leave-one-out is None where the remainder cannot be fitted") {
    // dropping the only 2001 loss leaves a single year
    val loo = fitSeverityTrendLeaveOneOut(Vector(2000, 2000, 2000, 2001), Vector(1.0, 2.0, 3.0, 4.0))
    assert(loo.take(3).forall(_.isDefined))
    assert(loo(3).isEmpty)
    // three losses: two remain after dropping one
    assert(fitSeverityTrendLeaveOneOut(Vector(2000, 2001, 2002), Vector(1.0, 2.0, 3.0)).forall(_.isEmpty))
  }

end SeverityTrendTest
