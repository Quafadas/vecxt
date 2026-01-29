package vecxt_re

import munit.FunSuite

class TrendAnalysisTest extends FunSuite:

  import TrendAnalysis.*

  // Test data: synthetic counts with known trend
  val yearsNoTrend = Vector(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
  val countsNoTrend = Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1) // No trend

  val yearsWithTrend = Vector(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
  val countsWithTrend = Vector(1, 1, 2, 2, 3, 4, 5, 6, 8, 10) // Clear upward trend

  // Example
  val realYears = Vector(
    1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
    2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017,
    2018, 2019, 2020, 2021, 2022, 2023, 2024
  )
  val realCounts = Vector(
    1, 0, 1, 0, 1, 0, 1, 2, 1, 0,
    0, 3, 1, 0, 0, 1, 1, 2, 1, 2,
    1, 3, 1, 1, 1, 0, 1
  )

  test("Poisson fitTrend returns valid result structure") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsNoTrend, countsNoTrend)

    assertEquals(result.nObs, 10)
    assertEquals(result.dfResidual, 8)
    assert(!result.intercept.isNaN, "intercept should not be NaN")
    assert(!result.slope.isNaN, "slope should not be NaN")
    assert(!result.seIntercept.isNaN, "seIntercept should not be NaN")
    assert(!result.seSlope.isNaN, "seSlope should not be NaN")
    assert(result.pValueSlope >= 0 && result.pValueSlope <= 1, "p-value should be in [0,1]")
  }

  test("Poisson fitTrend detects no significant trend in flat data") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsNoTrend, countsNoTrend)

    // Slope should be close to zero
    assert(math.abs(result.slope) < 0.1, s"slope should be near zero, got ${result.slope}")
    // p-value should be high (not significant)
    assert(result.pValueSlope > 0.1, s"p-value should be > 0.1 for no trend, got ${result.pValueSlope}")
    assert(!result.hasSignificantTrend(0.05), "should not detect significant trend in flat data")
  }

  test("Poisson fitTrend detects significant trend in increasing data") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsWithTrend, countsWithTrend)

    // Slope should be positive
    assert(result.slope > 0, s"slope should be positive, got ${result.slope}")
    // p-value should be low (significant)
    assert(result.pValueSlope < 0.05, s"p-value should be < 0.05 for clear trend, got ${result.pValueSlope}")
    assert(result.hasSignificantTrend(0.05), "should detect significant trend in increasing data")
  }

  test("Poisson fitTrend residual deviance less than null deviance for trending data") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsWithTrend, countsWithTrend)

    assert(
      result.residualDeviance < result.nullDeviance,
      s"residual deviance (${result.residualDeviance}) should be less than null deviance (${result.nullDeviance})"
    )
  }

  test("NegativeBinomial fitTrend returns valid result structure") {
    val nb = NegativeBinomial(a = 1.0, b = 1.0)
    val result = nb.fitTrend(yearsNoTrend, countsNoTrend)

    assertEquals(result.nObs, 10)
    assertEquals(result.dfResidual, 8)
    assert(!result.intercept.isNaN, "intercept should not be NaN")
    assert(!result.slope.isNaN, "slope should not be NaN")
    assert(!result.seIntercept.isNaN, "seIntercept should not be NaN")
    assert(!result.seSlope.isNaN, "seSlope should not be NaN")
    assert(result.pValueSlope >= 0 && result.pValueSlope <= 1, "p-value should be in [0,1]")
  }

  test("NegativeBinomial fitTrend detects no significant trend in flat data") {
    val nb = NegativeBinomial(a = 1.0, b = 1.0)
    val result = nb.fitTrend(yearsNoTrend, countsNoTrend)

    assert(math.abs(result.slope) < 0.1, s"slope should be near zero, got ${result.slope}")
    assert(!result.hasSignificantTrend(0.05), "should not detect significant trend in flat data")
  }

  test("NegativeBinomial fitTrend detects significant trend in increasing data") {
    val nb = NegativeBinomial(a = 2.0, b = 0.5)
    val result = nb.fitTrend(yearsWithTrend, countsWithTrend)

    assert(result.slope > 0, s"slope should be positive, got ${result.slope}")
    assert(result.hasSignificantTrend(0.05), "should detect significant trend in increasing data")
  }

  test("Poisson fitTrend on realistic data produces sensible coefficients") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(realYears, realCounts)

    assertEquals(result.nObs, 27)
    assertEquals(result.dfResidual, 25)

    // The image shows β₀ ≈ -91.887, β₁ ≈ 0.0456 for similar data
    // Our parameterization may differ slightly, but signs should match
    // Slope should be small and positive (slight upward trend)
    assert(result.slope > -0.1 && result.slope < 0.2, s"slope should be small, got ${result.slope}")

    // Check that summary doesn't throw
    val summary = result.summary
    assert(summary.nonEmpty, "summary should not be empty")
    assert(summary.contains("Coefficients"), "summary should contain 'Coefficients'")
    assert(summary.contains("Year"), "summary should contain 'Year'")
  }

  test("TrendFitResult summary formatting") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsWithTrend, countsWithTrend)
    val summary = result.summary

    // Check summary contains expected sections
    assert(summary.contains("Generalized Linear Model"), "should have model header")
    assert(summary.contains("(Intercept)"), "should show intercept")
    assert(summary.contains("Year"), "should show year coefficient")
    assert(summary.contains("Null Deviance"), "should show null deviance")
    assert(summary.contains("Residual Deviance"), "should show residual deviance")
    assert(summary.contains("AIC"), "should show AIC")
    assert(summary.contains("F-statistic"), "should show F-statistic")
  }

  test("fitTrend requires minimum 3 observations") {
    val pois = Poisson(1.0)

    intercept[IllegalArgumentException] {
      pois.fitTrend(Vector(2000, 2001), Vector(1, 2))
    }
  }

  test("fitTrend requires equal length years and counts") {
    val pois = Poisson(1.0)

    intercept[IllegalArgumentException] {
      pois.fitTrend(Vector(2000, 2001, 2002), Vector(1, 2))
    }
  }

  test("F-statistic p-value is valid") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsWithTrend, countsWithTrend)

    assert(result.fStatistic > 0, "F-statistic should be positive for trending data")
    assert(result.fPValue >= 0 && result.fPValue <= 1, "F p-value should be in [0,1]")
  }

  test("AIC is finite") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsWithTrend, countsWithTrend)

    assert(result.aic.isFinite, "AIC should be finite")
    assert(result.logLikelihood.isFinite, "log-likelihood should be finite")
  }

  test("Poisson and NegBin give similar results for low dispersion") {
    // When NegBin has high 'a' (low overdispersion), should approximate Poisson
    val pois = Poisson(1.0)
    val nb = NegativeBinomial(a = 100.0, b = 0.01) // High a = low overdispersion

    val poisResult = pois.fitTrend(yearsWithTrend, countsWithTrend)
    val nbResult = nb.fitTrend(yearsWithTrend, countsWithTrend)

    // Slopes should be in the same ballpark
    val slopeDiff = math.abs(poisResult.slope - nbResult.slope)
    assert(slopeDiff < 0.5, s"slopes should be similar: Poisson=${poisResult.slope}, NegBin=${nbResult.slope}")
  }

end TrendAnalysisTest
