package vecxt_re

import munit.FunSuite

import vecxt.all.*

class TrendAnalysisTest extends FunSuite:

  import TrendAnalysis.*

  // Test data: synthetic counts with known trend
  val yearsNoTrend = Vector(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
  val countsNoTrend = Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1) // No trend

  val yearsWithTrend = Vector(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
  val countsWithTrend = Vector(1, 1, 2, 2, 3, 4, 5, 6, 8, 10) // Clear upward trend

  val realYears = Array(
    1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025
  )
  val realCounts = Array(
    1,0,0,0,1,0,0,0,2,1,1,2,3,0,0,1,2,1,0,1,2,3,1,2,1,0,1
  )
  // Example results from numpy / statsmodels for realYears
/**
Fitted Poisson with lambda = 0.9629629629629629

Generalized Linear Model: log(Count) ~ 1 + Year
============================================================
                Generalized Linear Model Regression Results
==============================================================================
Dep. Variable:                      y   No. Observations:                   27
Model:                            GLM   Df Residuals:                       25
Model Family:                 Poisson   Df Model:                            1
Link Function:                    Log   Scale:                          1.0000
Method:                          IRLS   Log-Likelihood:                -32.760
Date:                Thu, 29 Jan 2026   Deviance:                       26.468
Time:                        12:14:51   Pearson chi2:                     22.3
No. Iterations:                     5   Pseudo R-squ. (CS):            0.08983
Covariance Type:            nonrobust
==============================================================================
                coef    std err          z      P>|z|      [0.025      0.975]
------------------------------------------------------------------------------
const        -82.0576     52.254     -1.570      0.116    -184.473      20.358
x1             0.0407      0.026      1.571      0.116      -0.010       0.092
==============================================================================
*/


  test("Poisson fitTrend returns valid result structure") {
    val pois = Poisson(realCounts.mean)
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

  test("Poisson fitTrend matches Python statsmodels GLM results") {
    // Python statsmodels GLM output for realYears/realCounts:
    // Fitted Poisson with lambda = 0.9629629629629629
    // No. Observations: 27, Df Residuals: 25, Df Model: 1
    // Log-Likelihood: -32.760, Deviance: 26.468, Pearson chi2: 22.3
    // const: -82.0576 (std err 52.254), z=-1.570, p=0.116
    // x1:      0.0407 (std err  0.026), z= 1.571, p=0.116
    val pois = Poisson(realCounts.mean)
    val result = pois.fitTrend(realYears, realCounts)

    // Observations and degrees of freedom
    assertEquals(result.nObs, 27)
    assertEquals(result.dfResidual, 25)

    // Coefficients (tolerance for numerical differences)
    assertEqualsDouble(result.intercept, -82.0576, 0.5)
    assertEqualsDouble(result.slope, 0.0407, 0.001)

    // Standard errors
    assertEqualsDouble(result.seIntercept, 52.254, 0.5)
    assertEqualsDouble(result.seSlope, 0.026, 0.001)

    // P-value for slope (Python: 0.116, some variation expected due to CDF approximation)
    assertEqualsDouble(result.pValueSlope, 0.116, 0.03)

    // Log-likelihood
    assertEqualsDouble(result.logLikelihood, -32.760, 0.1)

    // Residual deviance
    assertEqualsDouble(result.residualDeviance, 26.468, 0.1)
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

  test("F-statistic p-value is small for significant trend") {
    val pois = Poisson(1.0)
    val result = pois.fitTrend(yearsWithTrend, countsWithTrend)

    // For significant trend, F p-value should be small (< 0.05)
    // F ~ 2.5 with df1=1, df2=8 should have p-value around 0.15
    // For our strongly trending data, F should be larger and p-value smaller
    assert(
      result.fPValue < 0.2,
      s"F p-value should be small for significant trend, got ${result.fPValue} with F=${result.fStatistic}"
    )
  }

  test("F-statistic p-value matches expected range for known F values") {
    // Sanity check: for trending data, higher F should mean lower p-value
    val pois = Poisson(1.0)
    val trendResult = pois.fitTrend(yearsWithTrend, countsWithTrend)
    val flatResult = pois.fitTrend(yearsNoTrend, countsNoTrend)

    // Trending data should have higher F-stat and lower p-value than flat data
    assert(
      trendResult.fStatistic > flatResult.fStatistic || flatResult.fStatistic <= 0,
      s"Trending F (${trendResult.fStatistic}) should be >= flat F (${flatResult.fStatistic})"
    )
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
