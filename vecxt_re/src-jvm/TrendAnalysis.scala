package vecxt_re

import org.apache.commons.numbers.gamma.LogGamma
import org.apache.commons.numbers.gamma.RegularizedBeta
import org.apache.commons.statistics.distribution.ChiSquaredDistribution
import org.apache.commons.statistics.distribution.FDistribution
import org.apache.commons.statistics.distribution.NormalDistribution
import org.apache.commons.statistics.distribution.TDistribution

/** Result of fitting a GLM trend model: log(μ) = β₀ + β₁·year
  *
  * Contains coefficient estimates, standard errors, test statistics, and goodness-of-fit measures.
  *
  * @param nObs
  *   Number of observations
  * @param dfResidual
  *   Residual degrees of freedom (n - 2)
  * @param intercept
  *   Estimated intercept (β₀)
  * @param slope
  *   Estimated year coefficient (β₁)
  * @param seIntercept
  *   Standard error of intercept
  * @param seSlope
  *   Standard error of slope
  * @param zIntercept
  *   z-statistic for intercept (β₀ / SE(β₀))
  * @param zSlope
  *   z-statistic for slope (β₁ / SE(β₁))
  * @param pValueIntercept
  *   Two-tailed p-value for intercept (H₀: β₀ = 0)
  * @param pValueSlope
  *   Two-tailed p-value for slope (H₀: β₁ = 0) - this tests for significant trend
  * @param nullDeviance
  *   Deviance of intercept-only model
  * @param residualDeviance
  *   Deviance of full model
  * @param dispersion
  *   Estimated dispersion parameter (1.0 for Poisson, estimated for NegBin)
  * @param fStatistic
  *   Test statistic for model vs intercept-only: the deviance drop, divided by the estimated dispersion for NegBin
  * @param fPValue
  *   p-value for `fStatistic`: χ²(1) for Poisson (dispersion fixed at 1), F(1, n−2) for NegBin (dispersion estimated)
  * @param aic
  *   Akaike Information Criterion
  * @param logLikelihood
  *   Log-likelihood of the fitted model
  */
case class TrendFitResult(
    nObs: Int,
    dfResidual: Int,
    intercept: Double,
    slope: Double,
    seIntercept: Double,
    seSlope: Double,
    zIntercept: Double,
    zSlope: Double,
    pValueIntercept: Double,
    pValueSlope: Double,
    nullDeviance: Double,
    residualDeviance: Double,
    dispersion: Double,
    fStatistic: Double,
    fPValue: Double,
    aic: Double,
    logLikelihood: Double
):

  /** Test whether there is a statistically significant trend at the given alpha level */
  def hasSignificantTrend(alpha: Double = 0.05): Boolean = pValueSlope < alpha

  /** Nicely formatted summary string, similar to R's glm summary output */
  def summary: String =
    val sb = new StringBuilder
    sb.append("Generalized Linear Model: log(Count) ~ 1 + Year\n")
    sb.append("=" * 60 + "\n\n")

    sb.append("Coefficients:\n")
    sb.append(f"${""}%-15s ${"Estimate"}%12s ${"Std. Error"}%12s ${"z value"}%10s ${"Pr(>|z|)"}%12s\n")
    sb.append("-" * 60 + "\n")
    sb.append(
      f"(Intercept)${" "}%-4s $intercept%12.5f $seIntercept%12.5f $zIntercept%10.3f $pValueIntercept%12.6f${significanceCode(pValueIntercept)}%s\n"
    )
    sb.append(
      f"Year${" "}%-11s $slope%12.7f $seSlope%12.7f $zSlope%10.3f $pValueSlope%12.6f${significanceCode(pValueSlope)}%s\n"
    )
    sb.append("-" * 60 + "\n")
    sb.append("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")

    sb.append(f"$nObs observations, $dfResidual residual degrees of freedom\n")
    sb.append(f"Estimated Dispersion: $dispersion%.3f\n")
    sb.append(f"Null Deviance:     $nullDeviance%.3f\n")
    sb.append(f"Residual Deviance: $residualDeviance%.3f\n")
    sb.append(f"AIC: $aic%.3f\n")
    sb.append(f"Log-Likelihood: $logLikelihood%.3f\n\n")

    sb.append(f"Deviance test vs. constant model: $fStatistic%.3f, p-value = $fPValue%.6f\n")

    sb.toString
  end summary

  private def significanceCode(p: Double): String =
    if p < 0.001 then " ***"
    else if p < 0.01 then " **"
    else if p < 0.05 then " *"
    else if p < 0.1 then " ."
    else ""
end TrendFitResult

/** Result of fitting a severity trend: log(X) = β₀ + β₁·year + ε, by ordinary least squares.
  *
  * `exp(β₁) − 1` is the annual trend in the median (geometric mean) severity. If log(X) is normal with constant σ (a
  * lognormal severity), it is also the annual trend in the mean.
  *
  * Inference uses Student's t with `n − 2` degrees of freedom, assuming independent residuals with constant variance.
  * Residuals of log severity are often skewed (log of a Pareto is exponential). With small data volumes that makes the
  * t-based intervals approximate, and single losses can dominate the fit: see
  * [[TrendAnalysis.fitSeverityTrendLeaveOneOut]]. Read the result as a check for gross errors in the normalisation, not
  * as a measurement of the trend.
  *
  * @param nObs
  *   Number of losses
  * @param dfResidual
  *   Residual degrees of freedom (n − 2)
  * @param intercept
  *   Estimated intercept (β₀), log scale
  * @param slope
  *   Estimated year coefficient (β₁), log scale
  * @param seIntercept
  *   Standard error of intercept
  * @param seSlope
  *   Standard error of slope
  * @param tIntercept
  *   t-statistic for intercept
  * @param tSlope
  *   t-statistic for slope
  * @param pValueIntercept
  *   Two-tailed p-value for intercept (H₀: β₀ = 0)
  * @param pValueSlope
  *   Two-tailed p-value for slope (H₀: β₁ = 0) - this tests for a significant severity trend
  * @param sigma
  *   Residual standard deviation of log severity
  * @param rSquared
  *   Coefficient of determination
  * @param meanYear
  *   Mean of the years, x̄
  * @param sxx
  *   Σ(yearᵢ − x̄)²
  */
case class SeverityTrendResult(
    nObs: Int,
    dfResidual: Int,
    intercept: Double,
    slope: Double,
    seIntercept: Double,
    seSlope: Double,
    tIntercept: Double,
    tSlope: Double,
    pValueIntercept: Double,
    pValueSlope: Double,
    sigma: Double,
    rSquared: Double,
    meanYear: Double,
    sxx: Double
):

  private lazy val tDist = TDistribution.of(dfResidual.toDouble)

  /** Two-sided critical value t₁₋α/₂ with `n − 2` degrees of freedom. */
  def tCritical(alpha: Double = 0.05): Double =
    require(alpha > 0 && alpha < 1, "alpha must be in (0, 1)")
    tDist.inverseSurvivalProbability(alpha / 2)
  end tCritical

  /** Test whether there is a statistically significant severity trend at the given alpha level */
  def hasSignificantTrend(alpha: Double = 0.05): Boolean = pValueSlope < alpha

  /** Annual trend in median severity, `exp(β₁) − 1`. 0.03 means +3% per year. */
  def annualTrend: Double = math.expm1(slope)

  /** `(1 − alpha)` confidence interval for [[annualTrend]]. */
  def annualTrendCI(alpha: Double = 0.05): (lower: Double, upper: Double) =
    val t = tCritical(alpha)
    (lower = math.expm1(slope - t * seSlope), upper = math.expm1(slope + t * seSlope))
  end annualTrendCI

  /** Fitted log severity at `year`, with a `(1 − alpha)` confidence interval for the regression line. */
  def fittedLog(year: Double, alpha: Double = 0.05): (fit: Double, lower: Double, upper: Double) =
    val fit = intercept + slope * year
    val dx = year - meanYear
    val se = sigma * math.sqrt(1.0 / nObs + dx * dx / sxx)
    val t = tCritical(alpha)
    (fit = fit, lower = fit - t * se, upper = fit + t * se)
  end fittedLog

  /** Nicely formatted summary string, similar to R's lm summary output */
  def summary: String =
    val sb = new StringBuilder
    sb.append("Linear Model: log(Severity) ~ 1 + Year\n")
    sb.append("=" * 60 + "\n\n")

    sb.append("Coefficients:\n")
    sb.append(f"${""}%-15s ${"Estimate"}%12s ${"Std. Error"}%12s ${"t value"}%10s ${"Pr(>|t|)"}%12s\n")
    sb.append("-" * 60 + "\n")
    sb.append(
      f"(Intercept)${" "}%-4s $intercept%12.5f $seIntercept%12.5f $tIntercept%10.3f $pValueIntercept%12.6f${significanceCode(pValueIntercept)}%s\n"
    )
    sb.append(
      f"Year${" "}%-11s $slope%12.7f $seSlope%12.7f $tSlope%10.3f $pValueSlope%12.6f${significanceCode(pValueSlope)}%s\n"
    )
    sb.append("-" * 60 + "\n")
    sb.append("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")

    sb.append(f"Residual standard error: $sigma%.4f on $dfResidual degrees of freedom\n")
    sb.append(f"R-squared: $rSquared%.4f\n")
    val ci = annualTrendCI()
    sb.append(
      f"Annual trend in median severity: ${annualTrend * 100}%.2f%% (95%% CI ${ci.lower * 100}%.2f%% to ${ci.upper * 100}%.2f%%)\n"
    )

    sb.toString
  end summary

  private def significanceCode(p: Double): String =
    if p < 0.001 then " ***"
    else if p < 0.01 then " **"
    else if p < 0.05 then " *"
    else if p < 0.1 then " ."
    else ""
end SeverityTrendResult

object TrendAnalysis:
  private val normDist = NormalDistribution.of(0.0, 1.0)

  /** Two-tailed p-value from z-statistic using normal approximation */
  private inline def pValueFromZ(z: Double): Double =
    if z.isNaN || z.isInfinite then Double.NaN
    else 2.0 * (1.0 - normalCdf(math.abs(z)))

  /** Standard normal CDF using Apache Commons Math */
  private inline def normalCdf(x: Double): Double =
    normDist.cumulativeProbability(x)

  /** F-distribution p-value: P(F > f) for right-tailed test */
  private inline def fDistPValue(f: Double, df1: Int, df2: Int): Double =
    if f <= 0 || df1 <= 0 || df2 <= 0 then 1.0
    else
      val fDist = FDistribution.of(df1.toDouble, df2.toDouble)
      1.0 - fDist.cumulativeProbability(f)

  private val chiSq1 = ChiSquaredDistribution.of(1.0)

  /** Chi-squared(1) p-value: P(X > x) */
  private inline def chiSq1PValue(x: Double): Double =
    if x <= 0 then 1.0 else chiSq1.survivalProbability(x)

  /** Regularized incomplete beta function using Apache Commons Math */
  private inline def incompleteBeta(a: Double, b: Double, x: Double): Double =
    RegularizedBeta.value(x, a, b)

  extension (p: Poisson)
    /** Fit a Poisson GLM trend model: log(μ) = β₀ + β₁·year
      *
      * Uses IRLS to fit the model and computes test statistics for assessing whether there is a statistically
      * significant trend over time.
      *
      * @param years
      *   the year for each observation
      * @param counts
      *   the count for each observation (same length as years)
      * @return
      *   TrendFitResult containing coefficients, standard errors, p-values, and goodness-of-fit statistics
      */
    def fitTrend(years: IndexedSeq[Int], counts: IndexedSeq[Int]): TrendFitResult =
      require(years.length == counts.length, "years and counts must have the same length")
      require(years.length >= 3, "need at least 3 observations to fit a trend")

      val n = years.length
      val yearsD = years.map(_.toDouble)
      val countsD = counts.map(_.toDouble)

      // Fit full model: log(μ) = β₀ + β₁·year via IRLS
      val meanY = countsD.sum / n
      var beta0 = math.log(math.max(meanY, 0.1))
      var beta1 = 0.0

      for _ <- 0 until 25 do
        val mu = yearsD.map(y => math.exp(beta0 + beta1 * y))
        val z = (0 until n).map { i =>
          val eta = beta0 + beta1 * yearsD(i)
          eta + (countsD(i) - mu(i)) / math.max(mu(i), 1e-10)
        }
        val w = mu.map(m => math.max(m, 1e-10))

        var xtwx00, xtwx01, xtwx11 = 0.0
        var xtwz0, xtwz1 = 0.0
        var i = 0
        while i < n do
          val wi = w(i)
          val yi = yearsD(i)
          val zi = z(i)
          xtwx00 += wi
          xtwx01 += wi * yi
          xtwx11 += wi * yi * yi
          xtwz0 += wi * zi
          xtwz1 += wi * yi * zi
          i += 1
        end while

        val det = xtwx00 * xtwx11 - xtwx01 * xtwx01
        if math.abs(det) > 1e-15 then
          beta0 = (xtwx11 * xtwz0 - xtwx01 * xtwz1) / det
          beta1 = (xtwx00 * xtwz1 - xtwx01 * xtwz0) / det
        end if
      end for

      // Fit null model: log(μ) = β₀ only
      val nullBeta0 = math.log(meanY)
      val muNull = Array.fill(n)(meanY)

      // Compute deviances
      // Poisson deviance: 2 * Σ[yᵢ·log(yᵢ/μᵢ) - (yᵢ - μᵢ)]
      def poissonDeviance(observed: IndexedSeq[Double], fitted: IndexedSeq[Double]): Double =
        var dev = 0.0
        var i = 0
        while i < n do
          val y = observed(i)
          val mu = fitted(i)
          if y > 0 then dev += y * math.log(y / mu)
          end if
          dev -= (y - mu)
          i += 1
        end while
        2.0 * dev
      end poissonDeviance

      val muFull = yearsD.map(y => math.exp(beta0 + beta1 * y))
      val nullDeviance = poissonDeviance(countsD, muNull.toIndexedSeq)
      val residualDeviance = poissonDeviance(countsD, muFull)

      // Fisher information and standard errors
      var i00, i01, i11 = 0.0
      var j = 0
      while j < n do
        val mi = muFull(j)
        val yi = yearsD(j)
        i00 += mi
        i01 += mi * yi
        i11 += mi * yi * yi
        j += 1
      end while

      val detI = i00 * i11 - i01 * i01
      val seBeta0 = if detI > 1e-15 then math.sqrt(i11 / detI) else Double.NaN
      val seBeta1 = if detI > 1e-15 then math.sqrt(i00 / detI) else Double.NaN

      // z-statistics and p-values
      val zBeta0 = beta0 / seBeta0
      val zBeta1 = beta1 / seBeta1
      val pBeta0 = pValueFromZ(zBeta0)
      val pBeta1 = pValueFromZ(zBeta1)

      // For Poisson, dispersion = 1 by assumption
      val dispersion = 1.0

      // Pearson dispersion estimate (for diagnostics)
      var pearsonChi2 = 0.0
      var k = 0
      while k < n do
        val y = countsD(k)
        val mu = muFull(k)
        pearsonChi2 += (y - mu) * (y - mu) / math.max(mu, 1e-10)
        k += 1
      end while
      val estimatedDispersion = pearsonChi2 / (n - 2)

      // Likelihood-ratio test: with dispersion fixed at 1 the deviance drop is asymptotically χ²(1). An F reference
      // distribution is only appropriate when the dispersion is estimated (see the NegBin fit).
      val fStat = (nullDeviance - residualDeviance) / dispersion
      val fPVal = chiSq1PValue(fStat)

      // Log-likelihood
      def poissonLogLik(observed: IndexedSeq[Double], fitted: IndexedSeq[Double]): Double =
        var ll = 0.0
        var i = 0
        while i < n do
          val y = observed(i).toInt
          val mu = fitted(i)
          ll += y * math.log(mu) - mu - LogGamma.value(y + 1)
          i += 1
        end while
        ll
      end poissonLogLik

      val logLik = poissonLogLik(countsD, muFull)
      val aic = -2 * logLik + 2 * 2 // 2 parameters

      TrendFitResult(
        nObs = n,
        dfResidual = n - 2,
        intercept = beta0,
        slope = beta1,
        seIntercept = seBeta0,
        seSlope = seBeta1,
        zIntercept = zBeta0,
        zSlope = zBeta1,
        pValueIntercept = pBeta0,
        pValueSlope = pBeta1,
        nullDeviance = nullDeviance,
        residualDeviance = residualDeviance,
        dispersion = estimatedDispersion,
        fStatistic = fStat,
        fPValue = fPVal,
        aic = aic,
        logLikelihood = logLik
      )
  end extension

  extension (nb: NegativeBinomial)
    /** Fit a Negative Binomial GLM trend model: log(μ) = β₀ + β₁·year
      *
      * Uses IRLS with the NB2 variance function (Var = μ + μ²/θ where θ = a). This accounts for overdispersion in count
      * data.
      *
      * @param years
      *   the year for each observation
      * @param counts
      *   the count for each observation (same length as years)
      * @return
      *   TrendFitResult containing coefficients, standard errors, p-values, and goodness-of-fit statistics
      */
    def fitTrend(years: IndexedSeq[Int], counts: IndexedSeq[Int]): TrendFitResult =
      require(years.length == counts.length, "years and counts must have the same length")
      require(years.length >= 3, "need at least 3 observations to fit a trend")

      val n = years.length
      val yearsD = years.map(_.toDouble)
      val countsD = counts.map(_.toDouble)
      val theta = nb.a // overdispersion parameter

      // Fit full model via IRLS with NB variance function
      val meanY = countsD.sum / n
      var beta0 = math.log(math.max(meanY, 0.1))
      var beta1 = 0.0

      for _ <- 0 until 25 do
        val mu = yearsD.map(y => math.exp(beta0 + beta1 * y))

        // NB2 variance: Var = μ + μ²/θ, so weight = μ / (1 + μ/θ)
        val w = mu.map { m =>
          val v = m + m * m / theta
          math.max(m * m / v, 1e-10)
        }

        val z = (0 until n).map { i =>
          val eta = beta0 + beta1 * yearsD(i)
          eta + (countsD(i) - mu(i)) / math.max(mu(i), 1e-10)
        }

        var xtwx00, xtwx01, xtwx11 = 0.0
        var xtwz0, xtwz1 = 0.0
        var i = 0
        while i < n do
          val wi = w(i)
          val yi = yearsD(i)
          val zi = z(i)
          xtwx00 += wi
          xtwx01 += wi * yi
          xtwx11 += wi * yi * yi
          xtwz0 += wi * zi
          xtwz1 += wi * yi * zi
          i += 1
        end while

        val det = xtwx00 * xtwx11 - xtwx01 * xtwx01
        if math.abs(det) > 1e-15 then
          beta0 = (xtwx11 * xtwz0 - xtwx01 * xtwz1) / det
          beta1 = (xtwx00 * xtwz1 - xtwx01 * xtwz0) / det
        end if
      end for

      // Null model
      val nullBeta0 = math.log(meanY)

      // Negative binomial deviance: 2 * Σ[yᵢ·log(yᵢ/μᵢ) - (yᵢ + θ)·log((yᵢ + θ)/(μᵢ + θ))]
      def nbDeviance(observed: IndexedSeq[Double], fitted: IndexedSeq[Double]): Double =
        var dev = 0.0
        var i = 0
        while i < n do
          val y = observed(i)
          val mu = fitted(i)
          if y > 0 then dev += y * math.log(y / mu)
          end if
          dev -= (y + theta) * math.log((y + theta) / (mu + theta))
          i += 1
        end while
        2.0 * dev
      end nbDeviance

      val muFull = yearsD.map(y => math.exp(beta0 + beta1 * y))
      val muNull = IndexedSeq.fill(n)(meanY)
      val nullDeviance = nbDeviance(countsD, muNull)
      val residualDeviance = nbDeviance(countsD, muFull)

      // Fisher information with NB variance
      var i00, i01, i11 = 0.0
      var j = 0
      while j < n do
        val mi = muFull(j)
        val yi = yearsD(j)
        val wi = mi * mi / (mi + mi * mi / theta)
        i00 += wi
        i01 += wi * yi
        i11 += wi * yi * yi
        j += 1
      end while

      val detI = i00 * i11 - i01 * i01
      val seBeta0 = if detI > 1e-15 then math.sqrt(i11 / detI) else Double.NaN
      val seBeta1 = if detI > 1e-15 then math.sqrt(i00 / detI) else Double.NaN

      val zBeta0 = beta0 / seBeta0
      val zBeta1 = beta1 / seBeta1
      val pBeta0 = pValueFromZ(zBeta0)
      val pBeta1 = pValueFromZ(zBeta1)

      // Estimated dispersion (Pearson)
      var pearsonChi2 = 0.0
      var k = 0
      while k < n do
        val y = countsD(k)
        val mu = muFull(k)
        val v = mu + mu * mu / theta
        pearsonChi2 += (y - mu) * (y - mu) / v
        k += 1
      end while
      val dispersion = pearsonChi2 / (n - 2)

      // F-statistic
      val fStat = (nullDeviance - residualDeviance) / dispersion
      val fPVal = fDistPValue(fStat, 1, n - 2)

      // NB log-likelihood
      def nbLogLik(observed: IndexedSeq[Double], fitted: IndexedSeq[Double]): Double =
        var ll = 0.0
        var i = 0
        while i < n do
          val y = observed(i).toInt
          val mu = fitted(i)
          // log P(Y=y) = log Γ(y+θ) - log Γ(θ) - log(y!) + θ·log(θ/(θ+μ)) + y·log(μ/(θ+μ))
          ll += LogGamma.value(y + theta) - LogGamma.value(theta) - LogGamma.value(y + 1)
          ll += theta * math.log(theta / (theta + mu))
          ll += y * math.log(mu / (theta + mu))
          i += 1
        end while
        ll
      end nbLogLik

      val logLik = nbLogLik(countsD, muFull)
      val aic = -2 * logLik + 2 * 2 // 2 parameters (not counting θ as estimated here)

      TrendFitResult(
        nObs = n,
        dfResidual = n - 2,
        intercept = beta0,
        slope = beta1,
        seIntercept = seBeta0,
        seSlope = seBeta1,
        zIntercept = zBeta0,
        zSlope = zBeta1,
        pValueIntercept = pBeta0,
        pValueSlope = pBeta1,
        nullDeviance = nullDeviance,
        residualDeviance = residualDeviance,
        dispersion = dispersion,
        fStatistic = fStat,
        fPValue = fPVal,
        aic = aic,
        logLikelihood = logLik
      )
  end extension

  /** Fit a severity trend: log(X) = β₀ + β₁·year + ε, by ordinary least squares.
    *
    * Use on-levelled losses above a threshold that is fixed in on-levelled terms. A significant slope means the
    * conditional severity above the threshold is not stationary: on-levelling has left (or introduced) a trend.
    *
    * Note that with a fixed threshold, a pure scale trend in a Pareto tail is invisible here: above the threshold, the
    * Pareto is scale free, so the trend appears only as a trend in the counts above the threshold. Check the frequency
    * trend as well.
    *
    * @param years
    *   the year of each loss
    * @param losses
    *   the (on-levelled) loss amounts, all strictly positive, same length as years
    * @return
    *   SeverityTrendResult containing coefficients, standard errors, p-values and the implied annual trend
    */
  def fitSeverityTrend(years: IndexedSeq[Int], losses: IndexedSeq[Double]): SeverityTrendResult =
    require(years.length == losses.length, "years and losses must have the same length")
    require(years.length >= 3, "need at least 3 observations to fit a trend")

    val n = years.length
    var sumX = 0.0
    var sumY = 0.0
    var i = 0
    while i < n do
      val l = losses(i)
      require(l > 0 && !l.isInfinite, s"losses must be finite and strictly positive, got $l at index $i")
      sumX += years(i)
      sumY += math.log(l)
      i += 1
    end while
    val xBar = sumX / n
    val yBar = sumY / n

    // Centred sums, for numerical stability with years ~ 2000
    var sxx = 0.0
    var sxy = 0.0
    var syy = 0.0
    i = 0
    while i < n do
      val dx = years(i) - xBar
      val dy = math.log(losses(i)) - yBar
      sxx += dx * dx
      sxy += dx * dy
      syy += dy * dy
      i += 1
    end while
    require(sxx > 0, "need at least two distinct years to fit a trend")

    val beta1 = sxy / sxx
    val beta0 = yBar - beta1 * xBar
    val rss = math.max(syy - beta1 * sxy, 0.0)
    val df = n - 2
    val sigma = math.sqrt(rss / df)
    val seBeta1 = sigma / math.sqrt(sxx)
    val seBeta0 = sigma * math.sqrt(1.0 / n + xBar * xBar / sxx)

    val tDist = TDistribution.of(df.toDouble)
    def pValueT(t: Double): Double =
      if t.isNaN then Double.NaN
      else if t.isInfinite then 0.0
      else 2.0 * tDist.survivalProbability(math.abs(t))

    val tBeta0 = beta0 / seBeta0
    val tBeta1 = beta1 / seBeta1

    SeverityTrendResult(
      nObs = n,
      dfResidual = df,
      intercept = beta0,
      slope = beta1,
      seIntercept = seBeta0,
      seSlope = seBeta1,
      tIntercept = tBeta0,
      tSlope = tBeta1,
      pValueIntercept = pValueT(tBeta0),
      pValueSlope = pValueT(tBeta1),
      sigma = sigma,
      rSquared = if syy > 0 then 1.0 - rss / syy else Double.NaN,
      meanYear = xBar,
      sxx = sxx
    )
  end fitSeverityTrend

  /** Refit [[fitSeverityTrend]] with each loss left out in turn.
    *
    * With small data volumes a single loss can move the trend by more than any plausible true trend. The spread of the
    * leave-one-out trends shows how much the result depends on individual losses.
    *
    * @return
    *   one entry per loss, aligned with the inputs: the fit without that loss, or `None` where the remaining losses
    *   cannot be fitted (fewer than 3, or all in one year)
    */
  def fitSeverityTrendLeaveOneOut(
      years: IndexedSeq[Int],
      losses: IndexedSeq[Double]
  ): IndexedSeq[Option[SeverityTrendResult]] =
    require(years.length == losses.length, "years and losses must have the same length")
    years.indices.map { drop =>
      val ys = years.patch(drop, Nil, 1)
      val ls = losses.patch(drop, Nil, 1)
      Option.when(ys.length >= 3 && ys.distinct.length >= 2)(fitSeverityTrend(ys, ls))
    }
  end fitSeverityTrendLeaveOneOut

end TrendAnalysis
