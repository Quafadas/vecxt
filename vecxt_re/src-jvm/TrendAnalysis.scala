package vecxt_re

import org.apache.commons.math3.special.Gamma.logGamma

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
  *   F-statistic for model vs intercept-only (using dispersion)
  * @param fPValue
  *   p-value for F-statistic
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

    sb.append(f"F-statistic vs. constant model: $fStatistic%.3f, p-value = $fPValue%.6f\n")

    sb.toString
  end summary

  private def significanceCode(p: Double): String =
    if p < 0.001 then " ***"
    else if p < 0.01 then " **"
    else if p < 0.05 then " *"
    else if p < 0.1 then " ."
    else ""
end TrendFitResult

object TrendAnalysis:
  private val normDist = org.apache.commons.math3.distribution.NormalDistribution(0.0, 1.0)

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
      val fDist = new org.apache.commons.math3.distribution.FDistribution(df1.toDouble, df2.toDouble)
      1.0 - fDist.cumulativeProbability(f)

  /** Regularized incomplete beta function using Apache Commons Math */
  private inline def incompleteBeta(a: Double, b: Double, x: Double): Double =
    org.apache.commons.math3.special.Beta.regularizedBeta(x, a, b)

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

      // F-statistic: (null deviance - residual deviance) / dispersion
      val fStat = (nullDeviance - residualDeviance) / dispersion
      val fPVal = fDistPValue(fStat, 1, n - 2)

      // Log-likelihood
      def poissonLogLik(observed: IndexedSeq[Double], fitted: IndexedSeq[Double]): Double =
        var ll = 0.0
        var i = 0
        while i < n do
          val y = observed(i).toInt
          val mu = fitted(i)
          ll += y * math.log(mu) - mu - logGamma(y + 1)
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
          ll += logGamma(y + theta) - logGamma(theta) - logGamma(y + 1)
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

end TrendAnalysis
