package vecxt_re

import vecxt_re.HillEstimator.HillPlotResult
import vecxt_re.PickandsEstimator.PickandsPlotResult

import io.circe.syntax.*
import io.github.quafadas.plots.SetupVega.{*, given}

object Plots:
  // These must be private otherwise scaladoc get crazy.
  private lazy val timeline = VegaPlot.fromResource("timeline.vl.json") // riskInceptionDate, riskExpiryDate
  private lazy val seasonality = VegaPlot.fromResource("seasonality.vg.json") // catagory, amount
  private lazy val distributionDensity = VegaPlot.fromResource("distDensity.vg.json") // value, density
  private lazy val negBinCdfWSample = VegaPlot.fromResource("negBinCumul_vsSample.vl.json") // value, density
  private lazy val ecdfVsCdf = VegaPlot.fromResource("ecdfVsCdf.vl.json") // theoretical and empirical CDF
  private lazy val rootogram = VegaPlot.fromResource("rootogram.vl.json") // hanging rootogram
  private lazy val pearsonResiduals = VegaPlot.fromResource("pearsonResiduals.vl.json") // residual plot
  private lazy val poissonTrend = VegaPlot.fromResource("poissonTrend.vl.json") // Poisson GLM trend
  private lazy val logLogPlot = VegaPlot.fromResource("loglogCdf.vl.json") // log-log CDF plot
  private lazy val hillPlotSpec = VegaPlot.fromResource("hillPlot.vl.json") // Hill plot for tail index

  extension (idx: CalendarYearIndex)
    def plotIndex(reportingThreshold: Double)(using viz.LowPriorityPlotTarget) =
      val linePlot2 = VegaPlot.fromResource("index.vl.json")
      val cumulative = idx.onLevel(Array.fill(idx.years.length)(1.0), idx.years)
      val factors = idx.years.zip(idx.indices).zip(cumulative).map { case ((year, index), cumulative) =>
        (
          year = year,
          index = index,
          missing = 1 / cumulative,
          threshold = idx.suggestedNewThreshold(reportingThreshold)
        )
      }
      linePlot2.plot(
        _.data.values := factors.asJson
      )
  end extension

  extension (nb: NegativeBinomial)

    /** Plot ECDF vs theoretical CDF as step functions for visual goodness-of-fit assessment.
      *
      * Both curves are step functions. Deviations between the orange (empirical) and blue (theoretical) lines indicate
      * potential model misfit.
      */
    inline def plotEcdfVsCdf(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget) =
      val maxX = math.max(samples.max, (nb.mean + 4 * math.sqrt(nb.variance)).toInt)

      // Theoretical CDF
      var cumProb = 0.0
      val theoreticalCdf = (0 to maxX).map { k =>
        cumProb += nb.probabilityOf(k)
        (value = k, prob = cumProb)
      }

      // Empirical CDF (step function at each unique value)
      val n = samples.length.toDouble
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)
      var empiricalCum = 0.0
      val empiricalCdf = (0 to maxX).map { k =>
        empiricalCum += counts.getOrElse(k, 0)
        (value = k, prob = empiricalCum / n)
      }

      ecdfVsCdf.plot(
        _.title(s"NegBin(a=${nb.a}, b=${nb.b}) ECDF vs Theoretical CDF"),
        _.layer._0.data.values := theoreticalCdf.asJson,
        _.layer._1.data.values := empiricalCdf.asJson
      )
    end plotEcdfVsCdf

    /** Plot a hanging rootogram for count data diagnostics.
      *
      * A rootogram displays sqrt(expected) as the reference curve and hangs bars from it down to sqrt(observed). When
      * the model fits well, bars hang close to the zero line. Bars extending below zero indicate under-prediction; bars
      * stopping above zero indicate over-prediction.
      */
    inline def plotRootogram(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget) =
      val n = samples.length.toDouble
      val maxK = math.max(samples.max, (nb.mean + 3 * math.sqrt(nb.variance)).toInt)
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)

      val data = (0 to maxK).map { k =>
        val observed = counts.getOrElse(k, 0)
        val expected = nb.probabilityOf(k) * n
        val sqrtObs = math.sqrt(observed)
        val sqrtExp = math.sqrt(expected)
        // Hanging: bar goes from sqrtExp down by sqrtObs, ending at sqrtExp - sqrtObs
        (k = k, sqrtExpected = sqrtExp, sqrtObserved = sqrtObs, hanging = sqrtExp - sqrtObs)
      }

      rootogram.plot(
        _.title(s"NegBin(a=${nb.a}, b=${nb.b}) Hanging Rootogram"),
        _.data.values := data.asJson
      )
    end plotRootogram

    /** Plot Pearson residuals: (observed - expected) / sqrt(expected).
      *
      * Residuals beyond ±2 (shown in red) indicate significant deviation from the fitted model. For Negative Binomial,
      * we use the variance = μ(1 + μ/a) for the denominator when available.
      */
    inline def plotPearsonResiduals(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget) =
      val n = samples.length.toDouble
      val maxK = math.max(samples.max, (nb.mean + 3 * math.sqrt(nb.variance)).toInt)
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)

      val data = (0 to maxK).flatMap { k =>
        val observed = counts.getOrElse(k, 0)
        val expected = nb.probabilityOf(k) * n
        // Only include if expected > 0 to avoid division by zero
        if expected > 0.001 then
          // For NegBin, variance of count = expected * (1 + expected/(n*a)) approximately
          // Simplify to Pearson: (O - E) / sqrt(E)
          val residual = (observed - expected) / math.sqrt(expected)
          Some((k = k, residual = residual))
        else None
        end if
      }

      pearsonResiduals.plot(
        _.title(s"NegBin(a=${nb.a}, b=${nb.b}) Pearson Residuals"),
        _.data.values := data.asJson
      )
    end plotPearsonResiduals
  end extension

  extension (p: Poisson)
    /** Plot ECDF vs theoretical CDF as step functions for visual goodness-of-fit assessment. */
    inline def plotEcdfVsCdf(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget) =
      val maxX = math.max(samples.max, (p.mean + 4 * math.sqrt(p.variance)).toInt)

      // Theoretical CDF
      var cumProb = 0.0
      val theoreticalCdf = (0 to maxX).map { k =>
        cumProb += p.probabilityOf(k)
        (value = k, prob = cumProb)
      }

      // Empirical CDF
      val n = samples.length.toDouble
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)
      var empiricalCum = 0.0
      val empiricalCdf = (0 to maxX).map { k =>
        empiricalCum += counts.getOrElse(k, 0)
        (value = k, prob = empiricalCum / n)
      }

      ecdfVsCdf.plot(
        _.title(s"Poisson(λ=${p.lambda}) ECDF vs Theoretical CDF"),
        _.layer._0.data.values := theoreticalCdf.asJson,
        _.layer._1.data.values := empiricalCdf.asJson
      )
    end plotEcdfVsCdf

    /** Plot a hanging rootogram for Poisson count data diagnostics.
      *
      * Bars hang from sqrt(expected) down to sqrt(expected) - sqrt(observed). Good fit means bars end near zero.
      */
    inline def plotRootogram(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget) =
      val n = samples.length.toDouble
      val maxK = math.max(samples.max, (p.mean + 3 * math.sqrt(p.variance)).toInt)
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)

      val data = (0 to maxK).map { k =>
        val observed = counts.getOrElse(k, 0)
        val expected = p.probabilityOf(k) * n
        val sqrtObs = math.sqrt(observed)
        val sqrtExp = math.sqrt(expected)
        (k = k, sqrtExpected = sqrtExp, sqrtObserved = sqrtObs, hanging = sqrtExp - sqrtObs)
      }

      rootogram.plot(
        _.title(s"Poisson(λ=${p.lambda}) Hanging Rootogram"),
        _.data.values := data.asJson
      )
    end plotRootogram

    /** Plot Pearson residuals for Poisson: (observed - expected) / sqrt(expected).
      *
      * For Poisson, variance = mean, so the denominator is simply sqrt(expected). Residuals beyond ±2 (red) suggest
      * significant deviation. Systematic patterns may indicate overdispersion (consider Negative Binomial).
      */
    inline def plotPearsonResiduals(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget) =
      val n = samples.length.toDouble
      val maxK = math.max(samples.max, (p.mean + 3 * math.sqrt(p.variance)).toInt)
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)

      val data = (0 to maxK).flatMap { k =>
        val observed = counts.getOrElse(k, 0)
        val expected = p.probabilityOf(k) * n
        if expected > 0.001 then
          val residual = (observed - expected) / math.sqrt(expected)
          Some((k = k, residual = residual))
        else None
        end if
      }

      pearsonResiduals.plot(
        _.title(s"Poisson(λ=${p.lambda}) Pearson Residuals"),
        _.data.values := data.asJson
      )
    end plotPearsonResiduals

    /** Plot a Poisson GLM trend: log(Count) ~ 1 + Year with 95% confidence intervals.
      *
      * Fits a Poisson regression to count data over years and displays: - Observations (blue X markers) - Fitted trend
      * line (solid red) - 95% confidence interval band (dashed red lines with shaded area)
      *
      * The coefficients (intercept, year slope) and their standard errors are estimated via iteratively reweighted
      * least squares (IRLS). The confidence intervals use normal approximation on the log scale.
      *
      * @param years
      *   the year for each observation
      * @param counts
      *   the count for each observation (same length as years)
      */
    inline def plotTrend(years: IndexedSeq[Int], counts: IndexedSeq[Int])(using viz.LowPriorityPlotTarget) =
      require(years.length == counts.length, "years and counts must have the same length")

      val n = years.length
      val yearsD = years.map(_.toDouble)
      val countsD = counts.map(_.toDouble)

      // Fit Poisson GLM via IRLS: log(μ) = β₀ + β₁·year
      // Design matrix: X = [1 | year], each row is [1, yearᵢ]
      val meanY = countsD.sum / n
      var beta0 = math.log(math.max(meanY, 0.1))
      var beta1 = 0.0

      // IRLS iterations
      for _ <- 0 until 25 do
        // Fitted values: μ = exp(Xβ)
        val mu = yearsD.map(y => math.exp(beta0 + beta1 * y))

        // Working response: z = η + (y - μ)/μ where η = Xβ
        val z = (0 until n).map { i =>
          val eta = beta0 + beta1 * yearsD(i)
          eta + (countsD(i) - mu(i)) / math.max(mu(i), 1e-10)
        }

        // Weights: W = diag(μ) for Poisson canonical link
        val w = mu.map(m => math.max(m, 1e-10))

        // Solve weighted least squares: (XᵀWX)β = XᵀWz
        // XᵀWX is 2×2 symmetric: [[Σwᵢ, Σwᵢyᵢ], [Σwᵢyᵢ, Σwᵢyᵢ²]]
        // XᵀWz is 2×1: [Σwᵢzᵢ, Σwᵢyᵢzᵢ]
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

        // Solve 2×2 system via Cramer's rule: [xtwx00, xtwx01; xtwx01, xtwx11] * β = [xtwz0; xtwz1]
        val det = xtwx00 * xtwx11 - xtwx01 * xtwx01
        if math.abs(det) > 1e-15 then
          beta0 = (xtwx11 * xtwz0 - xtwx01 * xtwz1) / det
          beta1 = (xtwx00 * xtwz1 - xtwx01 * xtwz0) / det
        end if
      end for

      // Fisher information matrix: I = XᵀWX at final β
      // I is 2×2 symmetric: [[i00, i01], [i01, i11]]
      val muFinal = yearsD.map(y => math.exp(beta0 + beta1 * y))
      var i00, i01, i11 = 0.0
      var j = 0
      while j < n do
        val mi = muFinal(j)
        val yi = yearsD(j)
        i00 += mi
        i01 += mi * yi
        i11 += mi * yi * yi
        j += 1
      end while

      // Standard errors from Cov(β) = I⁻¹
      val detI = i00 * i11 - i01 * i01
      val seBeta0 = if detI > 1e-15 then math.sqrt(i11 / detI) else Double.NaN
      val seBeta1 = if detI > 1e-15 then math.sqrt(i00 / detI) else Double.NaN

      // Covariance matrix: Cov(β) = I⁻¹ = (1/det) * [[i11, -i01], [-i01, i00]]
      val covBeta =
        if detI > 1e-15 then
          Some(
            (
              v00 = i11 / detI,
              v01 = -i01 / detI,
              v11 = i00 / detI
            )
          )
        else None

      // Generate fitted curve with CI
      val minYear = years.min
      val maxYear = years.max
      val yearRange = (minYear to maxYear).toVector

      val ciData = yearRange.map { y =>
        val eta = beta0 + beta1 * y.toDouble
        // Var(η) = xᵀ Cov(β) x where x = [1, year]ᵀ
        val varEta = covBeta
          .map { c =>
            c.v00 + 2 * y * c.v01 + y.toDouble * y.toDouble * c.v11
          }
          .getOrElse(0.0)
        val seEta = math.sqrt(math.max(varEta, 0.0))
        val fit = math.exp(eta)
        val lower = math.exp(eta - 1.96 * seEta)
        val upper = math.exp(eta + 1.96 * seEta)
        (year = y, fit = fit, lower = lower, upper = upper)
      }

      val obsData = years.zip(counts).map { case (y, c) => (year = y, count = c) }

      poissonTrend.plot(
        _.title(s"Poisson Trend: β₀=${f"$beta0%.3f"}±${f"$seBeta0%.3f"}, β₁=${f"$beta1%.5f"}±${f"$seBeta1%.5f"}"),
        _.layer._0.data.values := ciData.asJson,
        _.layer._1.data.values := ciData.asJson,
        _.layer._2.data.values := ciData.asJson,
        _.layer._3.data.values := ciData.asJson,
        _.layer._4.data.values := obsData.asJson
      )
    end plotTrend
  end extension

  extension (scenario: Scenarr)
    inline def plotSeasonality(highlight: Option[(year: Int, month: Int)] = None)(using
        tgt: viz.LowPriorityPlotTarget
    ) =
      val calc = scenario.monthYear.zip(scenario.amounts).groupMapReduce(_._1)(_._2)(_ + _).toVector
      val normaliseBy = calc.map(_._2).sum // total of all claims
      val sorted = calc
        .sortBy(row => (row._1.year, row._1.month))
        .map(row =>
          (
            category =
              s"${row._1.month.getDisplayName(java.time.format.TextStyle.SHORT, java.util.Locale.getDefault())} ${row._1.year}",
            amount = row._2 / normaliseBy,
            color = highlight.exists(h => h.year == row._1.year && h.month == row._1.month.getValue)
          )
        )

      seasonality.plot(
        _.title("Seasonality " + scenario.name),
        _.data.values := sorted.asJson
      )
  end extension

  extension (mixed: Mixed)
    /** Plot log-log comparison of theoretical Mixed distribution vs empirical sample data.
      *
      * This plot shows the complementary CDF (1 - CDF) on log-log scale, which is useful for visualizing tail behavior
      * of heavy-tailed distributions. The x-axis shows log(value) and y-axis shows log(1 - CDF).
      */
    def plotLogLogVsSample(samples: IndexedSeq[Double], threshold: Double)(using viz.LowPriorityPlotTarget) =
      val sortedSamples = samples.sorted
      val n = sortedSamples.length.toDouble

      // Empirical survival function using Hazen plotting position
      val empiricalData = sortedSamples.zipWithIndex.collect {
        case (x, i) if x > 0 =>
          val survivalProb = (n - i - 0.5) / n
          if survivalProb > 0 then Some((x = x, y = survivalProb, source = "empirical"))
          else None
          end if
      }.flatten

      // Theoretical survival function (1 - CDF)
      // For Pareto: S(x) = (xₘ/x)^α, so log(S) = α·log(xₘ) - α·log(x) is linear
      val minX = sortedSamples.filter(_ > 0).headOption.getOrElse(1.0)
      val maxX = sortedSamples.last
      val numPoints = 200
      val theoreticalData = (0 until numPoints).flatMap { i =>
        val x = minX * math.pow(maxX / minX, i.toDouble / (numPoints - 1))
        val survivalProb = 1.0 - mixed.cdf(x)
        if survivalProb > 1e-10 && x > 0 then Some((x = x, y = survivalProb, source = "model"))
        else None
        end if
      }

      val allData = theoreticalData ++ empiricalData

      logLogPlot.plot(
        _.title("Mixed Distribution Log-Log Plot"),
        _.data.values := allData.asJson,
        _.layer._0.encoding.x.scale.domainMin := threshold
      )
    end plotLogLogVsSample
  end extension

  extension (hp: HillPlotResult)
    /** Plot a Hill plot showing tail index estimates α̂(k) vs k.
      *
      * A Hill plot helps identify the optimal number of upper order statistics to use for Pareto tail estimation. Look
      * for a stable plateau region where the estimate is relatively constant - this indicates the range of k where the
      * Pareto assumption holds. Too small k gives high variance; too large k includes non-tail observations.
      */
    def plotHill(using viz.LowPriorityPlotTarget) =
      val data = hp.kValues.zip(hp.estimates).map { case (k, est) =>
        (k = k, estimate = est)
      }

      hillPlotSpec.plot(
        _.title("Hill Plot for Pareto Tail Index Estimation"),
        _.data.values := data.asJson
      )
    end plotHill
  end extension

  extension (pp: PickandsPlotResult)
    /** Plot a Pickands plot showing tail index estimates α̂(k) = 1/γ̂(k) vs k.
      *
      * The Pickands estimator is more robust than Hill to model misspecification but has higher variance. Look for a
      * stable plateau region. Unlike the Hill plot, the Pickands estimator can give negative γ values for light-tailed
      * distributions; only positive γ (and thus positive α) indicates heavy tails.
      */
    def plotPickands(using viz.LowPriorityPlotTarget) =
      val data = pp.kValues
        .zip(pp.alphaEstimates)
        .filter { case (_, est) => !est.isNaN && est.isFinite && est > 0 }
        .map { case (k, est) =>
          (k = k, estimate = est)
        }

      hillPlotSpec.plot(
        _.title("Pickands Plot for Pareto Tail Index Estimation"),
        _.data.values := data.asJson
      )
    end plotPickands

    /** Plot the raw extreme value index γ̂(k) from Pickands estimator.
      *
      * For heavy-tailed data, γ > 0. For light-tailed data (e.g., exponential), γ = 0. For bounded distributions, γ <
      * 0.
      */
    def plotPickandsGamma(using viz.LowPriorityPlotTarget) =
      val data = pp.kValues
        .zip(pp.gammaEstimates)
        .filter { case (_, est) => !est.isNaN && est.isFinite }
        .map { case (k, est) =>
          (k = k, estimate = est)
        }

      hillPlotSpec.plot(
        _.title("Pickands Plot for Extreme Value Index γ"),
        _.data.values := data.asJson
      )
    end plotPickandsGamma
  end extension

  // extension (negBin: NegativeBinomial)
  //   inline def plotPdf(using viz.LowPriorityPlotTarget) =
  //     val numPoints = 1000
  //     val maxX = negBin.mean + 4 * math.sqrt(negBin.variance)
  //     val data = (0 until numPoints).map { i =>
  //       val x = i.toDouble * maxX / numPoints
  //       (value = x, density = negBin.probabilityOf(x.round.toInt))
  //     }

  //     distributionDensity.plot(
  //       _.title(s"Negative Binomial Distribution Density (a=${negBin.a}, b=${negBin.b})"),
  //       _.data.values := data.asJson
  //     )

  //   inline def plotCdf(using viz.LowPriorityPlotTarget) =
  //     val numPoints = 1000
  //     val maxX = negBin.mean + 4 * math.sqrt(negBin.variance)
  //     val data = (0 until numPoints).map { i =>
  //       val x = i.toDouble * maxX / numPoints
  //       (value = x, density = negBin.cdf(x))
  //     }

  //     distributionDensity.plot(
  //       _.title(s"Negative Binomial Distribution CDF (a=${negBin.a}, b=${negBin.b})"),
  //       _.data.values := data.asJson
  //     )
  // end extension
end Plots
