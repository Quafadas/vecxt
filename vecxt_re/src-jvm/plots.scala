package vecxt_re

import vecxt_re.HillEstimator.HillPlotResult
import vecxt_re.PickandsEstimator.PickandsPlotResult

import io.circe.Json
import org.apache.commons.statistics.distribution.ChiSquaredDistribution
import io.circe.syntax.*
import io.github.quafadas.plots.SetupVega.{*, given}
import viz.vega.VegaSpec

object Plots:

  /** A modification applied to a plot's JSON spec, after the plot's own modifications (data, title, ...) have been
    * applied. Every plot in this object accepts a `mods` argument of these.
    *
    * @example
    *   {{{hillResult.plotHill(Seq(Plots.fixedSize(600, 400)))}}}
    */
  type SpecMod = viz.macros.SpecMod

  /** Replace the top-level `width` / `height` (which default to `"container"`) with fixed pixel sizes. Use this in
    * notebooks, where sizing to the container causes the plot to grow indefinitely.
    */
  def fixedSize(width: Int = 600, height: Int = 400): SpecMod =
    _.mapObject(_.add("width", Json.fromInt(width)).add("height", Json.fromInt(height)))

  /** Apply the plot's own typed modifications, followed by the caller supplied `mods`. */
  private[vecxt_re] def render[M](spec: VegaSpec[M], mods: Seq[SpecMod])(own: (M => SpecMod)*)(using
      viz.LowPriorityPlotTarget
  ): viz.VizReturn =
    spec.plotWith(own.map(_(spec.mod)) ++ mods)

  // These must be private otherwise scaladoc get crazy.
  private lazy val timeline = VegaPlot.fromResource("timeline.vl.json") // riskInceptionDate, riskExpiryDate
  private lazy val seasonality = VegaPlot.fromResource("seasonality.vg.json") // catagory, amount
  private lazy val distributionDensity = VegaPlot.fromResource("distDensity.vg.json") // value, density
  private lazy val negBinCdfWSample = VegaPlot.fromResource("negBinCumul_vsSample.vl.json") // value, density
  private lazy val ecdfVsCdf = VegaPlot.fromResource("ecdfVsCdf.vl.json") // theoretical and empirical CDF
  private lazy val rootogram = VegaPlot.fromResource("rootogram.vl.json") // hanging rootogram
  private lazy val pearsonResiduals = VegaPlot.fromResource("pearsonResiduals.vl.json") // residual plot
  private lazy val poissonTrend = VegaPlot.fromResource("poissonTrend.vl.json") // Poisson GLM trend
  private lazy val severityTrend = VegaPlot.fromResource("severityTrend.vl.json") // log(severity) ~ year
  private lazy val lossStems = VegaPlot.fromResource("stemChartWithRollingAverage.vl.json") // losses against date
  private lazy val logLogPlot = VegaPlot.fromResource("loglogCdf.vl.json") // log-log CDF plot
  private lazy val hillPlotSpec = VegaPlot.fromResource("hillPlot.vl.json") // Hill plot for tail index

  extension (idx: CalendarYearRateChanges)
    /** Plot the per-year index factors, the shortfall of each year's level relative to the current year, and the
      * reporting threshold on-levelled to the current year.
      *
      * @param reportingThreshold
      *   The reporting threshold, in `thresholdYear` money
      * @param thresholdYear
      *   The year the reporting threshold was set
      */
    def plotIndex(reportingThreshold: Double, thresholdYear: Int, mods: Seq[SpecMod] = Nil)(using
        viz.LowPriorityPlotTarget
    ) =
      val linePlot2 = VegaPlot.fromResource("index.vl.json")
      val cumulative = idx.onLevel(Array.fill(idx.numYears)(1.0), idx.years)
      val threshold = idx.suggestedNewThreshold(reportingThreshold, thresholdYear)
      val factors = idx.years.indices.map { i =>
        (
          year = idx.years(i),
          index = idx.factors(i),
          missing = 1 / cumulative(i),
          threshold = threshold
        )
      }
      render(linePlot2, mods)(
        _.data.values := factors.asJson
      )
  end extension

  extension (idx: CalendarYearLevels)
    /** Plot the raw index level for each year. The tooltip shows the factor which on-levels each year to the current
      * year.
      */
    def plotLevels(mods: Seq[SpecMod] = Nil)(using viz.LowPriorityPlotTarget) =
      val spec = VegaPlot.fromResource("indexLevels.vl.json")
      val rows = idx.years.indices.map { i =>
        (
          year = idx.years(i),
          level = idx.levels(i),
          onLevelFactor = idx.onLevelFactor(idx.years(i))
        )
      }
      render(spec, mods)(
        _.data.values := rows.asJson
      )
  end extension

  extension (nb: NegativeBinomial)

    /** Plot ECDF vs theoretical CDF as step functions for visual goodness-of-fit assessment.
      *
      * Both curves are step functions. Deviations between the orange (empirical) and blue (theoretical) lines indicate
      * potential model misfit.
      */
    def plotEcdfVsCdf(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      plotEcdfVsCdf(samples, Nil)

    /** As [[plotEcdfVsCdf]], applying `mods` to the spec after the built-in modifications. */
    def plotEcdfVsCdf(samples: IndexedSeq[Int], mods: Seq[SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
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

      render(ecdfVsCdf, mods)(
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
      *
      * A tick marks each bar bottom, so a bucket with zero observations (bottom = top) is still visible. The grey band
      * marks ±1, the usual rule of thumb for a meaningful deviation on the √ scale.
      */
    def plotRootogram(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      plotRootogram(samples, Nil)

    /** As [[plotRootogram]], applying `mods` to the spec after the built-in modifications. */
    def plotRootogram(samples: IndexedSeq[Int], mods: Seq[SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
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

      render(rootogram, mods)(
        _.title(s"NegBin(a=${nb.a}, b=${nb.b}) Hanging Rootogram"),
        _.data.values := data.asJson
      )
    end plotRootogram

    /** Plot Pearson residuals per count value k: (Oₖ − Eₖ) / √Eₖ, where Oₖ is the number of samples equal to k and Eₖ =
      * n·P(X = k) under the fitted distribution.
      *
      * The denominator is √Eₖ because the number of samples landing in bucket k is approximately Poisson(Eₖ), whatever
      * distribution generated the samples. The Negative Binomial variance μ(1 + μ/a) is the variance of a single draw
      * X, not of a bucket count, so it does not belong here. Residuals beyond ±2 (shown in red) indicate significant
      * deviation from the fitted model.
      */
    def plotPearsonResiduals(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      plotPearsonResiduals(samples, Nil)

    /** As [[plotPearsonResiduals]], applying `mods` to the spec after the built-in modifications. */
    def plotPearsonResiduals(samples: IndexedSeq[Int], mods: Seq[SpecMod])(using
        viz.LowPriorityPlotTarget
    ): viz.VizReturn =
      val n = samples.length.toDouble
      val maxK = math.max(samples.max, (nb.mean + 3 * math.sqrt(nb.variance)).toInt)
      val counts = samples.groupMapReduce(identity)(_ => 1)(_ + _)

      val data = (0 to maxK).flatMap { k =>
        val observed = counts.getOrElse(k, 0)
        val expected = nb.probabilityOf(k) * n
        // Only include if expected > 0 to avoid division by zero
        if expected > 0.001 then
          // Bucket count Oₖ ≈ Poisson(Eₖ), so its standard deviation is √Eₖ
          val residual = (observed - expected) / math.sqrt(expected)
          Some((k = k, residual = residual))
        else None
        end if
      }

      render(pearsonResiduals, mods)(
        _.title(s"NegBin(a=${nb.a}, b=${nb.b}) Pearson Residuals"),
        _.data.values := data.asJson
      )
    end plotPearsonResiduals
  end extension

  extension (p: Poisson)
    /** Plot ECDF vs theoretical CDF as step functions for visual goodness-of-fit assessment. */
    def plotEcdfVsCdf(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      plotEcdfVsCdf(samples, Nil)

    /** As [[plotEcdfVsCdf]], applying `mods` to the spec after the built-in modifications. */
    def plotEcdfVsCdf(samples: IndexedSeq[Int], mods: Seq[SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
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

      render(ecdfVsCdf, mods)(
        _.title(s"Poisson(λ=${p.lambda}) ECDF vs Theoretical CDF"),
        _.layer._0.data.values := theoreticalCdf.asJson,
        _.layer._1.data.values := empiricalCdf.asJson
      )
    end plotEcdfVsCdf

    /** Plot a hanging rootogram for Poisson count data diagnostics.
      *
      * Bars hang from sqrt(expected) down to sqrt(expected) - sqrt(observed). Good fit means bars end near zero.
      */
    def plotRootogram(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      plotRootogram(samples, Nil)

    /** As [[plotRootogram]], applying `mods` to the spec after the built-in modifications. */
    def plotRootogram(samples: IndexedSeq[Int], mods: Seq[SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
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

      render(rootogram, mods)(
        _.title(s"Poisson(λ=${p.lambda}) Hanging Rootogram"),
        _.data.values := data.asJson
      )
    end plotRootogram

    /** Plot Pearson residuals for Poisson: (observed - expected) / sqrt(expected).
      *
      * For Poisson, variance = mean, so the denominator is simply sqrt(expected). Residuals beyond ±2 (red) suggest
      * significant deviation. Systematic patterns may indicate overdispersion (consider Negative Binomial).
      */
    def plotPearsonResiduals(samples: IndexedSeq[Int])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      plotPearsonResiduals(samples, Nil)

    /** As [[plotPearsonResiduals]], applying `mods` to the spec after the built-in modifications. */
    def plotPearsonResiduals(samples: IndexedSeq[Int], mods: Seq[SpecMod])(using
        viz.LowPriorityPlotTarget
    ): viz.VizReturn =
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

      render(pearsonResiduals, mods)(
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
    def plotTrend(years: IndexedSeq[Int], counts: IndexedSeq[Int], mods: Seq[SpecMod] = Nil)(using
        viz.LowPriorityPlotTarget
    ) =
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

      render(poissonTrend, mods)(
        _.title(s"Poisson Trend: β₀=${f"$beta0%.3f"}±${f"$seBeta0%.3f"}, β₁=${f"$beta1%.5f"}±${f"$seBeta1%.5f"}"),
        _.layer._0.data.values := ciData.asJson,
        _.layer._1.data.values := ciData.asJson,
        _.layer._2.data.values := ciData.asJson,
        _.layer._3.data.values := ciData.asJson,
        _.layer._4.data.values := obsData.asJson
      )
    end plotTrend
  end extension

  /** Stem plot of individual losses against their date: the amount as reported, the normalised (on-levelled) amount,
    * and a trailing rolling average of the normalised amounts.
    *
    * Use it to look at the data behind [[plotSeverityTrend]]: clusters of losses in time, single years that dominate,
    * and how much normalisation moves the older losses.
    *
    * The rolling average for end year `Y` is the mean normalised amount of the losses dated in years `Y - rollingYears
    * + 1` to `Y`, plotted at 31 December of `Y`. It is shown for every end year whose window lies inside the data and
    * contains at least one loss.
    *
    * @param dates
    *   the date of each loss
    * @param amounts
    *   the amount of each loss as reported
    * @param normalisedAmounts
    *   the amount of each loss normalised to the current year, same length as dates
    * @param rollingYears
    *   the length of the rolling average window, in years
    */
  def plotLossStems(
      dates: IndexedSeq[java.time.LocalDate],
      amounts: IndexedSeq[Double],
      normalisedAmounts: IndexedSeq[Double],
      rollingYears: Int = 3,
      title: String = "Losses over time",
      mods: Seq[SpecMod] = Nil
  )(using viz.LowPriorityPlotTarget): viz.VizReturn =
    require(
      dates.length == amounts.length && dates.length == normalisedAmounts.length,
      "dates, amounts and normalisedAmounts must have the same length"
    )
    require(dates.nonEmpty, "need at least one loss")
    require(rollingYears >= 1, "rollingYears must be at least 1")

    val stems = dates.indices.map { i =>
      (date = dates(i).toString, amount = amounts(i), normalisedAmount = normalisedAmounts(i))
    }

    val years = dates.map(_.getYear)
    val (firstYear, lastYear) = (years.min, years.max)
    val rolling = (firstYear + rollingYears - 1 to lastYear).flatMap { end =>
      val start = end - rollingYears + 1
      val inWindow = years.indices.filter(i => years(i) >= start && years(i) <= end)
      Option.when(inWindow.nonEmpty)(
        (
          date = java.time.LocalDate.of(end, 12, 31).toString,
          rollingAverage = inWindow.map(normalisedAmounts).sum / inWindow.length,
          window = if rollingYears == 1 then s"$end" else s"$start–$end",
          n = inWindow.length
        )
      )
    }

    render(lossStems, mods)(
      _.title(title),
      _.datasets.stemData := stems.asJson,
      _.datasets.rollingAverageData := rolling.asJson
    )
  end plotLossStems

  /** Plot a severity trend: log(Severity) ~ 1 + Year, fitted by [[TrendAnalysis.fitSeverityTrend]].
    *
    * Shows each loss on a log scale, the fitted median severity exp(β₀ + β₁·year) and a `(1 − alpha)` confidence band
    * for it. The title gives the annual trend, its confidence interval (the range of trends the data cannot rule out)
    * and the p-value of the slope. The subtitle gives the range of trends when each loss is left out in turn, and each
    * loss's tooltip shows the trend without it. Clicking a loss redraws the fit and band without it; double-clicking
    * restores all losses.
    *
    * With small data volumes the result is not robust: read it as a check for gross errors in the normalisation, not as
    * a measurement of the trend.
    *
    * @param years
    *   the year of each loss
    * @param losses
    *   the (on-levelled) loss amounts, all strictly positive, same length as years
    * @param alpha
    *   `1 − alpha` is the confidence level of the band and the interval in the title
    */
  def plotSeverityTrend(
      years: IndexedSeq[Int],
      losses: IndexedSeq[Double],
      alpha: Double = 0.05,
      mods: Seq[SpecMod] = Nil
  )(using viz.LowPriorityPlotTarget): viz.VizReturn =
    val fit = TrendAnalysis.fitSeverityTrend(years, losses)
    val ci = fit.annualTrendCI(alpha)
    val level = math.round((1 - alpha) * 100)
    val looFits = TrendAnalysis.fitSeverityTrendLeaveOneOut(years, losses)
    def pct(x: Double) = f"${x * 100}%+.1f%%"
    def pNum(p: Double) = if p < 0.001 then "<0.001" else f"$p%.3f"
    def pv(p: Double) = if p < 0.001 then "p < 0.001" else s"p = ${pNum(p)}"
    def describe(f: SeverityTrendResult) =
      val c = f.annualTrendCI(alpha)
      s"${pct(f.annualTrend)} p.a. ($level% CI ${pct(c.lower)} to ${pct(c.upper)}, ${pv(f.pValueSlope)})"
    end describe

    val title = s"Severity trend ${describe(fit)}"
    val loo = looFits.flatten
    val subtitle =
      if loo.isEmpty then "Too few losses to leave one out"
      else
        val t = loo.map(_.annualTrend)
        val p = loo.map(_.pValueSlope)
        s"Leaving out one loss at a time: ${pct(t.min)} to ${pct(t.max)} p.a., p from ${pNum(p.min)} to ${pNum(p.max)}"

    // The band for every fit: all losses (dropped = -1), and each leave-one-out refit (dropped = index of the loss
    // left out). The plot shows the one matching the clicked loss.
    val fits = (-1 -> fit) +: looFits.zipWithIndex.collect { case (Some(f), i) => i -> f }
    val yearRange = years.min to years.max
    val bandData = fits.flatMap { (dropped, f) =>
      yearRange.map { y =>
        val l = f.fittedLog(y.toDouble, alpha)
        (dropped = dropped, year = y, fit = math.exp(l.fit), lower = math.exp(l.lower), upper = math.exp(l.upper))
      }
    }
    val textData =
      (dropped = -1, text = "Click a loss to refit without it, double-click to restore") +:
        fits.tail.map((i, f) => (dropped = i, text = s"Without the ${years(i)} loss: ${describe(f)}"))
    val obsData = years.indices.map { i =>
      val without = looFits(i).fold("n/a")(f => s"${pct(f.annualTrend)}, ${pv(f.pValueSlope)}")
      (id = i, year = years(i), loss = losses(i), trendWithout = without)
    }

    render(severityTrend, mods)(
      _ =>
        _.mapObject(
          _.add("title", Json.obj("text" -> Json.fromString(title), "subtitle" -> Json.fromString(subtitle)))
        ),
      _.layer._0.data.values := bandData.asJson,
      _.layer._1.data.values := bandData.asJson,
      _.layer._2.data.values := obsData.asJson,
      _.layer._3.data.values := textData.asJson
    )
  end plotSeverityTrend

  extension (scenario: Scenarr)
    def plotSeasonality(
        highlight: Option[(year: Int, month: Int)] = None,
        mods: Seq[SpecMod] = Nil
    )(using
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

      render(seasonality, mods)(
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
    def plotLogLogVsSample(samples: IndexedSeq[Double], threshold: Double, mods: Seq[SpecMod] = Nil)(using
        viz.LowPriorityPlotTarget
    ) =
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

      render(logLogPlot, mods)(
        _.title("Mixed Distribution Log-Log Plot"),
        _.data.values := allData.asJson,
        _.layer._0.encoding.x.scale.domainMin := threshold
      )
    end plotLogLogVsSample
  end extension

  extension (hp: HillPlotResult)
    /** Plot a Hill plot: the tail index estimate α̂(k) against k / n, with a 95% interval.
      *
      * k / n is the empirical exceedance probability of the threshold X₍ₙ₋ₖ₎. The points are evenly spaced, one per
      * loss: the left end uses only the largest losses, and each step right lowers the threshold by one loss. The
      * threshold is in each point's tooltip. Look for a plateau. To the left the estimate is noisy; to the right it
      * pulls in losses from outside the tail.
      *
      * The band is exact if the losses above the threshold are Pareto: 2α·Σ log(Xᵢ / threshold) is then χ²(2k), so α
      * lies in [α̂·χ²₂ₖ(0.025) / 2k, α̂·χ²₂ₖ(0.975) / 2k]. It does not include bias from losses that are not Pareto,
      * which is what drifting to the right shows. If the result does not record n, the x-axis is k instead.
      */
    def plotHill(using viz.LowPriorityPlotTarget): viz.VizReturn = plotHill(Nil)

    /** As [[plotHill]], applying `mods` to the spec after the built-in modifications. */
    def plotHill(mods: Seq[SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      val hasThresholds = hp.thresholds.length == hp.kValues.length
      val hasN = hp.nObs > 0
      val data = hp.kValues.indices.map { i =>
        val k = hp.kValues(i)
        val est = hp.estimates(i)
        val chi2 = ChiSquaredDistribution.of(2.0 * k)
        (
          x = if hasN then k.toDouble / hp.nObs else k.toDouble,
          k = k,
          threshold = Option.when(hasThresholds)(hp.thresholds(i)),
          estimate = est,
          lower = est * chi2.inverseCumulativeProbability(0.025) / (2 * k),
          upper = est * chi2.inverseCumulativeProbability(0.975) / (2 * k)
        )
      }
      val byK: SpecMod = _.hcursor
        .downField("encoding")
        .downField("x")
        .withFocus(_.mapObject(_.add("title", Json.fromString("k (number of largest losses used)")).remove("axis")))
        .top
        .getOrElse(Json.Null)

      render(hillPlotSpec, if hasN then mods else byK +: mods)(
        _.title("Hill plot: Pareto tail index"),
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
    def plotPickands(using viz.LowPriorityPlotTarget): viz.VizReturn = plotPickands(Nil)

    /** As [[plotPickands]], applying `mods` to the spec after the built-in modifications. */
    def plotPickands(mods: Seq[SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      val data = pp.kValues
        .zip(pp.alphaEstimates)
        .filter { case (_, est) => !est.isNaN && est.isFinite && est > 0 }
        .map { case (k, est) =>
          (k = k, estimate = est)
        }

      render(hillPlotSpec, mods)(
        _.title("Pickands Plot for Pareto Tail Index Estimation"),
        _.data.values := data.asJson
      )
    end plotPickands

    /** Plot the raw extreme value index γ̂(k) from Pickands estimator.
      *
      * For heavy-tailed data, γ > 0. For light-tailed data (e.g., exponential), γ = 0. For bounded distributions, γ <
      * 0.
      */
    def plotPickandsGamma(using viz.LowPriorityPlotTarget): viz.VizReturn = plotPickandsGamma(Nil)

    /** As [[plotPickandsGamma]], applying `mods` to the spec after the built-in modifications. */
    def plotPickandsGamma(mods: Seq[SpecMod])(using viz.LowPriorityPlotTarget): viz.VizReturn =
      val data = pp.kValues
        .zip(pp.gammaEstimates)
        .filter { case (_, est) => !est.isNaN && est.isFinite }
        .map { case (k, est) =>
          (k = k, estimate = est)
        }

      render(hillPlotSpec, mods)(
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
