# Plots

vecxt_re can plot its distributions, fit diagnostics, indices and scenarios. Every plot is a Vega-Lite spec, shown through a **plot target** that you bring into scope:

| Target | Use |
|---|---|
| `viz.PlotTargets.desktopBrowser` | open in the default browser |
| `viz.PlotTargets.almond` | inline in an Almond (Jupyter) notebook |
| `viz.PlotTargets.tempHtmlFile`, `viz.PlotTargets.png` | write to a file |

The examples below assume:

```scala mdoc:silent
import vecxt_re.all.*
import viz.PlotTargets.desktopBrowser
```

Each example is type-checked against the library on every docs build, but not run. Values written `???` are your own inputs.

Every plot also accepts `mods: Seq[SpecMod]`, which edits the Vega-Lite JSON after the plot's own settings are applied. The one you will most often want is `fixedSize(width, height)`. Both come from `vecxt_re.Plots`, and `vecxt_re.all.*` exports them. Plots size themselves to their container by default, which can grow without limit in a notebook:

```scala mdoc:compile-only
NegativeBinomial(4.0, 2.0).plot(Seq(fixedSize(600, 400)))
```

## Distributions

`NegativeBinomial`, `Poisson`, `Pareto`, `Empirical` and `Mixed` all have `plot` (the probability mass or density function) and `plotCdf`:

```scala mdoc:compile-only
NegativeBinomial(a = 4.0, b = 2.0).plot
Poisson(6.5).plotCdf
Pareto(scale = 10.0e6, shape = 1.4).plot
```

## Fit diagnostics for count models

`NegativeBinomial` and `Poisson` have three plots comparing a fitted distribution with the counts it was fitted to. These are the natural follow-up to [`mle` / `volweightedMle`](frequency-above-threshold.md).

```scala mdoc:compile-only
val counts: Array[Int] = ???
val fitted = NegativeBinomial.mle(counts).dist

fitted match
  case nb: NegativeBinomial =>
    nb.plotEcdfVsCdf(counts.toIndexedSeq)
    nb.plotRootogram(counts.toIndexedSeq)
    nb.plotPearsonResiduals(counts.toIndexedSeq)
  case p: Poisson =>
    p.plotRootogram(counts.toIndexedSeq)
```

| Plot | Shows | Look for |
|---|---|---|
| `plotEcdfVsCdf` | empirical CDF against the fitted CDF, both as step functions | systematic gaps between the two curves |
| `plotRootogram` | bars hanging from √expected down to √observed, for each count value k | bar ends far from zero. Below zero means the model under-predicts that count; above zero means it over-predicts. The grey band marks ±1. |
| `plotPearsonResiduals` | (O<sub>k</sub> − E<sub>k</sub>)/√E<sub>k</sub> for each count value k | residuals beyond ±2 (red), and systematic patterns. For a Poisson fit, a U shape suggests overdispersion. |

With only 10–30 years of counts, all three plots are noisy. Treat them as a check for gross misfit, not as a test.

## Trend in counts

`plotTrend` fits a Poisson regression log(count) = β₀ + β₁·year by iteratively reweighted least squares, and plots the observations, the fitted trend and a 95% confidence band. The title shows β₀ and β₁ with their standard errors.

```scala mdoc:compile-only
val years: IndexedSeq[Int] = ???
val counts: IndexedSeq[Int] = ???

Poisson(1.0).plotTrend(years, counts)
```

`plotTrend` is defined on a `Poisson` value, but it fits its own model from `years` and `counts`; the value's λ is not used. Use it for the stationarity check in the [frequency recipe](frequency-above-threshold.md#checks-before-relying-on-it): a clearly non-zero β₁ means the years are not draws from one distribution. With a short history the confidence band is wide, so the check catches only large trends, such as a change in reporting or event definition. It cannot confirm that frequency is stable.

## Trend in severity

With small data volumes, no statistical test of severity trend is robust. The trend in the forecast should come from the normalisation index and from outside evidence: market inflation, industry losses, similar portfolios. The portfolio's own losses can only check whether that index is consistent with them. Use two plots for that check:

- a stem plot, to look at the losses and use judgement;
- a regression, to show the range of trends the data cannot rule out, and how much that range depends on single losses.

Expect either to catch only clear problems: a missing normalisation step, an index applied twice or in the wrong direction, mixed currencies, a wrong date.

### Look: losses over time

`plotLossStems` draws each loss at its date: the amount as reported (blue), the normalised amount (orange, joined to the reported amount by a stem), and a rolling average of the normalised amounts (green).

```scala mdoc:compile-only
import java.time.LocalDate

val dates: IndexedSeq[LocalDate] = ???
val amounts: IndexedSeq[Double] = ???    // as reported
val normalised: IndexedSeq[Double] = ??? // on-levelled to the current year

plotLossStems(dates, amounts, normalised, rollingYears = 3)
```

The rolling average for end year Y is the mean normalised loss dated in years Y − `rollingYears` + 1 to Y. It is drawn at 31 December of Y, and only for windows that lie inside the data. With few losses per window it moves a lot from year to year; look at its overall direction, not at individual steps. Look for:

- **Clusters.** Several losses close together, from one season, or one event split into several claims. The regression counts them as separate observations, but they are not independent.
- **One loss driving the picture.** A single large loss near either end of the period can create a trend, or hide one, on its own.
- **Stem length against age.** Stems should grow steadily towards the past. If an old loss has a stem out of line with its neighbours, check its date and the index for that year.
- **Direction after normalisation.** If normalised losses fall over time, the index may be too steep. If they rise, something may be missing from it.

### Check: regression on log severity

`plotSeverityTrend` fits log(loss) = β₀ + β₁·year by ordinary least squares. It plots each loss on a log scale, with the fitted median severity and a confidence band.

- **The title** gives the annual trend exp(β₁) − 1, its confidence interval (the range of trends the data cannot rule out) and the p-value of β₁. `alpha` (default 0.05) sets the confidence level.
- **The subtitle** gives the range of trends and p-values when each loss is left out in turn. Each loss's tooltip shows the trend and p-value without it.
- **Clicking a loss** redraws the fit and band without it, and shows that refit's trend, interval and p-value on the chart. Double-click to restore all losses.

```scala mdoc:compile-only
val years: IndexedSeq[Int] = ???
val losses: IndexedSeq[Double] = ??? // on-levelled, above a fixed threshold

plotSeverityTrend(years, losses)

val fit = fitSeverityTrend(years, losses)
fit.annualTrendCI() // 95% interval for exp(β₁) − 1
println(fit.summary)

// trend with each loss left out in turn
val withoutEach = fitSeverityTrendLeaveOneOut(years, losses).map(_.map(_.annualTrend))
```

Read the confidence interval against the trend you expect the index to have removed:

- **The interval excludes zero by a wide margin, and leaving out any one loss does not change that.** The data contradicts the normalisation. Look for the cause in the stem plot and the index.
- **The interval is wide and includes zero.** The data is consistent with the index, but cannot confirm it. This is the usual result with small data volumes, and it is not evidence that there is no trend.
- **The result depends on one loss.** Significant with every loss included, but not with one of them left out. Treat it as a question about that loss (its date, its normalisation, whether it is really one event), not as a trend.

The p-value and the interval carry the same information: the interval excludes zero exactly when p < `alpha`. The p-value is a continuous measure of how compatible the data is with no trend. Read it as a degree of confidence, not as a pass or fail at 0.05. With small data volumes, how much it moves when a single loss is left out matters as much as its value. `fit.pValueSlope` and `fit.summary` give it outside the plot.

### Reading the result

- **Year or date?** The slope is the same whichever you use. The year is the date rounded down, and the rounding error is independent of the year, so the slope is not biased. The intercept shifts by half a year, and the residual variance grows by β₁²/12, which is negligible: 0.0002 at a 5% trend, against a typical σ² of 0.1 or more. Days since the start of the period give the same regression rescaled: the slope is per day, so the annual trend is exp(365.25·β₁) − 1, and the confidence interval is unchanged. Dates make the plot easier to read, which is why `plotLossStems` uses them.
- **Use the same date basis as the normalisation.** The index is applied per calendar year of loss. Regressing on a different date, such as underwriting or reporting year, mixes two bases, and the mismatch can look like a trend.
- **Losses in the same year are not independent.** They share inflation, the legal environment and sometimes an event. The regression treats every loss as independent, so if years differ systematically, the interval is too narrow.
- **A fixed threshold hides part of the trend.** Above a threshold that is fixed in normalised terms, a pure scale trend in a Pareto tail does not show in severity at all, because the Pareto is scale-free above its threshold. The trend appears in the [counts](#trend-in-counts) instead. For lighter tails such as the lognormal, the threshold pulls the slope towards zero. Check frequency as well.

## Tails

```scala mdoc:compile-only
val losses: Array[Double] = ???

HillEstimator.hillPlot(losses).plotHill
PickandsEstimator.pickandsPlot(losses).plotPickands
PickandsEstimator.pickandsPlot(losses).plotPickandsGamma
```

| Plot | Shows | Look for |
|---|---|---|
| `plotHill` | the Hill estimate of the Pareto tail index α̂ against k / n, the empirical exceedance probability of its threshold (the (k+1)-th largest observation), with a 95% interval that is exact for Pareto data. One evenly spaced point per loss; the threshold is in the tooltip | a stable plateau. The left end uses only the largest losses and is noisy; further right, losses from outside the tail pull the estimate away. With small data volumes the interval is wide at every threshold, so no plateau can be pinned down. |
| `plotPickands` | the Pickands estimate α̂(k) = 1/γ̂(k) | a plateau, as for Hill. Pickands is more robust to misspecification than Hill but noisier. |
| `plotPickandsGamma` | the Pickands extreme value index γ̂(k) | γ > 0 is a heavy tail, γ ≈ 0 exponential-like, γ < 0 bounded |

For a fitted `Mixed` distribution (empirical body with a Pareto tail), `plotLogLogVsSample` compares the model's survival function with the sample's on log-log axes. A Pareto tail is a straight line there:

```scala mdoc:compile-only
val mixed: Mixed = ???
val samples: IndexedSeq[Double] = ???

mixed.plotLogLogVsSample(samples, threshold = 10.0e6)
```

`threshold` sets where the x-axis starts.

## Indices

```scala mdoc:compile-only
val tivIndex: CalendarYearLevels = ???
tivIndex.plotLevels()

val rateChanges: CalendarYearRateChanges = ???
rateChanges.plotIndex(reportingThreshold = 10.0e6, thresholdYear = 2016)
```

- **`CalendarYearLevels.plotLevels`** plots the index level for each year. The tooltip shows the factor that on-levels each year to the current year.
- **`CalendarYearRateChanges.plotIndex`** plots, for each year:
    - the year-on-year index factor;
    - that year's level as a fraction of the current year's;
    - the reporting threshold, set in `thresholdYear`, on-levelled to the current year.

## Scenarios

```scala mdoc:compile-only
val scenario: Scenarr = ???

scenario.plotSeasonality()
scenario.plotSeasonality(highlight = Some((year = 2024, month = 9)))
```

`plotSeasonality` plots the scenario's losses summed by calendar month, as a share of the total. `highlight` picks out one month.
