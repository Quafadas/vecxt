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

`plotTrend` is defined on a `Poisson` value, but it fits its own model from `years` and `counts`; the value's λ is not used. Use it for the stationarity check in the [frequency recipe](frequency-above-threshold.md#checks-before-relying-on-it): a clearly non-zero β₁ means the years are not draws from one distribution.

## Tails

```scala mdoc:compile-only
val losses: Array[Double] = ???

HillEstimator.hillPlot(losses).plotHill
PickandsEstimator.pickandsPlot(losses).plotPickands
PickandsEstimator.pickandsPlot(losses).plotPickandsGamma
```

| Plot | Shows | Look for |
|---|---|---|
| `plotHill` | the Hill estimate of the Pareto tail index α̂(k), using the k largest observations | a stable plateau. Small k is noisy; large k pulls in observations from outside the tail. |
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
