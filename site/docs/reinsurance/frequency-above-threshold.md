# Calibrating event frequency above a threshold

A recipe for calibrating the frequency and severity of loss events above a threshold, for perils such as severe convective storm (hail, tornado), when:

- history is only available as **events whose raw (un-indexed) loss exceeded a fixed threshold** u,
- loss is taken to be **proportional to TIV**, so losses are on-levelled by the TIV ratio, and
- the portfolio's spatial footprint has **not changed materially** over the history.

The output is the forecast-year count distribution above a contract threshold, plus the severity above it.

## The model

Each year draws its own activity level, and events follow a Poisson process given that level:

<math display="block"><msub><mi>Λ</mi><mi>i</mi></msub><mo>∼</mo><mtext>Gamma</mtext><mo>(</mo><mi>r</mi><mo>,</mo><mi>β</mi><mo>)</mo><mo>,</mo><mspace width="1em"/><msub><mi>n</mi><mi>i</mi></msub><mo>∣</mo><msub><mi>Λ</mi><mi>i</mi></msub><mo>∼</mo><mtext>Poisson</mtext><mo>(</mo><msub><mi>Λ</mi><mi>i</mi></msub><mo>)</mo><mspace width="1em"/><mo>⇒</mo><mspace width="1em"/><msub><mi>n</mi><mi>i</mi></msub><mo>∼</mo><mtext>NB</mtext><mo>(</mo><mi>r</mi><mo>,</mo><mi>β</mi><mo>)</mo></math>

β is the Gamma scale. What is calibrated is the shared parameters (r, β): the mean rate rβ and its year-to-year variability. The fitted `NegativeBinomial(r, β)` is the **forecast-year distribution**, because next year is a fresh draw of Λ.

It is a plug-in estimate: uncertainty in (r, β) itself is not included. See [Parameter uncertainty](#parameter-uncertainty).

## Method: one common on-levelled threshold

### Why it works

On-level each raw loss to the forecast year's TIV:

<math display="block"><msub><mi>y</mi><mi>j</mi></msub><mo>=</mo><msub><mi>x</mi><mi>j</mi></msub><mo>·</mo><msub><mtext>TIV</mtext><mtext>model</mtext></msub><mo>/</mo><msub><mtext>TIV</mtext><mi>i</mi></msub></math>

Year i only recorded losses with x > u. In today's money, its record is therefore complete only above u<sub>i</sub> = u·TIV<sub>model</sub>/TIV<sub>i</sub>. That threshold differs by year, and is highest for the lowest-TIV year. Above

<math display="block"><msup><mi>u</mi><mo>*</mo></msup><mo>=</mo><mi>u</mi><mo>·</mo><munder><mo>max</mo><mi>i</mi></munder><mfrac><msub><mtext>TIV</mtext><mtext>model</mtext></msub><msub><mtext>TIV</mtext><mi>i</mi></msub></mfrac></math>

**every year's record is complete**. Counts of on-levelled losses above u\* are therefore directly comparable across years, with no volume adjustment. The on-levelled losses above u\* are a clean sample for the severity.

Frequency and severity are fitted to the same events, on the same basis, above the same threshold.

### Recipe

| Input | Meaning |
|---|---|
| `u` | the fixed raw threshold the history was recorded at |
| `years(i)` | the historical years, contiguous |
| `tivIndex` | a `CalendarYearLevels` of TIV by year, whose current year is the **forecast year** at TIV<sub>model</sub> |
| `rawLossesByYear(i)` | raw loss of every event in `years(i)` with loss > u; may be empty |
| `d` | contract threshold, d ≥ u\* |

Holding TIV as a `CalendarYearLevels` makes the on-levelling inspectable. `tivIndex.onLevelFactor(year)` is TIV<sub>model</sub>/TIV<sub>i</sub>, `tivIndex.suggestedNewThreshold(u, year)` is u<sub>i</sub>, and `tivIndex.plotLevels()` plots the index, with each year's on-level factor in the tooltip. `plotLevels` needs `import vecxt_re.all.*` and a plot target in scope.

1. **Common threshold:** u\* = max<sub>i</sub> u<sub>i</sub> = u · max<sub>i</sub>(TIV<sub>model</sub>/TIV<sub>i</sub>), the largest of the per-year `suggestedNewThreshold`s.
2. **On-level and filter:** y<sub>j</sub> = `tivIndex.onLevel(x, year)` = x<sub>j</sub>·TIV<sub>model</sub>/TIV<sub>i</sub>, keeping only y<sub>j</sub> > u\*.
3. **Severity:** a Pareto tail above u\*, using the maximum-likelihood estimate with known threshold:

    <math display="block"><mover><mi>α</mi><mo>^</mo></mover><mo>=</mo><mfrac><msup><mi>m</mi><mo>*</mo></msup><mrow><munder><mo>∑</mo><mi>j</mi></munder><mi>log</mi><mo>(</mo><msub><mi>y</mi><mi>j</mi></msub><mo>/</mo><msup><mi>u</mi><mo>*</mo></msup><mo>)</mo></mrow></mfrac></math>

    Here m\* is the number of on-levelled losses above u\*.
4. **Frequency:** fit `NegativeBinomial.mle` to the per-year counts above u\*. It returns NB(r, β), or Poisson if the counts are not overdispersed.
5. **Forecast year above d:** each event exceeds d with probability (u\*/d)<sup>α̂</sup>, so thin the frequency and take the severity above d from the same tail:

    <math display="block"><msub><mi>N</mi><mi>d</mi></msub><mo>∼</mo><mtext>NB</mtext><mo>(</mo><mi>r</mi><mo>,</mo><mi>β</mi><mo>·</mo><msup><mrow><mo>(</mo><msup><mi>u</mi><mo>*</mo></msup><mo>/</mo><mi>d</mi><mo>)</mo></mrow><mover><mi>α</mi><mo>^</mo></mover></msup><mo>)</mo><mo>,</mo><mspace width="2em"/><mi>X</mi><mo>∣</mo><mi>X</mi><mo>&gt;</mo><mi>d</mi><mo>∼</mo><mtext>Pareto</mtext><mo>(</mo><mi>d</mi><mo>,</mo><mover><mi>α</mi><mo>^</mo></mover><mo>)</mo></math>

Losses are in today's money throughout.

### Code

The inputs. `rawLossesByYear` would be your data. Here it is a seeded synthetic sample generated from the model itself, so the page is reproducible. Each year has NB(4, 4) events above u at today's TIV with Pareto(u, 1.4) losses, scaled back to that year's TIV, keeping what a raw record would hold.

```scala mdoc:silent
import vecxt_re.*
import org.apache.commons.rng.simple.RandomSource
import org.apache.commons.statistics.distribution.{GammaDistribution, PoissonDistribution}

val u = 10.0e6
val years = (2016 to 2025).toArray
val tivIndex = CalendarYearLevels(
  years :+ 2026, // the forecast year is the index's current year
  Array(55.0, 58, 62, 66, 70, 75, 81, 86, 92, 100) :+ 105.0
)

val rng = RandomSource.XO_RO_SHI_RO_128_PP.create(java.lang.Long.valueOf(42L))
val activity = GammaDistribution.of(4.0, 4.0).createSampler(rng)
val rawLossesByYear: Array[Array[Double]] = years.map { y =>
  val n = PoissonDistribution.of(activity.sample()).createSampler(rng).sample()
  Array
    .fill(n)(u * math.pow(1.0 - rng.nextDouble(), -1.0 / 1.4)) // Pareto(u, 1.4) at today's TIV
    .map(_ / tivIndex.onLevelFactor(y)) // back to year y's TIV
    .filter(_ > u)
}
```

The recipe:

```scala mdoc:silent
// 1. Common on-levelled threshold: the largest per-year on-levelled threshold
val uStar = years.map(y => tivIndex.suggestedNewThreshold(u, y)).max

// 2. On-level, keep losses above uStar
val onLevelledByYear = years
  .zip(rawLossesByYear)
  .map((y, xs) => xs.map(x => tivIndex.onLevel(value = x, fromYear = y)).filter(_ > uStar))
val counts = onLevelledByYear.map(_.length)
val ys = onLevelledByYear.flatten

// 3. Severity: Pareto tail index above uStar
val alpha = ys.length / ys.map(y => math.log(y / uStar)).sum

// 4. Frequency above uStar
val (freqAboveUStar, converged) = NegativeBinomial.mle(counts)
require(converged)

// 5. Forecast year above d
val d = 25.0e6
require(d >= uStar, "d below uStar needs extrapolation of the tail below where it was fitted")
val thin = math.pow(uStar / d, alpha)
val freqAboveD: Poisson | NegativeBinomial = freqAboveUStar match
  case nb: NegativeBinomial => NegativeBinomial(nb.a, nb.b * thin)
  case p: Poisson           => Poisson(p.lambda * thin)
val sevAboveD = Pareto(d, alpha)
```

The results:

```scala mdoc
years.map(y => f"$y: ${tivIndex.onLevelFactor(y)}%.3f").mkString(", ") // on-level factor per year
uStar
counts
alpha
freqAboveUStar
freqAboveD
sevAboveD
```

### Checks before relying on it

- **How many events survive u\*?** u\* is set by the lowest-TIV year you keep. Dropping the earliest years lowers u\* and keeps more events, so you trade years for events. Aim for several events in a typical year. Below that, β is poorly determined and the fit often returns Poisson.
- **Does the Pareto tail hold above u\*?** Step 5 relies on it. Check with a [Hill or Pickands plot](plots.md#tails) over the on-levelled losses above u\*.
- **Is d ≥ u\*?** A contract threshold below u\* needs the tail extrapolated below where it was fitted.
- **Are the years stationary?** Years are assumed to be independent draws from one Gamma(r, β). A trend left in the counts, from reporting or event-definition changes, is read as year-to-year variability and inflates β. [`plotTrend`](plots.md#trend-in-counts) shows it. Restrict to a stable period, or detrend first.
- **Is exceedance independent of activity?** Step 5 assumes whether an event clears d is independent of how active the season is. If active seasons also bring more severe events, counts above d are more dispersed than the thinned NB implies. A cheap check is to refit at a second, higher threshold: r should be roughly stable.
- **Has the spatial footprint changed?** If it has, counts above u\* also need a volume for exposure growth. Use `NegativeBinomial.volweightedMle(counts, volumes)` with v<sub>i</sub> = location or risk-count ratio, never the TIV ratio, because value growth is already in the on-levelling.

### Parameter uncertainty

The recipe gives a plug-in forecast. With 10–30 years of history, uncertainty in α̂, r and β is a material part of the forecast-year spread, especially in the tail. A simple way to include it is to bootstrap whole years and refit every step:

```scala mdoc:silent
val resampler = new scala.util.Random(1)
val nYears = years.length

def resampledForecastCount(): Option[Int] =
  val years = Array.fill(nYears)(resampler.nextInt(nYears))
  val c = years.map(onLevelledByYear(_).length)
  if c.sum == 0 then None // no events above uStar in this resample
  else
    val ysB = years.flatMap(onLevelledByYear(_))
    val a = ysB.length / ysB.map(y => math.log(y / uStar)).sum
    val (f, _) = NegativeBinomial.mle(c)
    val p = math.pow(uStar / d, a)
    Some(f match
      case nb: NegativeBinomial => NegativeBinomial(nb.a, nb.b * p).draw
      case pois: Poisson        => Poisson(pois.lambda * p).draw
    )
  end if
end resampledForecastCount

val forecastCounts = Iterator.continually(resampledForecastCount()).flatten.take(5_000).toArray
```

`forecastCounts` is a sample from the forecast-year count distribution above d, including parameter uncertainty. Comparing its upper quantiles with the plug-in `freqAboveD` shows how much the plug-in understates:

```scala mdoc:silent
def quantile(xs: Array[Int], q: Double): Int = xs.sorted.apply((q * (xs.length - 1)).round.toInt)

val plugIn = Array.fill(5_000)(freqAboveD match
  case nb: NegativeBinomial => nb.draw
  case p: Poisson           => p.draw
)
```

```scala mdoc
for q <- Seq(0.5, 0.9, 0.99) yield (q, quantile(plugIn, q), quantile(forecastCounts, q))
```

Each tuple is (quantile, plug-in, with parameter uncertainty). Both columns come from random draws, so they move slightly between builds.

## Pitfalls, in both directions

There is one correct basis: on-levelled losses above a threshold at which every year is complete. Errors come from adjusting too little, which keeps old years on the raw basis, or from adjusting too much or inconsistently. In the table, "old years" means the low-TIV years.

| Mistake | Frequency | Severity |
|---|---|---|
| Raw counts above u | understated; spurious upward trend, so β is inflated | — |
| On-levelled counts above u, or any threshold below u\* | understated, less so than raw | α̂ too low (tail too heavy) |
| Raw counts with the TIV ratio as volume | λ too low if α > 1, too high if α < 1 | — |
| On-levelled counts above u\* **and** a TIV-ratio volume | λ overstated (adjusted twice) | — |
| Frequency on one basis, severity on another | understated | too heavy |

### Too little adjustment: staying on the raw basis

- **Raw counts above u.** A low-TIV year recorded only its larger events. Events that would clear u at today's TIV were below u in that year's money, and are missing. Old years look quieter than they would be today. The fitted rate is too low for the forecast year. The growth in counts across the history looks like a trend, and the NB fit reads it as year-to-year variability, inflating β.
- **"Fixing" raw counts with the TIV ratio as volume.** The fraction of events clearing a fixed raw threshold scales as (TIV<sub>i</sub>/TIV<sub>model</sub>)<sup>α</sup>, not linearly. The plain TIV ratio is right only when α = 1. With α > 1 it overstates old years' exposure, so λ comes out too low. With α < 1 it comes out too high.

### Too much, or mismatched, adjustment

- **On-levelling, but keeping the raw threshold u.** In today's money, old years are incomplete between u and u<sub>i</sub>: events in that band were below u in their own year and never recorded. Counts above u are still understated for old years, though less than raw counts. A severity fitted to the pooled on-levelled losses against u is too heavy. Each old-year loss was really observed above u<sub>i</sub>, not u, so every log(y/u) is overstated by log(TIV<sub>model</sub>/TIV<sub>i</sub>) and α̂ is biased low.
- **Choosing a threshold below u\*,** for example one that suits the median year. This gives the same failure as above for every year whose u<sub>i</sub> exceeds the threshold you chose.
- **Adjusting twice.** Counts above u\* are already complete in every year. Adding a TIV-ratio volume on top treats old years as having less exposure than they had, so λ for the forecast year is overstated. Value growth belongs in exactly one place, and here it is the on-levelling.
- **Fitting frequency and severity on different bases.** For example, frequency from raw counts and severity from on-levelled losses. The frequency misses events that the severity sample implies exist. Frequency is understated, severity is too heavy, and the split between them is wrong even if expected loss happens to come out close.

### Threshold placement

- **u\* too low:** the early years are incomplete, which is the "too much adjustment" failure above.
- **u\* too high,** forced up by very old, very low-TIV years: few events survive, β is poorly determined, and parameter uncertainty dominates. The remedy is to drop the earliest years, not to lower u\* below what the remaining years support.

### When TIV is irrelevant to the event count

For perils whose event count does not depend on the book, such as hurricane landfall counts, TIV plays no part in frequency at all. Count the events directly. Only the severity is on-levelled.
