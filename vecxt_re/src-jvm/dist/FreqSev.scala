package vecxt_re

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*

/** Collective risk model: a claim-count (frequency) distribution paired with a per-claim severity distribution.
  *
  * The aggregate loss is $S = \sum_{i=1}^{N} X_i$ with $N$ drawn from `frequency` and the $X_i$ iid from `severity`.
  *
  * Serialises to JSON with only the distribution parameters; the frequency carries a `"type"` discriminator so the
  * Poisson / Negative Binomial choice survives a round trip:
  * {{{
  * {
  *   "frequency": {"type": "NegativeBinomial", "a": 2.0, "b": 0.5},
  *   "severity": {"empirical": {"values": [...], "weights": [...]}, "mixingPoint": 1e6, "paretoShape": 1.8}
  * }
  * }}}
  *
  * @param frequency
  *   The claim-count distribution
  * @param severity
  *   The per-claim loss distribution
  */
case class FreqSev(frequency: Poisson | NegativeBinomial, severity: Mixed):

  /** Expected aggregate loss, $E[S] = E[N] \, E[X]$. */
  def mean: Double = frequency.mean * severity.mean

  /** Aggregate loss variance, $Var(S) = E[N] \, Var(X) + Var(N) \, E[X]^2$. Infinite when the severity variance is. */
  def variance: Double =
    val ex = severity.mean
    frequency.mean * severity.variance + frequency.variance * ex * ex
  end variance

  /** Simulate the individual losses of a single period. */
  def draw: Array[Double] =
    val n = frequency.draw
    Array.fill(n)(severity.draw)
  end draw

  /** Simulate the aggregate loss of a single period. */
  def drawAggregate: Double =
    val n = frequency.draw
    var total = 0.0
    var i = 0
    while i < n do
      total += severity.draw
      i += 1
    end while
    total
  end drawAggregate

  /** Simulate `numberIterations` one-year periods as a [[Scenarr]], ready for the reinsurance calculations (e.g.
    * `tower.splitScenarioAmounts`).
    *
    *   1. For each iteration, draw the number of claims from `frequency`.
    *   1. For each claim, draw a severity.
    *   1. For each claim, draw a day of occurrence, uniform over the year starting at `day1` (0-based: day 0 is
    *      `day1`).
    *   1. Unfold the claim counts into a contiguous iteration column (1-based), e.g. counts `[2, 0, 1]` become
    *      iterations `[1, 1, 3]`.
    *
    * Days are sorted within each iteration so the result is sorted by iteration then day. Claims at or below
    * `threshold` are then dropped, matching `Scenarr.applyThreshold`.
    *
    * {{{
    * val scenarr = freqSev.drawScenarr(numberIterations = 10000, day1 = LocalDate.of(2026, 1, 1), name = "property")
    * }}}
    *
    * @param numberIterations
    *   Number of simulated years
    * @param day1
    *   The first day of the simulated year
    * @param name
    *   Name of the resulting Scenarr
    * @param threshold
    *   Reporting threshold; only claims strictly above it are kept
    */
  def drawScenarr(
      numberIterations: Int,
      day1: LocalDate = LocalDate.of(2019, 1, 1),
      name: String = "",
      threshold: Double = 0d
  ): Scenarr =
    require(numberIterations > 0, s"numberIterations must be > 0, got $numberIterations")
    require(threshold >= 0d, s"threshold must be >= 0, got $threshold")
    val daysInYear = ChronoUnit.DAYS.between(day1, day1.plusYears(1)).toInt
    val rng = scala.util.Random()

    // 1. number of claims per iteration
    val claimCounts = Array.fill(numberIterations)(frequency.draw)
    val numClaims = claimCounts.sum

    // 2. a severity per claim
    val amounts = Array.fill(numClaims)(severity.draw)

    // 3. a uniform day per claim
    val days = Array.fill(numClaims)(rng.nextInt(daysInYear))

    // 4. unfold the counts into a contiguous, 1-based iteration column; sort days within each iteration
    val iterations = new Array[Int](numClaims)
    var start = 0
    var itr = 0
    while itr < numberIterations do
      val end = start + claimCounts(itr)
      java.util.Arrays.fill(iterations, start, end, itr + 1)
      java.util.Arrays.sort(days, start, end)
      start = end
      itr += 1
    end while

    Scenarr.withGeneratedIds(
      java.util.Arrays.copyOf(iterations, kept),
      java.util.Arrays.copyOf(days, kept),
      java.util.Arrays.copyOf(amounts, kept),
      numberIterations,
      threshold = threshold,
      day1 = day1,
      name = name,
      isSorted = true
    )
  end drawScenarr

end FreqSev

object FreqSev:

  private val PoissonTag = "Poisson"
  private val NegativeBinomialTag = "NegativeBinomial"

  /** Frequency codec: the distribution's own JSON plus a `"type"` discriminator. */
  given frequencyEncoder: Encoder[Poisson | NegativeBinomial] = Encoder.instance {
    case p: Poisson           => p.asJson.deepMerge(Json.obj("type" -> PoissonTag.asJson))
    case nb: NegativeBinomial => nb.asJson.deepMerge(Json.obj("type" -> NegativeBinomialTag.asJson))
  }

  given frequencyDecoder: Decoder[Poisson | NegativeBinomial] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case PoissonTag          => c.as[Poisson]
      case NegativeBinomialTag => c.as[NegativeBinomial]
      case other               =>
        Left(
          DecodingFailure(
            s"unknown frequency type '$other', expected '$PoissonTag' or '$NegativeBinomialTag'",
            c.history
          )
        )
    }
  }

  given freqSevEncoder: Encoder[FreqSev] =
    Encoder.forProduct2[FreqSev, Poisson | NegativeBinomial, Mixed]("frequency", "severity")(fs =>
      (fs.frequency, fs.severity)
    )

  given freqSevDecoder: Decoder[FreqSev] =
    Decoder.forProduct2[FreqSev, Poisson | NegativeBinomial, Mixed]("frequency", "severity")(FreqSev.apply)

end FreqSev
