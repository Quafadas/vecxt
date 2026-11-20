package vecxt_re

import java.time.LocalDate

import munit.FunSuite
import io.circe.syntax.*
import io.circe.parser.decode

import vecxt_re.SplitScenario.*

class FreqSevTest extends FunSuite:

  private val severity =
    Mixed.fromWeightedValues(Array(1.0, 2.0, 3.0, 5.0, 10.0), Array(1.0, 2.0, 1.0, 0.5, 0.5), 4.0, 2.5)

  private def assertSameParams(back: FreqSev, orig: FreqSev)(using munit.Location): Unit =
    assertEquals(back.frequency, orig.frequency)
    assertEquals(back.severity.empirical.values.toSeq, orig.severity.empirical.values.toSeq)
    assertEquals(back.severity.empirical.weights.toSeq, orig.severity.empirical.weights.toSeq)
    assertEquals(back.severity.mixingPoint, orig.severity.mixingPoint)
    assertEquals(back.severity.paretoShape, orig.severity.paretoShape)
  end assertSameParams

  test("JSON round-trip with Poisson frequency") {
    val fs = FreqSev(Poisson(3.0), severity)
    val json = fs.asJson
    assertEquals(json.hcursor.downField("frequency").downField("type").as[String], Right("Poisson"))
    val back = decode[FreqSev](json.noSpaces).fold(throw _, identity)
    assert(back.frequency.isInstanceOf[Poisson])
    assertSameParams(back, fs)
  }

  test("JSON round-trip with NegativeBinomial frequency") {
    val fs = FreqSev(NegativeBinomial(2.0, 0.5), severity)
    val json = fs.asJson
    assertEquals(json.hcursor.downField("frequency").downField("type").as[String], Right("NegativeBinomial"))
    val back = decode[FreqSev](json.noSpaces).fold(throw _, identity)
    assert(back.frequency.isInstanceOf[NegativeBinomial])
    assertSameParams(back, fs)
  }

  test("JSON decoder rejects an unknown frequency type") {
    val json = FreqSev(Poisson(3.0), severity).asJson.hcursor
      .downField("frequency")
      .downField("type")
      .withFocus(_ => "Binomial".asJson)
      .top
      .get
    assert(decode[FreqSev](json.noSpaces).isLeft)
  }

  test("mean and variance follow the collective risk model") {
    val nb = NegativeBinomial(2.0, 0.5)
    val fs = FreqSev(nb, severity)
    assertEqualsDouble(fs.mean, nb.mean * severity.mean, 1e-12)
    val ex = severity.mean
    assertEqualsDouble(fs.variance, nb.mean * severity.variance + nb.variance * ex * ex, 1e-12)
  }

  test("drawAggregate sample mean is close to the analytic mean") {
    val fs = FreqSev(Poisson(5.0), Mixed.fromValues(Array(1.0, 2.0, 3.0, 4.0, 5.0), 10.0, 3.0))
    val n = 50000
    val sampleMean = Iterator.fill(n)(fs.drawAggregate).sum / n
    assertEqualsDouble(sampleMean, fs.mean, fs.mean * 0.05)
  }

  // --- drawScenarr ---

  private val simple = FreqSev(Poisson(5.0), Mixed.fromValues(Array(1.0, 2.0, 3.0, 4.0, 5.0), 10.0, 3.0))

  test("drawScenarr sets the Scenarr metadata") {
    val s = simple.drawScenarr(numberIterations = 100, day1 = LocalDate.of(2026, 1, 1), name = "property")
    assertEquals(s.numberIterations, 100)
    assertEquals(s.day1, LocalDate.of(2026, 1, 1))
    assertEquals(s.name, "property")
    assertEquals(s.threshold, 0d)
    assert(s.isSorted)
  }

  test("drawScenarr defaults day1 and name like Scenarr") {
    val s = simple.drawScenarr(10)
    assertEquals(s.day1, LocalDate.of(2019, 1, 1))
    assertEquals(s.name, "")
  }

  test("drawScenarr iterations are 1-based and days 0-based within the year") {
    val s = simple.drawScenarr(numberIterations = 500, day1 = LocalDate.of(2026, 1, 1))
    assert(s.iterations.nonEmpty)
    assert(s.iterations.forall(i => i >= 1 && i <= 500))
    assert(s.days.forall(d => d >= 0 && d < 365))
    // ~2,500 claims over 365 days: the earliest is all but certain to land in the first few days
    assert(s.days.min < 5, s"days should start at 0, earliest was ${s.days.min}")
  }

  test("drawScenarr covers 366 days in a leap year") {
    val s = simple.drawScenarr(numberIterations = 2000, day1 = LocalDate.of(2024, 1, 1))
    assert(s.days.forall(d => d >= 0 && d < 366))
    assert(s.days.max >= 365, "leap day should be reachable")
  }

  test("drawScenarr output is sorted by iteration then day") {
    val s = simple.drawScenarr(numberIterations = 200)
    val pairs = s.iterations.zip(s.days).toSeq
    assertEquals(pairs, pairs.sorted)
  }

  test("drawScenarr frequency and aggregate means match the model") {
    val n = 20000
    val s = simple.drawScenarr(n)
    assertEquals(s.freq.length, n)
    assertEqualsDouble(s.meanFreq, 5.0, 5.0 * 0.05)
    assertEqualsDouble(s.agg.sum / n, simple.mean, simple.mean * 0.05)
  }

  test("drawScenarr drops claims at or below the threshold") {
    val s = simple.drawScenarr(numberIterations = 1000, threshold = 3.0)
    assertEquals(s.threshold, 3.0)
    assert(s.amounts.forall(_ > 3.0))
  }

  test("drawScenarr rejects invalid arguments") {
    intercept[IllegalArgumentException](simple.drawScenarr(0))
    intercept[IllegalArgumentException](simple.drawScenarr(10, threshold = -1.0))
  }

  test("drawScenarr iteration column is the unfolded claim counts") {
    val n = 300
    val s = simple.drawScenarr(n)
    // contiguous blocks: iterations never decrease, and each block's length is that iteration's count
    assert(s.iterations.sliding(2).forall(w => w.length < 2 || w(0) <= w(1)))
    assertEquals(s.freq.sum, s.amounts.length)
    val countsFromColumn = s.iterations.groupBy(identity).view.mapValues(_.length).toMap
    for i <- 1 to n do assertEquals(s.freq(i - 1), countsFromColumn.getOrElse(i, 0), s"iteration $i")
    end for
  }

  test("drawScenarr with a threshold stays sorted") {
    val s = simple.drawScenarr(numberIterations = 500, threshold = 3.0)
    assert(s.isSorted)
    val pairs = s.iterations.zip(s.days).toSeq
    assertEquals(pairs, pairs.sorted)
  }

  test("drawScenarr feeds the reinsurance split") {
    val s = simple.drawScenarr(1000)
    val tower = Tower.fromRetention(5.0, Vector(5.0, 10.0))
    val result = tower.splitScenarioAmounts(s)
    assertEquals(result.ceded.length, s.amounts.length)
    assertEqualsDouble(result.ceded.sum + result.retained.sum, s.amounts.sum, 1e-6 * s.amounts.sum)
  }

end FreqSevTest
