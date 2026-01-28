package vecxt_re

import org.apache.commons.rng.simple.RandomSource

import io.circe.syntax.*
import io.github.quafadas.plots.SetupVega.{*, given}

/** Empirical distribution (JVM only).
  *
  * This is a nonparametric distribution built directly from observed samples. It supports positive weights $w_i$.
  *
  * The distribution is represented as a discrete measure on the (possibly repeated) sample values: $$\mathbb{P}(X = x) =
  * \sum_{i: x_i = x} \frac{w_i}{\sum_k w_k}.$$
  *
  * Consequently, the CDF is a right-continuous step function $$F(t) = \mathbb{P}(X \le t) = \sum_{x \le t}
  * \mathbb{P}(X=x).$$
  *
  * Sampling is performed by inverse-transform sampling on the cumulative weights.
  *
  * Note: Since this is an empirical (atomic) distribution, a density/PDF in the usual continuous sense is not defined.
  * The `plot` method instead displays a (weighted) histogram density estimate.
  */
case class Empirical(values: IArray[Double], weights: IArray[Double])
    extends DiscreteDistr[Double]
    with HasMean[Double]
    with HasVariance[Double]
    with HasCdf
    with HasInverseCdf:

  require(values.nonEmpty, "values must not be empty")
  require(values.forall(_.isFinite), "all values must be finite")
  require(weights.length == values.length, "weights must be the same length as values")
  require(weights.forall(w => w > 0 && w.isFinite), "weights must be positive and finite")

  private val rng = RandomSource.XO_RO_SHI_RO_128_PP.create()

  private val n = values.length

  private val pairs: Array[(Double, Double)] =
    val out = new Array[(Double, Double)](n)
    var j = 0
    while j < n do
      out(j) = (values(j), weights(j))
      j += 1
    end while
    out
  end pairs

  scala.util.Sorting.stableSort(pairs, (a: (Double, Double), b: (Double, Double)) => a._1 < b._1)

  // Compress duplicates so we have unique support points.
  private val xsBuf = scala.collection.mutable.ArrayBuffer.empty[Double]
  private val wBuf = scala.collection.mutable.ArrayBuffer.empty[Double]

  private var totalWeight = 0.0
  private var i = 0
  while i < pairs.length do
    val x = pairs(i)._1
    var wSum = 0.0
    while i < pairs.length && pairs(i)._1 == x do
      val w = pairs(i)._2
      wSum += w
      totalWeight += w
      i += 1
    end while
    xsBuf += x
    wBuf += wSum
  end while

  private val xs: Array[Double] = xsBuf.toArray
  private val probs: Array[Double] = wBuf.toArray.map(_ / totalWeight)

  private val cdfVals: Array[Double] =
    val out = new Array[Double](probs.length)
    var acc = 0.0
    var j = 0
    while j < probs.length do
      acc += probs(j)
      out(j) = acc
      j += 1
    end while
    out
  end cdfVals

  private val meanVal: Double =
    var s = 0.0
    var j = 0
    while j < xs.length do
      s += xs(j) * probs(j)
      j += 1
    end while
    s
  end meanVal

  private val varVal: Double =
    var s2 = 0.0
    var j = 0
    while j < xs.length do
      val d = xs(j) - meanVal
      s2 += probs(j) * d * d
      j += 1
    end while
    s2
  end varVal

  def mean: Double = meanVal

  def variance: Double = varVal

  /** Probability mass at exactly `x` (sums weights for duplicates). */
  def probabilityOf(x: Double): Double =
    val idx = java.util.Arrays.binarySearch(xs, x)
    if idx >= 0 then probs(idx) else 0.0
    end if
  end probabilityOf

  /** Draw a sample using inverse CDF sampling over the atomic masses. */
  def draw: Double =
    val u = rng.nextDouble()
    inverseCdf(u)
  end draw

  /** CDF $F(t)=P(X\le t)$ (right-continuous). */
  def cdf(x: Double): Double =
    if x < xs(0) then 0.0
    else if x >= xs(xs.length - 1) then 1.0
    else
      // Find the last index with xs(idx) <= x
      val ip = java.util.Arrays.binarySearch(xs, x)
      val idx = if ip >= 0 then ip else -ip - 2
      cdfVals(idx)

  /** Probability that $x < X \le y$. */
  def probability(x: Double, y: Double): Double =
    if y <= x then 0.0
    else cdf(y) - cdf(x)

  /** Inverse CDF (quantile function): returns the smallest `x` with `F(x) >= p`. */
  def inverseCdf(p: Double): Double =
    require(p >= 0.0 && p <= 1.0, "p must be in [0,1]")
    if p <= 0.0 then xs(0)
    else
      val ip = java.util.Arrays.binarySearch(cdfVals, p)
      val idx = if ip >= 0 then ip else -ip - 1
      xs(math.min(idx, xs.length - 1))
    end if
  end inverseCdf

  /** Plot a (weighted) histogram density estimate. */
  def plot(using viz.LowPriorityPlotTarget) =
    val plot = VegaPlot.fromResource("empiricalPdf.vl.json")
    val data = (0 until n).map(i => (x = values(i), w = weights(i)))
    plot.plot(
      _.data.values := data.asJson,
      _ += (title = s"Empirical Distribution (n=$n)").asJson
    )
  end plot

  /** Plot the empirical CDF (step function). */
  def plotCdf(using viz.LowPriorityPlotTarget) =
    val plot = VegaPlot.fromResource("empiricalCdf.vl.json")

    // Add an initial point at (min, 0) so the step is visible from the left.
    val points =
      val pts = scala.collection.mutable.ArrayBuffer.empty[(x: Double, cdf: Double)]
      pts += ((x = xs(0), cdf = 0.0))
      var j = 0
      while j < xs.length do
        pts += ((x = xs(j), cdf = cdfVals(j)))
        j += 1
      end while
      pts.toVector
    end points

    plot.plot(
      _.data.values := points.asJson,
      _ += (title = s"Empirical CDF (n=$n)").asJson
    )
  end plotCdf

end Empirical

object Empirical:
  /** Construct an unweighted empirical distribution (all weights equal to $1$).
    *
    * Note: We intentionally avoid `apply` overloads here because `IArray[Double]` erases to `Array[Double]` on the JVM,
    * which can create signature collisions with the case class companion methods.
    */
  inline def equalWeights(values: Array[Double]): Empirical =
    Empirical(
      IArray.from(values),
      IArray.from(Array.fill(values.length)(1.0))
    )

  /** Construct a weighted empirical distribution from arrays. */
  inline def weighted(values: Array[Double], weights: Array[Double]): Empirical =
    Empirical(IArray.from(values), IArray.from(weights))
end Empirical
