package vecxt_re

trait Density[T]:

  /** Returns the unnormalized value of the measure */
  def apply(x: T): Double

  /** Returns the log unnormalized value of the measure */
  def logApply(x: T): Double = math.log(apply(x))
end Density

/** Represents a continuous Distribution.
  */
trait ContinuousDistr[T] extends Density[T] with Rand[T]:

  /** Returns the probability density function at that point. */
  def pdf(x: T): Double = math.exp(logPdf(x))
  def logPdf(x: T): Double = unnormalizedLogPdf(x) - logNormalizer

  /** Returns the probability density function up to a constant at that point. */
  def unnormalizedPdf(x: T): Double = math.exp(unnormalizedLogPdf(x))

  def unnormalizedLogPdf(x: T): Double
  def logNormalizer: Double

  // 1/Z where Z = exp(logNormalizer)
  lazy val normalizer: Double = math.exp(-logNormalizer)

  def apply(x: T) = unnormalizedPdf(x)
  override def logApply(x: T) = unnormalizedLogPdf(x)
end ContinuousDistr

trait HasCdf:
  def probability(x: Double, y: Double): Double // Probability that P(x < X <= y)
  def cdf(x: Double): Double

  // experimental plotting support
  def plot(using viz.LowPriorityPlotTarget): viz.VizReturn
  def plotCdf(using viz.LowPriorityPlotTarget): viz.VizReturn
end HasCdf

trait HasInverseCdf:
  def inverseCdf(p: Double): Double // Compute the quantile of p
end HasInverseCdf

/** Represents a discrete Distribution
  */
trait DiscreteDistr[T] extends Density[T] with Rand[T]:

  /** Returns the probability of that draw. */
  def probabilityOf(x: T): Double
  def logProbabilityOf(x: T): Double = math.log(probabilityOf(x))

  /** Returns the probability of that draw up to a constant */
  def unnormalizedProbabilityOf(x: T): Double = probabilityOf(x)
  def unnormalizedLogProbabilityOf(x: T): Double = math.log(unnormalizedProbabilityOf(x))

  def apply(x: T) = unnormalizedProbabilityOf(x)
  override def logApply(x: T) = unnormalizedLogProbabilityOf(x)
end DiscreteDistr

trait HasMean[T]:
  def mean: T
end HasMean

trait HasVariance[T]:
  def variance: T
end HasVariance
