package vecxt_re

/**
 * A trait for monadic distributions. Provides support for use in for-comprehensions
 */
trait Rand[T] { outer =>

  /**
   * Gets one sample from the distribution. Equivalent to sample
   */
  def draw: T

  inline def get = draw

  /** Overridden by filter/map/flatmap for monadic invocations. Basically, rejection samplers will return None here */
  def drawOpt: Option[T] = Some(draw)

  /**
   * Gets one sample from the distribution. Equivalent to get
   */
  inline def sample = get

  /**
   * Gets n samples from the distribution.
   */
  inline def sample(n: Int): IndexedSeq[T] = IndexedSeq.fill(n)(draw)

  /**
   * Gets n samples from the distribution into a specified collection type.
   */
  inline def sampleTo[C](n: Int)(using factory: scala.collection.Factory[T, C]): C = {
    val builder = factory.newBuilder
    builder.sizeHint(n)
    var i = 0
    while (i < n) {
      builder += draw
      i += 1
    }
    builder.result()
  }

  /**
   * An infinitely long iterator that samples repeatedly from the Rand
   * @return an iterator that repeatedly samples
   */
  inline def samples: Iterator[T] = Iterator.continually(draw)

  /**
   * Converts a random sampler of one type to a random sampler of another type.
   * Examples:
   * uniform.map(_*2) gives a Rand[Double] in the range [0,2]
   * Equivalently, for(x <- uniform) yield 2*x
   *
   * @param f the transform to apply to the sampled value.
   *
   */
  def map[E](f: T => E): Rand[E] = MappedRand(outer, f)

  def flatMap[E](f: T => Rand[E]): Rand[E] = FlatMappedRand(outer, f)

  def withFilter(p: T => Boolean): Rand[T] = FilteredRand(outer, p)

}

private final case class MappedRand[@specialized(Int, Double) T, @specialized(Int, Double) U](
    rand: Rand[T],
    func: T => U)
    extends Rand[U] {
  override def draw: U = func(rand.draw)
  override def drawOpt: Option[U] = rand.drawOpt.map(func)
  override def map[E](f: U => E): Rand[E] = MappedRand(rand, (x: T) => f(func(x)))
}

private final case class FlatMappedRand[@specialized(Int, Double) T, @specialized(Int, Double) U](
    rand: Rand[T],
    func: T => Rand[U])
    extends Rand[U] {
  override def draw: U = func(rand.draw).draw
  override def drawOpt: Option[U] = rand.drawOpt.flatMap(x => func(x).drawOpt)
  override def flatMap[E](f: U => Rand[E]): Rand[E] = FlatMappedRand(rand, (x: T) => func(x).flatMap(f))
}

private final case class FilteredRand[@specialized(Int, Double) T](
    rand: Rand[T],
    predicate: T => Boolean)
    extends Rand[T] {
  override def draw: T = {
    var result = rand.draw
    var attempts = 0
    while (!predicate(result)) {
      attempts += 1
      if (attempts > 100000) throw new RuntimeException("Rejection sampling exceeded max attempts")
      result = rand.draw
    }
    result
  }
  override def drawOpt: Option[T] = rand.drawOpt.filter(predicate)
}