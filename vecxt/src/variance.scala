package vecxt

enum VarianceMode:
  case Population
  case Sample
end VarianceMode

object VarianceMode:
  inline def denominator(length: Int, mode: VarianceMode): Double =
    mode match
      case VarianceMode.Population => length.toDouble
      case VarianceMode.Sample     => (length - 1).toDouble
end VarianceMode

/** The result of a mean-and-variance pass over `Array[Double]` or `Array[Int]`.
  *
  * Replaces the named tuple `(mean: Double, variance: Double)`, and the reason is measured rather than assumed
  * (vecxt/issues/105):
  *
  *   - A named tuple erases to `scala.Tuple2`, so reading one field costs an unbox. Check C3 measured
  *     `variance(mode)` — whose whole body is `meanAndVariance(mode).variance` — at 37 bytes against a 35-byte
  *     `MaxInlineSize`, roughly 30 of them being the destructuring.
  *   - Check D1 measured 59.33 bytes/op allocated when a caller reads the pair. Notably it measured **zero** for
  *     `variance(mode)`, where the pair is dead and escape analysis scalarizes it — so the allocation is real only for
  *     callers who actually want both numbers, which is every caller of `meanAndVariance`.
  *
  * Two primitive `double` fields is one flat object with nothing nested for escape analysis to chase, and a field read
  * is an `invokevirtual` on a final class rather than an unbox.
  *
  * `final class`, not `case class`, for the reason recorded on [[vecxt.matrix.Layout]]: `case class` synthesises
  * `productElement: Int => Object`, which boxes.
  *
  * No `unapply`, deliberately. `val (m, v) = arr.meanAndVariance` was the old idiom and is now `.mean` / `.variance`;
  * `val (m, v) = x` is a `Tuple2` pattern and cannot match a non-tuple whatever the companion offers, so restoring
  * destructuring would mean `case MeanAndVariance(m, v) =>` at every site — new API surface to buy back a spelling.
  */
final class MeanAndVariance(val mean: Double, val variance: Double):
  override def toString: String = s"mean: $mean, variance: $variance"

  // `java.lang.Double.compare` rather than `==`: an empty or single-element input gives a NaN variance, and
  // `NaN == NaN` is false, so `==` would make such a result unequal to itself.
  override def equals(that: Any): Boolean = that match
    case o: MeanAndVariance =>
      java.lang.Double.compare(mean, o.mean) == 0 && java.lang.Double.compare(variance, o.variance) == 0
    case _ => false

  override def hashCode: Int = 31 * mean.hashCode() + variance.hashCode()
end MeanAndVariance

/** The `Array[Float]` counterpart of [[MeanAndVariance]], carrying two primitive `float` fields.
  *
  * A separate class rather than one generic in the element type: `MeanAndVariance[A]` would erase `A` to `Object` and
  * box both fields, which is the cost being removed. The float kernels accumulate in `Double` and narrow once at the
  * end, so this describes the result, not the arithmetic.
  */
final class MeanAndVarianceF(val mean: Float, val variance: Float):
  override def toString: String = s"mean: $mean, variance: $variance"

  override def equals(that: Any): Boolean = that match
    case o: MeanAndVarianceF =>
      java.lang.Float.compare(mean, o.mean) == 0 && java.lang.Float.compare(variance, o.variance) == 0
    case _ => false

  override def hashCode: Int = 31 * mean.hashCode() + variance.hashCode()
end MeanAndVarianceF
