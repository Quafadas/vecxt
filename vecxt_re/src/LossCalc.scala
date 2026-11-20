package vecxt_re

enum LossCalc:
  case Agg, Occ
end LossCalc

/** What `lossReport` divides EL and std by.
  *
  *   - `FirstLimit` / `AggLimit`: the layer's limit. Ceded amounts already include the share, so they are divided by
  *     `limit * share` (see [[divisor]]). The resulting ratios are on a 100% basis and do not depend on share.
  *   - `Custom(d)`: divided by exactly `d`, with no share adjustment, e.g. `Custom(1.0)` for raw ceded EL / std.
  */
enum ReportDenominator:
  case FirstLimit
  case AggLimit
  case Custom(denominator: Double)

  /** The denominator as reported in the `limit` column: the layer's full (100%) limit, or the custom value. */
  def fromlayer(layer: Layer): Double =
    this match
      case FirstLimit          => layer.firstLimit
      case AggLimit            => layer.aggLimit.getOrElse(Double.PositiveInfinity)
      case Custom(denominator) => denominator

  /** What ceded (post-share) EL and std are divided by. For the limit-based denominators this is `limit * share`, so
    * the ratio is share-independent. A share of 0 gives NaN ratios, as there is no 100% basis to recover.
    */
  def divisor(layer: Layer): Double =
    this match
      case FirstLimit | AggLimit => fromlayer(layer) * layer.share
      case Custom(denominator)   => denominator

end ReportDenominator
