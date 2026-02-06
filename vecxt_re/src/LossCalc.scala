package vecxt_re

enum LossCalc:
  case Agg, Occ
end LossCalc

enum ReportDenominator:
  case FirstLimit
  case AggLimit
  case Custom(denominator: Double)
  def fromlayer(layer: Layer) =
    this match
      case FirstLimit          => layer.firstLimit
      case AggLimit            => layer.aggLimit.getOrElse(Double.PositiveInfinity)
      case Custom(denominator) => denominator

end ReportDenominator
