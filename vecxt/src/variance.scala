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
