package vecxt_re

/** The unit a price or rate is quoted in. `unit` is the value that represents par (100%) in that unit. */
enum PriceUnit(val unit: Int):
  case Bps extends PriceUnit(10000)
  case Pts extends PriceUnit(100)
  case One extends PriceUnit(1)

  /** Converts `value`, expressed in this unit, into the unit `to`. e.g. `Pts.convert(102, One) == 1.02` */
  def convert[A: Fractional](value: A, to: PriceUnit): A =
    import scala.math.Fractional.Implicits.*
    value * Fractional[A].fromInt(to.unit) / Fractional[A].fromInt(unit)
  end convert
end PriceUnit