package vecxt.dimensionExtender

object DimensionExtender:

  type DimensionExtender = Int | Dimension

  extension (d: DimensionExtender)
    def asInt: Int = d match
      case d: Dimension => d.dim
      case i: Int       => i
  end extension

  enum Dimension(val dim: Int):
    case Rows extends Dimension(0)
    case Cols extends Dimension(1)
    case X extends Dimension(0)
    case Y extends Dimension(1)

  end Dimension

end DimensionExtender
