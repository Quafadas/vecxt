package vecxt.fusion

/** The element dtype of a tensor value. */
enum DType:
  case F64, F32, I64, I32, I8, Bool

  override def toString: String = this match
    case F64  => "f64"
    case F32  => "f32"
    case I64  => "i64"
    case I32  => "i32"
    case I8   => "i8"
    case Bool => "bool"
end DType

/** A single axis dimension: concrete, symbolic, or unknown at trace time. */
sealed trait Dim
object Dim:
  /** A statically known, concrete axis size. Must be non-negative. */
  case class Known(n: Int) extends Dim:
    require(n >= 0, s"Known dimension must be non-negative, got $n")
    override def toString: String = n.toString
  end Known

  /** A symbolic axis size (e.g. "batch"). */
  case class Sym(name: String) extends Dim:
    override def toString: String = name
  end Sym

  /** An axis size that is not known at compile / trace time. */
  case object Unknown extends Dim:
    override def toString: String = "?"
  end Unknown
end Dim

/** The shape of a tensor: an ordered sequence of `Dim`s.
  *
  * Structural equality and hashing are defined over the *content* of `dims`, not its reference identity, so two `Shape`
  * values with the same dims compare equal.
  */
final class Shape(val dims: Array[Dim]):
  def rank: Int = dims.length
  def isScalar: Boolean = dims.isEmpty

  override def equals(other: Any): Boolean = other match
    case that: Shape => dims.sameElements(that.dims)
    case _           => false

  override def hashCode: Int =
    dims.foldLeft(1)((h, d) => 31 * h + d.hashCode)

  override def toString: String =
    dims.mkString("[", "×", "]")
end Shape

object Shape:
  /** Build a shape from explicit dims. */
  def apply(dims: Dim*): Shape = new Shape(dims.toArray)

  /** The scalar (rank-0) shape. */
  val scalar: Shape = new Shape(Array.empty)
end Shape

/** The complete type of a tensor value: element dtype plus shape. */
final case class TType(dtype: DType, shape: Shape):
  override def toString: String = s"$dtype$shape"
end TType
