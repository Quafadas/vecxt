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

/** Error produced by `Shape.broadcast` when two shapes are incompatible. */
final case class ShapeError(message: String)

object Shape:
  /** Build a shape from explicit dims. */
  def apply(dims: Dim*): Shape = new Shape(dims.toArray)

  /** The scalar (rank-0) shape. */
  val scalar: Shape = new Shape(Array.empty)

  /** Compute the NumPy-style broadcast output shape of `a` and `b`.
    *
    * Both shapes are right-aligned and padded with `Known(1)` on the left. For each aligned pair of dims:
    *   - `Known(n)` + `Known(n)` → `Known(n)` (equal)
    *   - `Known(1)` + `Known(m)` → `Known(m)` (and vice-versa)
    *   - `Known(n)` + `Known(m)` where n ≠ m, neither 1 → `ShapeError`
    *   - `Sym(s)` + `Sym(s)` → `Sym(s)` (same name unified)
    *   - `Sym(s)` + `Known(1)` → `Sym(s)` (and vice-versa)
    *   - `Sym(a)` + `Sym(b)` where a ≠ b → `Unknown`
    *   - `Sym(s)` + `Known(n)` where n ≠ 1 → `Unknown`
    *   - `Unknown` + anything → `Unknown`
    */
  def broadcast(a: Shape, b: Shape): Either[ShapeError, Shape] =
    val n = math.max(a.rank, b.rank)
    val aDims = padLeft(a.dims, n)
    val bDims = padLeft(b.dims, n)
    val result = new Array[Dim](n)
    var i = 0
    var err: ShapeError = null
    while i < n && err == null do
      broadcastDim(aDims(i), bDims(i)) match
        case Right(d) => result(i) = d
        case Left(e)  => err = e
      end match
      i += 1
    end while
    if err != null then Left(err)
    else Right(new Shape(result))
    end if
  end broadcast

  private def padLeft(dims: Array[Dim], n: Int): Array[Dim] =
    if dims.length == n then dims
    else Array.fill(n - dims.length)(Dim.Known(1)) ++ dims

  private def broadcastDim(a: Dim, b: Dim): Either[ShapeError, Dim] = (a, b) match
    case (Dim.Unknown, _) | (_, Dim.Unknown)                     => Right(Dim.Unknown)
    case (Dim.Known(n), Dim.Known(m)) if n == m                  => Right(Dim.Known(n))
    case (Dim.Known(1), Dim.Known(m))                            => Right(Dim.Known(m))
    case (Dim.Known(n), Dim.Known(1))                            => Right(Dim.Known(n))
    case (Dim.Known(n), Dim.Known(m))                            => Left(ShapeError(s"dimension mismatch: $n vs $m"))
    case (Dim.Sym(s), Dim.Sym(t)) if s == t                      => Right(Dim.Sym(s))
    case (Dim.Sym(_), Dim.Sym(_))                                => Right(Dim.Unknown)
    case (Dim.Sym(s), Dim.Known(1))                              => Right(Dim.Sym(s))
    case (Dim.Known(1), Dim.Sym(s))                              => Right(Dim.Sym(s))
    case (Dim.Sym(_), Dim.Known(_)) | (Dim.Known(_), Dim.Sym(_)) => Right(Dim.Unknown)
end Shape

/** The complete type of a tensor value: element dtype plus shape. */
final case class TType(dtype: DType, shape: Shape):
  override def toString: String = s"$dtype$shape"
end TType
