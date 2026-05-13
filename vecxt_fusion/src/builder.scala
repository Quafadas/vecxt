package vecxt.fusion

import scala.collection.mutable
import vecxt.ndarray.NDArray

/** Type class that maps a Scala type `A` to its corresponding `DType`.
  *
  * Instances are provided for `Double` → `F64`, `Float` → `F32`, `Int` → `I32`, `Boolean` → `Bool`.
  */
trait DTypeOf[A]:
  def dtype: DType
end DTypeOf

object DTypeOf:
  given DTypeOf[Double] with
    def dtype: DType = DType.F64
  given DTypeOf[Float] with
    def dtype: DType = DType.F32
  given DTypeOf[Int] with
    def dtype: DType = DType.I32
  given DTypeOf[Boolean] with
    def dtype: DType = DType.Bool
end DTypeOf

/** Evidence that type `A` supports numeric operations: `+`, `-`, `*`, `/`, `unary_-`.
  *
  * Instances are provided for `Double`, `Float`, and `Int`. `Boolean` deliberately has no instance.
  */
sealed trait IsNumeric[A]

object IsNumeric:
  given IsNumeric[Double] = new IsNumeric[Double] {}
  given IsNumeric[Float] = new IsNumeric[Float] {}
  given IsNumeric[Int] = new IsNumeric[Int] {}
end IsNumeric

/** A mutable graph builder with hash-consing.
  *
  * Create leaf nodes with `Expr.param`, `Expr.const`, or `Expr.lift`, compose them using the extension methods on
  * `Expr[A]`, then call `build` to extract the final, immutable `TensorGraph`.
  *
  * Hash-consing guarantees that structurally identical nodes share a single `NodeId`. Two nodes are equal when their
  * constructor, child `NodeId`s, op, and `TType` are all equal. Floating-point `Const` values use
  * `java.lang.Double.equals` / `java.lang.Float.equals` bit-level equality (so `NaN == NaN`).
  *
  * `Lift` nodes are deliberately **not** hash-consed: two separate `Expr.lift` calls over the same array always
  * produce distinct nodes so that the graph correctly reflects the number of distinct source arrays.
  *
  * Usage:
  * {{{
  * val b = new GraphBuilder()
  * import b.*
  * val x = b.Expr.param[Double]("x")
  * val y = b.Expr.param[Double]("y")
  * val g = b.build(x + y)
  * }}}
  */
final class GraphBuilder:

  private val _nodes: mutable.ArrayBuffer[TensorExpr] = mutable.ArrayBuffer.empty
  private val _index: mutable.HashMap[TensorExpr, NodeId] = mutable.HashMap.empty
  private var _nextHandleId: Int = 0

  // ─── internal helpers ──────────────────────────────────────────────────────

  /** Return the `NodeId` for `node`, inserting it if it has not been seen before (hash-consing). */
  private def intern(node: TensorExpr): NodeId =
    _index.getOrElse(
      node, {
        val id = NodeId(_nodes.size)
        _nodes += node
        _index(node) = id
        id
      }
    )
  end intern

  /** Look up the `TType` of the node at `id`. */
  private def tpeOf(id: NodeId): TType = _nodes(id.i).tpe

  // ─── Expr opaque type ─────────────────────────────────────────────────────

  /** The typed expression handle: an opaque wrapper around `NodeId`.
    *
    * Because this type is defined inside `GraphBuilder`, a `b1.Expr[A]` and a `b2.Expr[A]` for distinct builder
    * instances are distinct types — cross-builder composition is a compile error.
    */
  opaque type Expr[A] = NodeId

  /** Factory methods for `Expr[A]` leaf nodes. */
  object Expr:

    /** Create a named input parameter with the given shape. */
    def param[A: DTypeOf](name: String, shape: Shape): Expr[A] =
      intern(TensorExpr.Param(name, TType(summon[DTypeOf[A]].dtype, shape)))

    /** Create a scalar named input parameter (rank-0 shape). */
    def param[A: DTypeOf](name: String): Expr[A] =
      param[A](name, Shape.scalar)

    /** Create a constant scalar value.
      *
      * Floating-point constants are hash-consed using bit-level equality.
      */
    def const[A: DTypeOf](value: A): Expr[A] =
      intern(TensorExpr.Const(value, TType(summon[DTypeOf[A]].dtype, Shape.scalar)))

    /** Lift a live `NDArray[A]` into the graph.
      *
      * The shape is derived from the array's runtime shape. Each call produces a **distinct** `Lift` node even if the
      * same array is passed multiple times.
      */
    def lift[A: DTypeOf](nd: NDArray[A]): Expr[A] =
      val dims: Array[Dim] = nd.shape.map(n => Dim.Known(n))
      val shape = new Shape(dims)
      val tpe = TType(summon[DTypeOf[A]].dtype, shape)
      val hid = _nextHandleId
      _nextHandleId += 1
      val handle = new NDArrayHandle { val id: Int = hid }
      val node = TensorExpr.Lift(handle, tpe)
      val nid = NodeId(_nodes.size)
      _nodes += node
      nid
    end lift

  end Expr

  // ─── Numeric operations ────────────────────────────────────────────────────

  extension [A: DTypeOf](e: Expr[A])(using IsNumeric[A])
    def +(o: Expr[A]): Expr[A] = intern(TensorExpr.Binary(BinaryOp.Add, e, o, tpeOf(e)))
    def -(o: Expr[A]): Expr[A] = intern(TensorExpr.Binary(BinaryOp.Sub, e, o, tpeOf(e)))
    def *(o: Expr[A]): Expr[A] = intern(TensorExpr.Binary(BinaryOp.Mul, e, o, tpeOf(e)))
    def /(o: Expr[A]): Expr[A] = intern(TensorExpr.Binary(BinaryOp.Div, e, o, tpeOf(e)))
    def unary_- : Expr[A] = intern(TensorExpr.Unary(UnaryOp.Neg, e, tpeOf(e)))
  end extension

  // ─── Comparison operations ────────────────────────────────────────────────

  extension [A: DTypeOf](e: Expr[A])
    def <(o: Expr[A]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.Lt, e, o, TType(DType.Bool, tpeOf(e).shape)))
    def <=(o: Expr[A]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.Lte, e, o, TType(DType.Bool, tpeOf(e).shape)))
    def >(o: Expr[A]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.Gt, e, o, TType(DType.Bool, tpeOf(e).shape)))
    def >=(o: Expr[A]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.Gte, e, o, TType(DType.Bool, tpeOf(e).shape)))
    def ===(o: Expr[A]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.Eq, e, o, TType(DType.Bool, tpeOf(e).shape)))
    def =!=(o: Expr[A]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.Neq, e, o, TType(DType.Bool, tpeOf(e).shape)))
  end extension

  // ─── Unary math operations ────────────────────────────────────────────────

  extension [A: DTypeOf](e: Expr[A])
    def sin: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Sin, e, tpeOf(e)))
    def cos: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Cos, e, tpeOf(e)))
    def tan: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Tan, e, tpeOf(e)))
    def exp: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Exp, e, tpeOf(e)))
    def log: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Log, e, tpeOf(e)))
    def sqrt: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Sqrt, e, tpeOf(e)))
    def abs: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Abs, e, tpeOf(e)))
    def reciprocal: Expr[A] = intern(TensorExpr.Unary(UnaryOp.Reciprocal, e, tpeOf(e)))
  end extension

  // ─── Boolean operations ───────────────────────────────────────────────────

  extension (e: Expr[Boolean])
    def &&(o: Expr[Boolean]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.And, e, o, tpeOf(e)))
    def ||(o: Expr[Boolean]): Expr[Boolean] =
      intern(TensorExpr.Binary(BinaryOp.Or, e, o, tpeOf(e)))
    def unary_! : Expr[Boolean] =
      intern(TensorExpr.Unary(UnaryOp.Not, e, tpeOf(e)))
  end extension

  // ─── Finaliser ────────────────────────────────────────────────────────────

  /** Finalise the graph, returning an immutable `TensorGraph` with `out` as the result node.
    *
    * The builder may continue to be used after calling `build`; subsequent calls produce graphs that contain all nodes
    * accumulated so far (including those from earlier `build` calls).
    */
  def build[A](out: Expr[A]): TensorGraph =
    TensorGraph(_nodes.toVector, out)

end GraphBuilder
