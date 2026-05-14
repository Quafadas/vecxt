package vecxt.fusion

import scala.collection.mutable
import vecxt.ndarray.NDArray

/** Type class that maps a Scala type `A` to its corresponding `DType`.
  *
  * Instances are provided for `Double` → `F64`, `Float` → `F32`, `Int` → `I32`, `Long` → `I64`, `Boolean` → `Bool`.
  */
trait DTypeOf[A]:
  def dtype: DType
end DTypeOf

object DTypeOf:
  given DTypeOf[Double] with
    def dtype: DType = DType.F64
  end given
  given DTypeOf[Float] with
    def dtype: DType = DType.F32
  end given
  given DTypeOf[Int] with
    def dtype: DType = DType.I32
  end given
  given DTypeOf[Long] with
    def dtype: DType = DType.I64
  end given
  given DTypeOf[Boolean] with
    def dtype: DType = DType.Bool
  end given
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
  * `Lift` nodes are deliberately **not** hash-consed: two separate `Expr.lift` calls over the same array always produce
  * distinct nodes so that the graph correctly reflects the number of distinct source arrays.
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

  /** Align two nodes for a binary operation, inserting `BCast` nodes as needed.
    *
    * If both nodes already have the same shape, they are returned unchanged. Otherwise, NumPy-style broadcasting is
    * attempted via `Shape.broadcast`. Each operand whose shape differs from the broadcast result is wrapped in a
    * `BCast` node (hash-consed as usual). Throws `IllegalArgumentException` if the shapes are incompatible.
    *
    * @return
    *   `(alignedA, alignedB, resultShape)`
    */
  private def align(a: NodeId, b: NodeId): (NodeId, NodeId, Shape) =
    val aTpe = tpeOf(a)
    val bTpe = tpeOf(b)
    if aTpe.shape == bTpe.shape then (a, b, aTpe.shape)
    else
      Shape.broadcast(aTpe.shape, bTpe.shape) match
        case Left(err) =>
          throw IllegalArgumentException(
            s"Cannot broadcast shapes ${aTpe.shape} and ${bTpe.shape}: ${err.message}"
          )
        case Right(broadcastShape) =>
          val newA =
            if aTpe.shape == broadcastShape then a
            else intern(TensorExpr.BCast(a, broadcastShape, TType(aTpe.dtype, broadcastShape)))
          val newB =
            if bTpe.shape == broadcastShape then b
            else intern(TensorExpr.BCast(b, broadcastShape, TType(bTpe.dtype, broadcastShape)))
          (newA, newB, broadcastShape)
    end if
  end align

  /** Compute the output shape of a reduction over the given axes.
    *
    * Axes are removed from the input shape: e.g. reducing shape `[3,4]` along axis 0 yields `[4]`.
    */
  private def reduceOutputShape(inputShape: Shape, axes: Seq[Int]): Shape =
    val axisSet = axes.toSet
    val outputDims = inputShape.dims.zipWithIndex.collect { case (d, i) if !axisSet.contains(i) => d }
    new Shape(outputDims)
  end reduceOutputShape

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
      * same array is passed multiple times. This deliberately bypasses hash-consing: two separate `lift` calls over the
      * same array always produce distinct nodes so that the graph correctly reflects the number of distinct source
      * arrays.
      */
    def lift[A: DTypeOf](nd: NDArray[A]): Expr[A] =
      val dims: Array[Dim] = nd.shape.map(n => Dim.Known(n))
      val shape = new Shape(dims)
      val tpe = TType(summon[DTypeOf[A]].dtype, shape)
      val hid = _nextHandleId
      _nextHandleId += 1
      val handle = new NDArrayHandle:
        val id: Int = hid
      val node = TensorExpr.Lift(handle, tpe)
      val nid = NodeId(_nodes.size)
      _nodes += node
      nid
    end lift

  end Expr

  // ─── Numeric operations ────────────────────────────────────────────────────

  extension [A: DTypeOf](e: Expr[A])(using IsNumeric[A])
    def +(o: Expr[A]): Expr[A] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Add, ae, oe, TType(tpeOf(e).dtype, shape)))
    end +
    def -(o: Expr[A]): Expr[A] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Sub, ae, oe, TType(tpeOf(e).dtype, shape)))
    end -
    def *(o: Expr[A]): Expr[A] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Mul, ae, oe, TType(tpeOf(e).dtype, shape)))
    end *
    def /(o: Expr[A]): Expr[A] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Div, ae, oe, TType(tpeOf(e).dtype, shape)))
    end /
    def unary_- : Expr[A] = intern(TensorExpr.Unary(UnaryOp.Neg, e, tpeOf(e)))
  end extension

  // ─── Comparison operations ────────────────────────────────────────────────

  extension [A: DTypeOf](e: Expr[A])
    def <(o: Expr[A]): Expr[Boolean] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Lt, ae, oe, TType(DType.Bool, shape)))
    end <
    def <=(o: Expr[A]): Expr[Boolean] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Lte, ae, oe, TType(DType.Bool, shape)))
    end <=
    def >(o: Expr[A]): Expr[Boolean] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Gt, ae, oe, TType(DType.Bool, shape)))
    end >
    def >=(o: Expr[A]): Expr[Boolean] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Gte, ae, oe, TType(DType.Bool, shape)))
    end >=
    def ===(o: Expr[A]): Expr[Boolean] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Eq, ae, oe, TType(DType.Bool, shape)))
    end ===
    def =!=(o: Expr[A]): Expr[Boolean] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Neq, ae, oe, TType(DType.Bool, shape)))
    end =!=
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
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.And, ae, oe, TType(DType.Bool, shape)))
    end &&
    def ||(o: Expr[Boolean]): Expr[Boolean] =
      val (ae, oe, shape) = align(e, o)
      intern(TensorExpr.Binary(BinaryOp.Or, ae, oe, TType(DType.Bool, shape)))
    end ||
    def unary_! : Expr[Boolean] =
      intern(TensorExpr.Unary(UnaryOp.Not, e, tpeOf(e)))
  end extension

  // ─── Cast ─────────────────────────────────────────────────────────────────

  extension [A](e: Expr[A])
    /** Cast this expression to dtype `B`, preserving shape. */
    def castTo[B: DTypeOf]: Expr[B] =
      val targetDtype = summon[DTypeOf[B]].dtype
      intern(TensorExpr.Cast(targetDtype, e, TType(targetDtype, tpeOf(e).shape)))
  end extension

  // ─── Reduce operations ────────────────────────────────────────────────────

  extension [A: DTypeOf](e: Expr[A])

    /** Reduce by summing along `axes`. Preserves the element dtype. */
    def reduceSum(axes: Int*): Expr[A] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.Sum, e, axes.toVector, TType(tpeOf(e).dtype, outShape)))
    end reduceSum

    /** Reduce by taking the product along `axes`. Preserves the element dtype. */
    def reduceProduct(axes: Int*): Expr[A] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.Product, e, axes.toVector, TType(tpeOf(e).dtype, outShape)))
    end reduceProduct

    /** Reduce by minimum along `axes`. Preserves the element dtype. */
    def reduceMin(axes: Int*): Expr[A] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.Min, e, axes.toVector, TType(tpeOf(e).dtype, outShape)))
    end reduceMin

    /** Reduce by maximum along `axes`. Preserves the element dtype. */
    def reduceMax(axes: Int*): Expr[A] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.Max, e, axes.toVector, TType(tpeOf(e).dtype, outShape)))
    end reduceMax

    /** Index of the maximum value along `axes`. Result dtype is I64. */
    def argMax(axes: Int*): Expr[Long] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.ArgMax, e, axes.toVector, TType(DType.I64, outShape)))
    end argMax

    /** Index of the minimum value along `axes`. Result dtype is I64. */
    def argMin(axes: Int*): Expr[Long] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.ArgMin, e, axes.toVector, TType(DType.I64, outShape)))
    end argMin

  end extension

  extension (e: Expr[Boolean])

    /** Reduce by logical AND along `axes`. */
    def reduceAll(axes: Int*): Expr[Boolean] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.All, e, axes.toVector, TType(DType.Bool, outShape)))
    end reduceAll

    /** Reduce by logical OR along `axes`. */
    def reduceAny(axes: Int*): Expr[Boolean] =
      val outShape = reduceOutputShape(tpeOf(e).shape, axes)
      intern(TensorExpr.Reduce(ReduceOp.Any, e, axes.toVector, TType(DType.Bool, outShape)))
    end reduceAny

  end extension

  // ─── Where (conditional select) ───────────────────────────────────────────

  /** Element-wise conditional: selects `thenE` where `cond` is true, `elseE` elsewhere.
    *
    * `thenE` and `elseE` must have the same dtype and shape. `cond` is broadcast to that shape if needed.
    */
  def where[A: DTypeOf](cond: Expr[Boolean], thenE: Expr[A], elseE: Expr[A]): Expr[A] =
    val xTpe = tpeOf(thenE)
    val (ae, be, shape) = align(thenE, elseE)
    val alignedCond =
      if tpeOf(cond).shape == shape then cond
      else intern(TensorExpr.BCast(cond, shape, TType(DType.Bool, shape)))
    intern(TensorExpr.Where(alignedCond, ae, be, TType(xTpe.dtype, shape)))
  end where

  // ─── Finaliser ────────────────────────────────────────────────────────────

  /** Finalise the graph, returning an immutable `TensorGraph` with `out` as the result node.
    *
    * The builder may continue to be used after calling `build`; subsequent calls produce graphs that contain all nodes
    * accumulated so far (including those from earlier `build` calls).
    */
  def build[A](out: Expr[A]): TensorGraph =
    TensorGraph(_nodes.toVector, out)

end GraphBuilder
