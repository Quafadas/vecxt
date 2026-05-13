package vecxt.fusion

/** An opaque handle to a live NDArray used as a leaf in `Lift` nodes.
  *
  * The handle carries enough information for an execution backend to identify and read the underlying buffer; the IR
  * itself treats it as an abstract reference.
  */
trait NDArrayHandle:
  /** A unique integer identifier for this handle, used in `toString`. */
  def id: Int
end NDArrayHandle

/** A typed node identifier in a `TensorGraph`.
  *
  * Indices are dense and zero-based: `graph.nodes(id.i)` always yields the corresponding node.
  */
final case class NodeId(i: Int) extends AnyVal:
  override def toString: String = i.toString
end NodeId

/** The semantic IR for tensor expressions.
  *
  * Every node carries its result type (`tpe`) so that any pass can read dtype and shape without re-inferring them.
  * Operands are referenced by `NodeId` so the graph is a DAG with shared nodes.
  */
sealed trait TensorExpr:
  def tpe: TType
end TensorExpr

object TensorExpr:
  /** A literal constant scalar or tensor value. */
  final case class Const(value: Any, tpe: TType) extends TensorExpr

  /** A named input parameter of the graph. */
  final case class Param(name: String, tpe: TType) extends TensorExpr

  /** A lifted live NDArray. The `handle` is an opaque reference; the IR never inspects it.
    */
  final case class Lift(handle: NDArrayHandle, tpe: TType) extends TensorExpr

  /** Elementwise unary operation on node `a`. */
  final case class Unary(op: UnaryOp, a: NodeId, tpe: TType) extends TensorExpr

  /** Elementwise binary operation on nodes `a` and `b` (same shape). */
  final case class Binary(op: BinaryOp, a: NodeId, b: NodeId, tpe: TType) extends TensorExpr

  /** Explicit dtype cast of node `a` to dtype `to` (shape-preserving). */
  final case class Cast(to: DType, a: NodeId, tpe: TType) extends TensorExpr

  /** Explicit broadcast of node `a` to shape `to`. */
  final case class BCast(a: NodeId, to: Shape, tpe: TType) extends TensorExpr

  /** Reduction of node `a` along `axes`.
    *
    * `axes` uses `Vector[Int]` rather than `Array[Int]` so that structural equality and hashing work without manual
    * overrides.
    *
    * Each element of `axes` must be a non-negative integer strictly less than the rank of the input node. Negative
    * (Python-style) indices and duplicate axes are not supported at the IR level; normalisation is the caller's
    * responsibility.
    */
  final case class Reduce(op: ReduceOp, a: NodeId, axes: Vector[Int], tpe: TType) extends TensorExpr

  /** Element-wise conditional select between `x` and `y` driven by boolean `c`. */
  final case class Where(c: NodeId, x: NodeId, y: NodeId, tpe: TType) extends TensorExpr
end TensorExpr

/** An immutable DAG of tensor expressions.
  *
  * `nodes(i)` is the node with `NodeId(i)`. Indices are dense and zero-based. `output` is the node whose value is the
  * result of the whole expression.
  *
  * @param nodes
  *   Dense vector of all nodes; position equals node id.
  * @param output
  *   Id of the result node.
  */
final case class TensorGraph(nodes: Vector[TensorExpr], output: NodeId):
  /** Total number of nodes in the graph. */
  def size: Int = nodes.length

  /** Retrieve the node for a given id. */
  def apply(id: NodeId): TensorExpr = nodes(id.i)

  /** Pretty-print the DAG: one line per node followed by the output id. */
  override def toString: String =
    val sb = new StringBuilder
    sb.append("graph {\n")
    nodes.zipWithIndex.foreach { (node, idx) =>
      sb.append(s"  $idx: ${nodeToString(node)}\n")
    }
    sb.append(s"  output: ${output.i}\n")
    sb.append("}")
    sb.toString()
  end toString

  private def nodeToString(node: TensorExpr): String =
    import TensorExpr.*
    val body = node match
      case Const(v, _)          => s"Const($v)"
      case Param(n, _)          => s"""Param("$n")"""
      case Lift(h, _)           => s"Lift(handle#${h.id})"
      case Unary(op, a, _)      => s"Unary($op, $a)"
      case Binary(op, a, b, _)  => s"Binary($op, $a, $b)"
      case Cast(to, a, _)       => s"Cast($to, $a)"
      case BCast(a, to, _)      => s"BCast($a, $to)"
      case Reduce(op, a, ax, _) => s"Reduce($op, $a, axes=${ax.mkString("[", ",", "]")})"
      case Where(c, x, y, _)    => s"Where($c, $x, $y)"
    s"$body : ${node.tpe}"
  end nodeToString
end TensorGraph
