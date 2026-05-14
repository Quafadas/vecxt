package vecxt.fusion

/** Error produced by `TypeCheck.infer` when a node's type is inconsistent with its children.
  *
  * Every variant carries the `NodeId` of the offending node so that callers can point the user at the specific failure
  * site in the graph.
  */
sealed trait TypeError:
  def at: NodeId
  def message: String
end TypeError

object TypeError:

  /** A binary or broadcast operation has operands whose shapes are incompatible. */
  final case class ShapeMismatch(at: NodeId, message: String) extends TypeError

  /** A node's element dtype is inconsistent: wrong input dtype or the stored type disagrees with the inferred type.
    */
  final case class DTypeMismatch(at: NodeId, message: String) extends TypeError

  /** A `Reduce` node lists axes that are outside the valid range for the input rank. */
  final case class InvalidAxes(at: NodeId, message: String) extends TypeError

end TypeError

/** Type-checker and type-inference pass for `TensorGraph`.
  *
  * `TypeCheck.infer` walks every node in topological order (nodes are stored densely so that each node's index is its
  * `NodeId`, and parents always appear before children). For each node it re-derives the result type from the types of
  * its children and then verifies that the stored `tpe` field agrees with the derivation.
  *
  * The pass does **not** insert `BCast` or `Cast` nodes: that is the responsibility of the `GraphBuilder`. The pass
  * exists as a safety / sanity check for programmatically constructed graphs and as a correctness oracle for tests.
  */
object TypeCheck:

  /** Infer and validate every node in `graph`.
    *
    * Returns `Right(graph)` (unchanged) if all nodes are well-typed, or `Left(err)` for the first type error found.
    */
  def infer(graph: TensorGraph): Either[TypeError, TensorGraph] =
    val inferred = new Array[TType](graph.nodes.size)
    var result: Either[TypeError, TensorGraph] = Right(graph)
    var i = 0
    while i < graph.nodes.size && result.isRight do
      val nodeId = NodeId(i)
      checkNode(nodeId, graph.nodes(i), inferred) match
        case Left(err)  => result = Left(err)
        case Right(tpe) => inferred(i) = tpe
      end match
      i += 1
    end while
    result
  end infer

  // ─── per-node checker ──────────────────────────────────────────────────────

  private def checkNode(
      nodeId: NodeId,
      node: TensorExpr,
      inferred: Array[TType]
  ): Either[TypeError, TType] =
    import TensorExpr.*
    node match

      // Leaf nodes: trust the stored type as-is.
      case Const(_, tpe) => Right(tpe)
      case Param(_, tpe) => Right(tpe)
      case Lift(_, tpe)  => Right(tpe)

      // ── Unary ──────────────────────────────────────────────────────────────
      case Unary(op, a, tpe) =>
        val aTpe = inferred(a.i)
        val resultDtype = op match
          case UnaryOp.Not =>
            if aTpe.dtype != DType.Bool then
              return Left(
                TypeError.DTypeMismatch(nodeId, s"Not requires Bool input, got ${aTpe.dtype}")
              )
            end if
            DType.Bool
          case _ => aTpe.dtype
        val inferredTpe = TType(resultDtype, aTpe.shape)
        checkStored(nodeId, inferredTpe, tpe)

      // ── Binary ─────────────────────────────────────────────────────────────
      case Binary(op, a, b, tpe) =>
        val aTpe = inferred(a.i)
        val bTpe = inferred(b.i)
        if aTpe.shape != bTpe.shape then
          return Left(
            TypeError.ShapeMismatch(
              nodeId,
              s"Binary $op: operand shapes ${aTpe.shape} and ${bTpe.shape} differ; a BCast node is required"
            )
          )
        end if
        val (resultDtype, dtypeOk) = op match
          case BinaryOp.Add | BinaryOp.Sub | BinaryOp.Mul | BinaryOp.Div | BinaryOp.Pow | BinaryOp.Min | BinaryOp.Max =>
            (aTpe.dtype, aTpe.dtype == bTpe.dtype)
          case BinaryOp.Eq | BinaryOp.Neq | BinaryOp.Lt | BinaryOp.Lte | BinaryOp.Gt | BinaryOp.Gte =>
            (DType.Bool, aTpe.dtype == bTpe.dtype)
          case BinaryOp.And | BinaryOp.Or =>
            (DType.Bool, aTpe.dtype == DType.Bool && bTpe.dtype == DType.Bool)
        if !dtypeOk then
          return Left(
            TypeError.DTypeMismatch(
              nodeId,
              s"Binary $op: incompatible dtypes ${aTpe.dtype} and ${bTpe.dtype}"
            )
          )
        end if
        val inferredTpe = TType(resultDtype, aTpe.shape)
        checkStored(nodeId, inferredTpe, tpe)

      // ── Cast ───────────────────────────────────────────────────────────────
      case Cast(to, a, tpe) =>
        val aTpe = inferred(a.i)
        val inferredTpe = TType(to, aTpe.shape)
        checkStored(nodeId, inferredTpe, tpe)

      // ── BCast ──────────────────────────────────────────────────────────────
      case BCast(a, targetShape, tpe) =>
        val aTpe = inferred(a.i)
        Shape.broadcast(aTpe.shape, targetShape) match
          case Left(err) =>
            Left(
              TypeError.ShapeMismatch(
                nodeId,
                s"BCast: cannot broadcast ${aTpe.shape} to $targetShape: ${err.message}"
              )
            )
          case Right(broadcastShape) =>
            if broadcastShape != targetShape then
              Left(
                TypeError.ShapeMismatch(
                  nodeId,
                  s"BCast: broadcasting ${aTpe.shape} yields $broadcastShape, not $targetShape"
                )
              )
            else
              val inferredTpe = TType(aTpe.dtype, targetShape)
              checkStored(nodeId, inferredTpe, tpe)
        end match

      // ── Reduce ─────────────────────────────────────────────────────────────
      case Reduce(op, a, axes, tpe) =>
        val aTpe = inferred(a.i)
        val rank = aTpe.shape.rank
        axes.find(ax => ax < 0 || ax >= rank) match
          case Some(ax) =>
            return Left(
              TypeError.InvalidAxes(nodeId, s"Reduce: axis $ax out of range for rank-$rank input")
            )
          case None => ()
        end match
        val axisSet = axes.toSet
        val outputDims = aTpe.shape.dims.zipWithIndex.collect { case (d, i) if !axisSet.contains(i) => d }
        val outputShape = new Shape(outputDims)
        val outputDtype = op match
          case ReduceOp.Sum | ReduceOp.Product | ReduceOp.Min | ReduceOp.Max => aTpe.dtype
          case ReduceOp.All | ReduceOp.Any                                   => DType.Bool
          case ReduceOp.ArgMax | ReduceOp.ArgMin                             => DType.I64
        val inferredTpe = TType(outputDtype, outputShape)
        checkStored(nodeId, inferredTpe, tpe)

      // ── Where ──────────────────────────────────────────────────────────────
      case Where(c, x, y, tpe) =>
        val cTpe = inferred(c.i)
        val xTpe = inferred(x.i)
        val yTpe = inferred(y.i)
        if cTpe.dtype != DType.Bool then
          return Left(
            TypeError.DTypeMismatch(nodeId, s"Where: condition must be Bool, got ${cTpe.dtype}")
          )
        end if
        if xTpe.shape != yTpe.shape then
          return Left(
            TypeError.ShapeMismatch(
              nodeId,
              s"Where: branch shapes differ: ${xTpe.shape} vs ${yTpe.shape}"
            )
          )
        end if
        if xTpe.dtype != yTpe.dtype then
          return Left(
            TypeError.DTypeMismatch(
              nodeId,
              s"Where: branch dtypes differ: ${xTpe.dtype} vs ${yTpe.dtype}"
            )
          )
        end if
        checkStored(nodeId, xTpe, tpe)
    end match

  end checkNode

  // ─── helper ────────────────────────────────────────────────────────────────

  private def checkStored(
      nodeId: NodeId,
      inferredTpe: TType,
      storedTpe: TType
  ): Either[TypeError, TType] =
    if inferredTpe != storedTpe then
      Left(
        TypeError.DTypeMismatch(
          nodeId,
          s"node stores $storedTpe but type inferred from children is $inferredTpe"
        )
      )
    else Right(inferredTpe)

end TypeCheck
