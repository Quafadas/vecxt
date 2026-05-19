package vecxt.fusion

import scala.collection.mutable

/** Phase-8 scheduler: lowers a `FusionPlan` into a sequence of `CompiledKernel`s.
  *
  * One `CompiledKernel` is produced per `FusionGroup`, in the execution order supplied by the planner. The kernels can
  * then be handed off to a platform-specific executor (see `KernelExecutor` on JVM).
  *
  * === Supported nodes (Phase 8 first cut) ===
  *
  *   - F64 leafs: `Const`, `Param`, `Lift` — become boundary inputs.
  *   - `Unary`, `Binary`, `Where` — lowered to `ScalarExpr` tree nodes.
  *   - `BCast(a, toShape)` — handled via broadcast-safe index wrapping in `ScalarExpr.Load`: the inner node `a` is
  *     loaded with its own `numel`, and the executor uses `i % numel` to map the flat output index.
  *   - `Cast(F64, a)` where `a` is already F64 — elided (identity).
  *   - `Reduce` as the terminal node of a group — lowered to `KernelIR.FullReduce` when all axes are reduced.
  *
  * === Unsupported in Phase 8 (produce `ScheduleError`) ===
  *
  *   - Non-F64 dtypes.
  *   - `Cast` to a dtype other than F64.
  *   - Partial-axis `Reduce` (only full all-axes reductions are supported).
  *   - Nested `Reduce` nodes inside a group body.
  */
object Schedule:

  // ── Error ADT ──────────────────────────────────────────────────────────────

  sealed trait ScheduleError:
    def message: String
  end ScheduleError

  object ScheduleError:

    /** A node uses a dtype other than F64, which is not yet supported. */
    final case class UnsupportedDType(nodeId: NodeId, dtype: DType) extends ScheduleError:
      def message: String = s"Node $nodeId: unsupported dtype $dtype (only F64 is supported in Phase 8)"
    end UnsupportedDType

    /** A `Reduce` node is encountered that only reduces some axes, not all. */
    final case class PartialReduce(nodeId: NodeId) extends ScheduleError:
      def message: String =
        s"Node $nodeId: partial-axis reduction is not supported in Phase 8; only full all-axes reductions are allowed"
    end PartialReduce

    /** A node that cannot be scheduled for another reason. */
    final case class UnsupportedNode(nodeId: NodeId, reason: String) extends ScheduleError:
      def message: String = s"Node $nodeId: $reason"
    end UnsupportedNode

  end ScheduleError

  // ── Public API ─────────────────────────────────────────────────────────────

  /** Lower every `FusionGroup` in `plan` to a `CompiledKernel`.
    *
    * Groups are returned in the same execution order as `plan.groups`. The caller should execute them in sequence,
    * feeding each kernel's output as an input to later kernels that reference its `outputNode`.
    *
    * @param plan a `FusionPlan` produced by `FusionPlanner.plan`, ideally over a normalised graph.
    * @return `Right(kernels)` on success, or `Left(error)` if any group cannot be scheduled.
    */
  def lower(plan: FusionPlan): Either[ScheduleError, Vector[CompiledKernel]] =
    val graph   = plan.graph
    val results = Vector.newBuilder[CompiledKernel]
    var error: ScheduleError = null
    val it = plan.groups.iterator
    while it.hasNext && error == null do
      lowerGroup(it.next(), graph) match
        case Right(k) => results += k
        case Left(e)  => error = e
    end while
    if error != null then Left(error)
    else Right(results.result())
  end lower

  // ── Group lowering ─────────────────────────────────────────────────────────

  private def lowerGroup(group: FusionGroup, graph: TensorGraph): Either[ScheduleError, CompiledKernel] =
    graph(group.output) match
      case TensorExpr.Reduce(op, bodyId, axes, _) =>
        lowerReduceGroup(group, graph, op, bodyId, axes)
      case _ =>
        lowerElementwiseGroup(group, graph)
  end lowerGroup

  // ── Elementwise group ──────────────────────────────────────────────────────

  private def lowerElementwiseGroup(
      group: FusionGroup,
      graph: TensorGraph
  ): Either[ScheduleError, CompiledKernel] =
    val outNode  = graph(group.output)
    val outShape = knownShape(outNode.tpe.shape)
    val outNumel = shapeNumel(outNode.tpe.shape)
    val inMap    = buildInputMap(group, graph)
    val cache    = mutable.HashMap.empty[NodeId, Either[ScheduleError, ScalarExpr]]

    buildScalarExpr(group.output, group, graph, inMap, outNumel, cache).map { expr =>
      val inputNumel = group.inputs.map(id => shapeNumel(graph(id).tpe.shape)).toArray
      CompiledKernel(
        KernelIR.Elementwise(outShape, inputNumel, expr),
        group.inputs,
        group.output
      )
    }
  end lowerElementwiseGroup

  // ── Reduce group ───────────────────────────────────────────────────────────

  private def lowerReduceGroup(
      group:  FusionGroup,
      graph:  TensorGraph,
      op:     ReduceOp,
      bodyId: NodeId,
      axes:   Vector[Int]
  ): Either[ScheduleError, CompiledKernel] =
    val inShape = graph(bodyId).tpe.shape
    val rank    = inShape.rank

    // Phase 8: only full all-axes reductions are supported.
    val isFullReduce = (rank == 0 && axes.isEmpty) || (rank > 0 && axes == (0 until rank).toVector)
    if !isFullReduce then
      return Left(ScheduleError.PartialReduce(group.output))
    end if

    val inNumel  = shapeNumel(inShape)
    val inMap    = buildInputMap(group, graph)
    val cache    = mutable.HashMap.empty[NodeId, Either[ScheduleError, ScalarExpr]]

    buildScalarExpr(bodyId, group, graph, inMap, inNumel, cache).map { bodyExpr =>
      val inputNumel = group.inputs.map(id => shapeNumel(graph(id).tpe.shape)).toArray
      CompiledKernel(
        KernelIR.FullReduce(inNumel, inputNumel, op, bodyExpr),
        group.inputs,
        group.output
      )
    }
  end lowerReduceGroup

  // ── Scalar expression construction ─────────────────────────────────────────

  /** Map each boundary input `NodeId` to its `(BufRef.Input(k), numel)`. */
  private def buildInputMap(
      group: FusionGroup,
      graph: TensorGraph
  ): Map[NodeId, (BufRef.Input, Int)] =
    group.inputs.zipWithIndex.foldLeft(Map.empty[NodeId, (BufRef.Input, Int)]) { case (m, (nodeId, k)) =>
      m + (nodeId -> ((BufRef.Input(k): BufRef.Input, shapeNumel(graph(nodeId).tpe.shape))))
    }
  end buildInputMap

  /** Recursively build the `ScalarExpr` for `nodeId`.
    *
    * Boundary inputs (in `inMap`) become `ScalarExpr.Load`. Internal group nodes are expanded recursively. Results are
    * memoised in `cache` to avoid re-building shared subexpressions.
    */
  private def buildScalarExpr(
      nodeId:   NodeId,
      group:    FusionGroup,
      graph:    TensorGraph,
      inMap:    Map[NodeId, (BufRef.Input, Int)],
      outNumel: Int,
      cache:    mutable.HashMap[NodeId, Either[ScheduleError, ScalarExpr]]
  ): Either[ScheduleError, ScalarExpr] =
    cache.getOrElseUpdate(nodeId, computeScalarExpr(nodeId, group, graph, inMap, outNumel, cache))
  end buildScalarExpr

  /** Compute (uncached) the `ScalarExpr` for a single `nodeId`. */
  private def computeScalarExpr(
      nodeId:   NodeId,
      group:    FusionGroup,
      graph:    TensorGraph,
      inMap:    Map[NodeId, (BufRef.Input, Int)],
      outNumel: Int,
      cache:    mutable.HashMap[NodeId, Either[ScheduleError, ScalarExpr]]
  ): Either[ScheduleError, ScalarExpr] =
    inMap.get(nodeId) match
      case Some((buf, numel)) =>
        Right(ScalarExpr.Load(buf, numel))

      case None =>
        graph(nodeId) match

          case TensorExpr.Const(v, tpe) =>
            if tpe.dtype != DType.F64 then Left(ScheduleError.UnsupportedDType(nodeId, tpe.dtype))
            else Right(ScalarExpr.Lit(v.asInstanceOf[Double]))

          case TensorExpr.Unary(op, a, _) =>
            buildScalarExpr(a, group, graph, inMap, outNumel, cache).map { ae =>
              ScalarExpr.SUnary(op, ae)
            }

          case TensorExpr.Binary(op, a, b, _) =>
            for
              ae <- buildScalarExpr(a, group, graph, inMap, outNumel, cache)
              be <- buildScalarExpr(b, group, graph, inMap, outNumel, cache)
            yield ScalarExpr.SBinary(op, ae, be)

          case TensorExpr.BCast(a, _, _) =>
            // Broadcast is handled by Load.numel: the inner node is loaded with its own element count,
            // and the executor wraps the flat index with i % numel automatically.
            buildScalarExpr(a, group, graph, inMap, outNumel, cache)

          case TensorExpr.Cast(to, a, _) =>
            if to != DType.F64 then Left(ScheduleError.UnsupportedDType(nodeId, to))
            else buildScalarExpr(a, group, graph, inMap, outNumel, cache)

          case TensorExpr.Where(c, x, y, _) =>
            for
              ce <- buildScalarExpr(c, group, graph, inMap, outNumel, cache)
              xe <- buildScalarExpr(x, group, graph, inMap, outNumel, cache)
              ye <- buildScalarExpr(y, group, graph, inMap, outNumel, cache)
            yield ScalarExpr.Select(ce, xe, ye)

          case TensorExpr.Reduce(_, _, _, _) =>
            Left(ScheduleError.UnsupportedNode(nodeId, "nested Reduce inside a group body is not allowed"))

          case _: TensorExpr.Param | _: TensorExpr.Lift =>
            // These should always appear in inMap if the plan is valid; this path is a safety net.
            Left(ScheduleError.UnsupportedNode(nodeId, "unexpected leaf node inside group body (plan invariant violated)"))

        end match
    end match
  end computeScalarExpr

  // ── Utilities (package-private so tests and executor can reuse them) ────────

  /** Total number of elements for a shape; treats `Sym`/`Unknown` dims as 1. */
  private[fusion] def shapeNumel(shape: Shape): Int =
    shape.dims.foldLeft(1) {
      case (acc, Dim.Known(n)) => acc * n
      case (acc, _)            => acc
    }
  end shapeNumel

  /** Convert a `Shape` to a concrete `Array[Int]`, treating `Sym`/`Unknown` dims as 1. */
  private[fusion] def knownShape(shape: Shape): Array[Int] =
    shape.dims.map {
      case Dim.Known(n) => n
      case _            => 1
    }
  end knownShape

end Schedule
