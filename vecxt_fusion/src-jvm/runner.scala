package vecxt.fusion

/** JVM Phase-8 fused runner — top-level API for executing a `TensorGraph` through the fusion pipeline.
  *
  * The pipeline is: `Normalize` → `FusionPlanner` → `Schedule.lower` → `KernelExecutor` (one call per group).
  *
  * This is the run-time fusion path. Only F64 (Double) graphs are supported. Leaf nodes (`Param`, `Lift`, `Const`) are
  * materialised before the first kernel runs; each kernel's output is stored and available to downstream kernels.
  *
  * For the correctness oracle (cross-platform, all dtypes) use `Interpreter.eval` from Phase 7. `FusedRunner` and
  * `Interpreter` should produce the same numerical results for any F64 graph.
  */
object FusedRunner:

  /** Evaluate `graph` through the full fusion pipeline and return the output buffer.
    *
    * @param graph
    *   the tensor graph to evaluate (normalised internally).
    * @param params
    *   values for `TensorExpr.Param` nodes, keyed by parameter name. Each value is a flat, contiguous, column-major
    *   `Array[Double]` of `prod(shape)` elements.
    * @param lifts
    *   values for `TensorExpr.Lift` nodes, keyed by `NDArrayHandle.id`. Same array convention as `params`.
    * @return
    *   `Right(buf)` — the flat output buffer — or `Left(err)` if any group could not be scheduled.
    */
  def eval(
      graph: TensorGraph,
      params: Map[String, Array[Double]],
      lifts: Map[Int, Array[Double]] = Map.empty
  ): Either[Schedule.ScheduleError, Array[Double]] =
    val normalized = Normalize.run(graph)
    val plan = FusionPlanner.plan(normalized)

    Schedule.lower(plan).map { kernels =>
      // ── 1. Materialise all leaf nodes ──────────────────────────────────────
      val bufs = new Array[Array[Double]](normalized.size)
      var i = 0
      while i < normalized.size do
        normalized(NodeId(i)) match
          case TensorExpr.Param(name, _) =>
            bufs(i) = params.getOrElse(name, throw RuntimeException(s"FusedRunner: missing param '$name'"))
          case TensorExpr.Lift(handle, _) =>
            bufs(i) =
              lifts.getOrElse(handle.id, throw RuntimeException(s"FusedRunner: missing lift handle ${handle.id}"))
          case TensorExpr.Const(v, _) =>
            // Consts are inlined as Lit nodes in the scalar expr by the scheduler; however a Const that is
            // a group boundary input (e.g. shared across groups) needs a real buffer entry.
            bufs(i) = Array(v.asInstanceOf[Double])
          case _ => // non-leaf: will be written by a kernel below
        end match
        i += 1
      end while

      // ── 2. Execute each kernel in topological order ─────────────────────────
      kernels.foreach { kernel =>
        val inputs = kernel.inputNodes.map(id => bufs(id.i)).toArray
        val result = KernelExecutor.run(kernel, inputs)
        bufs(kernel.outputNode.i) = result
      }

      // ── 3. Return the final output buffer ──────────────────────────────────
      bufs(normalized.output.i)
    }
  end eval

end FusedRunner
