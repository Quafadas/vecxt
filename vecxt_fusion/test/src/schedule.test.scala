package vecxt.fusion

import munit.FunSuite

class SchedulePhase8Test extends FunSuite:

  // ── shared type aliases ──────────────────────────────────────────────────

  private val f64s  = TType(DType.F64, Shape.scalar)
  private val f64v4 = TType(DType.F64, Shape(Dim.Known(4)))
  private val f64v3 = TType(DType.F64, Shape(Dim.Known(3)))
  private val f64m  = TType(DType.F64, Shape(Dim.Known(3), Dim.Known(4)))

  // ── graph helpers ─────────────────────────────────────────────────────────

  private def param(name: String, tpe: TType = f64v4): TensorExpr = TensorExpr.Param(name, tpe)
  private def g(nodes: TensorExpr*): TensorGraph = TensorGraph(nodes.toVector, NodeId(nodes.length - 1))

  private def planThen(graph: TensorGraph) =
    val plan = FusionPlanner.plan(Normalize.run(graph))
    Schedule.lower(plan)

  // ══════════════════════════════════════════════════════════════════════════
  // Schedule.lower — structural tests (no execution)
  // ══════════════════════════════════════════════════════════════════════════

  test("schedule: single Unary → one Elementwise kernel") {
    // 0=Param(x,[4]), 1=Sin(0)
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4)
    )
    planThen(graph) match
      case Left(err) => fail(s"unexpected schedule error: ${err.message}")
      case Right(kernels) =>
        assertEquals(kernels.size, 1)
        val k = kernels.head
        assert(k.ir.isInstanceOf[KernelIR.Elementwise])
        assertEquals(k.inputNodes.size, 1)
        assertEquals(k.inputNodes.head, NodeId(0))
  }

  test("schedule: x + y → one Elementwise kernel, two inputs") {
    val graph = g(
      param("x"),
      param("y"),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64v4)
    )
    planThen(graph) match
      case Left(err)      => fail(err.message)
      case Right(kernels) =>
        assertEquals(kernels.size, 1)
        val k = kernels.head
        assert(k.ir.isInstanceOf[KernelIR.Elementwise])
        assertEquals(k.inputNodes.size, 2)
  }

  test("schedule: Elementwise outShape matches input param shape") {
    val graph = g(
      param("x", f64v4),
      TensorExpr.Unary(UnaryOp.Neg, NodeId(0), f64v4)
    )
    planThen(graph) match
      case Left(err) => fail(err.message)
      case Right(kernels) =>
        val e = kernels.head.ir.asInstanceOf[KernelIR.Elementwise]
        assertEquals(e.outShape.toSeq, Seq(4))
        assertEquals(e.inputNumel.toSeq, Seq(4))
  }

  test("schedule: chain sin(cos(x)) → one kernel with single input") {
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(1), f64v4)
    )
    planThen(graph) match
      case Left(err) => fail(err.message)
      case Right(kernels) =>
        assertEquals(kernels.size, 1)
        assertEquals(kernels.head.inputNodes.size, 1) // x is the only external input
  }

  test("schedule: sin(x) + cos(x) → one kernel, one input, SBinary at root") {
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    planThen(graph) match
      case Left(err) => fail(err.message)
      case Right(kernels) =>
        assertEquals(kernels.size, 1)
        assertEquals(kernels.head.inputNodes.size, 1)
        val e = kernels.head.ir.asInstanceOf[KernelIR.Elementwise]
        assert(e.expr.isInstanceOf[ScalarExpr.SBinary], s"expected SBinary root, got ${e.expr}")
  }

  test("schedule: full reduce → one FullReduce kernel") {
    // 0=Param(x,[4]), 1=Reduce(Sum, 0, axes=[0])
    val graph = g(
      param("x"),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), f64s)
    )
    planThen(graph) match
      case Left(err) => fail(err.message)
      case Right(kernels) =>
        assertEquals(kernels.size, 1)
        val k = kernels.head
        val r = k.ir.asInstanceOf[KernelIR.FullReduce]
        assertEquals(r.op, ReduceOp.Sum)
        assertEquals(r.inNumel, 4)
        assertEquals(k.inputNodes, Vector(NodeId(0)))
  }

  test("schedule: sum(sin(x) + 1) → one FullReduce kernel, body is SBinary") {
    val graph = g(
      param("x"),
      TensorExpr.Const(1.0, f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64v4),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(2), Vector(0), f64s)
    )
    planThen(graph) match
      case Left(err) => fail(err.message)
      case Right(kernels) =>
        assertEquals(kernels.size, 1)
        val r = kernels.head.ir.asInstanceOf[KernelIR.FullReduce]
        assertEquals(r.op, ReduceOp.Sum)
        // body should be Add(Load, Lit(1.0)) — not just Load
        assert(r.bodyExpr.isInstanceOf[ScalarExpr.SBinary], s"expected SBinary body, got ${r.bodyExpr}")
  }

  test("schedule: two-group plan produces two kernels in order") {
    // y = sin(x) has refCount=2 so the planner splits it into two groups.
    // 0=Param(x), 1=Sin(0), 2=Exp(1), 3=Add(1,2)
    // Sin has 2 consumers (Exp and Add) → materialised as boundary → two groups.
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    planThen(graph) match
      case Left(err) => fail(err.message)
      case Right(kernels) =>
        assertEquals(kernels.size, 2)
        // first kernel output is Sin, second uses it as input
        val k0 = kernels(0)
        val k1 = kernels(1)
        assert(k1.inputNodes.contains(k0.outputNode))
  }

  test("schedule: Where node → Elementwise with Select expr") {
    // 0=Param(c,bool shape), 1=Param(x), 2=Param(y), 3=Where(0,1,2)
    val boolV4 = TType(DType.Bool, Shape(Dim.Known(4)))
    val graph = TensorGraph(
      Vector(
        TensorExpr.Param("c", boolV4),
        TensorExpr.Param("x", f64v4),
        TensorExpr.Param("y", f64v4),
        TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64v4)
      ),
      NodeId(3)
    )
    FusionPlanner.plan(graph) |> { plan =>
      Schedule.lower(plan) match
        case Left(err) => fail(err.message)
        case Right(kernels) =>
          assertEquals(kernels.size, 1)
          val e = kernels.head.ir.asInstanceOf[KernelIR.Elementwise]
          assert(e.expr.isInstanceOf[ScalarExpr.Select], s"expected Select root, got ${e.expr}")
    }
  }

  // ── Error cases ───────────────────────────────────────────────────────────

  test("schedule error: partial reduce (axes don't cover all dims) → PartialReduce error") {
    // Reduce a [3,4] matrix along axis 0 only → output [4] (partial reduce)
    val graph = g(
      param("x", f64m),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), f64v4) // only axis 0, not all
    )
    planThen(graph) match
      case Left(err) =>
        assert(err.isInstanceOf[Schedule.ScheduleError.PartialReduce], s"expected PartialReduce, got $err")
      case Right(_) =>
        fail("expected PartialReduce error for partial-axis reduce")
  }

  // ── outputNode wiring ─────────────────────────────────────────────────────

  test("schedule: outputNode of each kernel is the group's output NodeId") {
    val graph = g(
      param("x"),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64v4),
      TensorExpr.Unary(UnaryOp.Exp, NodeId(1), f64v4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(1), NodeId(2), f64v4)
    )
    val plan = FusionPlanner.plan(Normalize.run(graph))
    plan.groups.zipWithIndex.foreach { (group, gi) =>
      val _ = gi // suppress unused warning
      Schedule.lower(plan) match
        case Left(err) => fail(err.message)
        case Right(kernels) =>
          kernels.foreach { k =>
            assert(
              plan.groups.exists(_.output == k.outputNode),
              s"outputNode ${k.outputNode} not found in any FusionGroup"
            )
          }
    }
  }

end SchedulePhase8Test

// ── Extension for pipe syntax used in one test ────────────────────────────────
extension [A](a: A)
  private def |>[B](f: A => B): B = f(a)
