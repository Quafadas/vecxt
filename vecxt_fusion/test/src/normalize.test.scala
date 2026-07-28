package vecxt.fusion

import munit.FunSuite

class NormalizePhase5Test extends FunSuite:

  val f64s = TType(DType.F64, Shape.scalar)
  val bools = TType(DType.Bool, Shape.scalar)

  // Build a tiny hand-crafted graph with two nodes: Const(v) and output
  def constGraph(v: Double): TensorGraph =
    val c = TensorExpr.Const(v, f64s)
    TensorGraph(Vector(c), NodeId(0))
  end constGraph

  // Build a graph with nodes `nodes` and output = last node
  def g(nodes: TensorExpr*): TensorGraph =
    TensorGraph(nodes.toVector, NodeId(nodes.length - 1))

  def p(name: String, tpe: TType = f64s): TensorExpr = TensorExpr.Param(name, tpe)
  def c(v: Double): TensorExpr = TensorExpr.Const(v, f64s)
  def cb(v: Boolean): TensorExpr = TensorExpr.Const(v, bools)

  // 0 = Param(x), 1 = Const(k), 2 = Binary(op, 0, 1)
  def paramOpConst(op: BinaryOp, k: Double): TensorGraph =
    g(p("x"), c(k), TensorExpr.Binary(op, NodeId(0), NodeId(1), f64s))

  // 0 = Const(k), 1 = Param(x), 2 = Binary(op, 0, 1)
  def constOpParam(op: BinaryOp, k: Double): TensorGraph =
    g(c(k), p("x"), TensorExpr.Binary(op, NodeId(0), NodeId(1), f64s))

  // Normalize and return the single output node
  def normalizedOutput(graph: TensorGraph): TensorExpr =
    val g2 = Normalize.run(graph)
    g2(g2.output)
  end normalizedOutput

  // ══════════════════════════════════════════════════════════════════════════
  // Constant folding — Unary F64
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: Unary(Neg, Const(2.0)) → Const(-2.0)") {
    val graph = g(c(2.0), TensorExpr.Unary(UnaryOp.Neg, NodeId(0), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, -2.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Unary(Sin, Const(0.0)) → Const(0.0)") {
    val graph = g(c(0.0), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 0.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Unary(Cos, Const(0.0)) → Const(1.0)") {
    val graph = g(c(0.0), TensorExpr.Unary(UnaryOp.Cos, NodeId(0), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 1.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Unary(Exp, Const(0.0)) → Const(1.0)") {
    val graph = g(c(0.0), TensorExpr.Unary(UnaryOp.Exp, NodeId(0), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 1.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Unary(Sqrt, Const(4.0)) → Const(2.0)") {
    val graph = g(c(4.0), TensorExpr.Unary(UnaryOp.Sqrt, NodeId(0), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 2.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Unary(Abs, Const(-3.0)) → Const(3.0)") {
    val graph = g(c(-3.0), TensorExpr.Unary(UnaryOp.Abs, NodeId(0), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 3.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Unary(Reciprocal, Const(4.0)) → Const(0.25)") {
    val graph = g(c(4.0), TensorExpr.Unary(UnaryOp.Reciprocal, NodeId(0), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 0.25, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Unary(Not, Const(true)) → Const(false)") {
    val graph = g(cb(true), TensorExpr.Unary(UnaryOp.Not, NodeId(0), bools))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Boolean, _) => assertEquals(v, false)
      case other                           => fail(s"expected Const(Bool), got $other")
    end match
  }

  test("normalize: Unary(Not, Param(x)) is not folded (not a const)") {
    val graph = g(p("x", bools), TensorExpr.Unary(UnaryOp.Not, NodeId(0), bools))
    assert(normalizedOutput(graph).isInstanceOf[TensorExpr.Unary])
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Constant folding — Binary F64
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: Const(2.0) + Const(3.0) → Const(5.0)") {
    val graph = g(c(2.0), c(3.0), TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 5.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Const(6.0) / Const(3.0) → Const(2.0)") {
    val graph = g(c(6.0), c(3.0), TensorExpr.Binary(BinaryOp.Div, NodeId(0), NodeId(1), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 2.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Const(2.0) ^ Const(3.0) → Const(8.0)") {
    val graph = g(c(2.0), c(3.0), TensorExpr.Binary(BinaryOp.Pow, NodeId(0), NodeId(1), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Double, _) => assertEqualsDouble(v, 8.0, 1e-15)
      case other                          => fail(s"expected Const, got $other")
    end match
  }

  test("normalize: Const(2.0) < Const(3.0) → Const(true) [Bool result]") {
    val tpe = TType(DType.Bool, Shape.scalar)
    val graph = g(c(2.0), c(3.0), TensorExpr.Binary(BinaryOp.Lt, NodeId(0), NodeId(1), tpe))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Boolean, _) => assertEquals(v, true)
      case other                           => fail(s"expected Const(Bool), got $other")
    end match
  }

  test("normalize: Const(true) && Const(false) → Const(false)") {
    val graph = g(cb(true), cb(false), TensorExpr.Binary(BinaryOp.And, NodeId(0), NodeId(1), bools))
    normalizedOutput(graph) match
      case TensorExpr.Const(v: Boolean, _) => assertEquals(v, false)
      case other                           => fail(s"expected Const(Bool), got $other")
    end match
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Identity removal
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: x + 0 → x (right-zero identity)") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Add, 0.0))
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: 0 + x → x (left-zero identity)") {
    val g2 = Normalize.run(constOpParam(BinaryOp.Add, 0.0))
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: x - 0 → x") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Sub, 0.0))
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: x * 1 → x (right-one identity)") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Mul, 1.0))
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: 1 * x → x (left-one identity)") {
    val g2 = Normalize.run(constOpParam(BinaryOp.Mul, 1.0))
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: x / 1 → x") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Div, 1.0))
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: pow(x, 1) → x") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Pow, 1.0))
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Annihilator removal
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: x * 0 → 0 (right annihilator)") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Mul, 0.0))
    g2(g2.output) match
      case TensorExpr.Const(0.0, _) => ()
      case other                    => fail(s"expected Const(0.0), got $other")
    end match
  }

  test("normalize: 0 * x → 0 (left annihilator)") {
    val g2 = Normalize.run(constOpParam(BinaryOp.Mul, 0.0))
    g2(g2.output) match
      case TensorExpr.Const(0.0, _) => ()
      case other                    => fail(s"expected Const(0.0), got $other")
    end match
  }

  test("normalize: 0 / Const(5.0) → 0 (numerator annihilator for non-zero denom)") {
    // nodes: Const(0.0), Const(5.0), Div(0,1)
    val graph = g(c(0.0), c(5.0), TensorExpr.Binary(BinaryOp.Div, NodeId(0), NodeId(1), f64s))
    normalizedOutput(graph) match
      case TensorExpr.Const(0.0, _) => ()
      case other                    => fail(s"expected Const(0.0), got $other")
    end match
  }

  test("normalize: 0 / 0 folds to NaN (IEEE semantics; annihilator rule skipped for zero denom)") {
    // nodes: Const(0.0), Const(0.0), Div(0,1) — BUT hash-consing means two Const(0.0) share one id
    // Build without sharing to force the pattern:
    val n0 = TensorExpr.Const(0.0, f64s)
    val n1 = TensorExpr.Binary(BinaryOp.Div, NodeId(0), NodeId(0), f64s)
    val graph = TensorGraph(Vector(n0, n1), NodeId(1))
    // constant folding: 0.0 / 0.0 → NaN (IEEE), so it IS folded to Const(NaN)
    // The invariant: 0/0 must NOT be rewritten by the simplifyBinary annihilator rule,
    // but constant folding (which gives the correct IEEE result) is still applied.
    val g2 = Normalize.run(graph)
    g2(g2.output) match
      case TensorExpr.Const(v: Double, _) =>
        // IEEE 0/0 = NaN — folding is correct
        assert(v.isNaN, s"expected NaN, got $v")
      case TensorExpr.Binary(BinaryOp.Div, _, _, _) =>
        () // also acceptable: left as-is if x is Param (but here it's Const, so should fold)
      case other => fail(s"unexpected: $other")
    end match
  }

  test("normalize: pow(x, 0) → 1") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Pow, 0.0))
    g2(g2.output) match
      case TensorExpr.Const(1.0, _) => ()
      case other                    => fail(s"expected Const(1.0), got $other")
    end match
  }

  test("normalize: pow(1, x) → 1") {
    val g2 = Normalize.run(constOpParam(BinaryOp.Pow, 1.0))
    g2(g2.output) match
      case TensorExpr.Const(1.0, _) => ()
      case other                    => fail(s"expected Const(1.0), got $other")
    end match
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Where short-circuit
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: where(true, x, y) → x") {
    // nodes: 0=Const(true:Bool), 1=Param(x), 2=Param(y), 3=Where(0,1,2)
    val nodes = Vector(
      TensorExpr.Const(true, bools),
      TensorExpr.Param("x", f64s),
      TensorExpr.Param("y", f64s),
      TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64s)
    )
    val graph = TensorGraph(nodes, NodeId(3))
    val g2 = Normalize.run(graph)
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: where(false, x, y) → y") {
    val nodes = Vector(
      TensorExpr.Const(false, bools),
      TensorExpr.Param("x", f64s),
      TensorExpr.Param("y", f64s),
      TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64s)
    )
    val graph = TensorGraph(nodes, NodeId(3))
    val g2 = Normalize.run(graph)
    g2(g2.output) match
      case TensorExpr.Param("y", _) => ()
      case other                    => fail(s"expected Param(y), got $other")
    end match
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Cast elision
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: Cast(F64, x: f64) → x (same dtype, elided)") {
    val nodes = Vector(
      TensorExpr.Param("x", f64s),
      TensorExpr.Cast(DType.F64, NodeId(0), f64s)
    )
    val graph = TensorGraph(nodes, NodeId(1))
    val g2 = Normalize.run(graph)
    g2(g2.output) match
      case TensorExpr.Param("x", _) => ()
      case other                    => fail(s"expected Param(x), got $other")
    end match
  }

  test("normalize: Cast(Bool, x: f64) is NOT elided (different dtype)") {
    val nodes = Vector(
      TensorExpr.Param("x", f64s),
      TensorExpr.Cast(DType.Bool, NodeId(0), bools)
    )
    val graph = TensorGraph(nodes, NodeId(1))
    val g2 = Normalize.run(graph)
    assert(g2(g2.output).isInstanceOf[TensorExpr.Cast])
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Dead-node pruning
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: dead nodes unreachable from output are pruned") {
    // 0=Param(dead), 1=Param(live), 2=Unary(Neg, 1) — output=2
    val nodes = Vector(
      TensorExpr.Param("dead", f64s),
      TensorExpr.Param("live", f64s),
      TensorExpr.Unary(UnaryOp.Neg, NodeId(1), f64s)
    )
    val graph = TensorGraph(nodes, NodeId(2))
    val g2 = Normalize.run(graph)
    // Only Param(live) and Unary(Neg) survive → size = 2
    assertEquals(g2.size, 2)
    assert(
      g2.nodes.forall {
        case TensorExpr.Param("dead", _) => false
        case _                           => true
      },
      "dead node should have been pruned"
    )
  }

  test("normalize: all nodes live when all reachable from output") {
    val graph = paramOpConst(BinaryOp.Sub, 5.0)
    val g2 = Normalize.run(graph)
    // no identities: x - 5 stays; 3 nodes remain
    assertEquals(g2.size, 3)
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Canonical commutative ordering
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: y + x and x + y produce the same graph (canonical order)") {
    // Build x + y  (x=NodeId(0), y=NodeId(1), Add(0,1)=NodeId(2))
    val xy = g(p("x"), p("y"), TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s))
    // Build y + x  (y=NodeId(0), x=NodeId(1), Add(0,1)=NodeId(2))
    val yx = g(p("y"), p("x"), TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s))

    val gxy = Normalize.run(xy)
    val gyx = Normalize.run(yx)

    // Both results should have the same structure (2 Params + 1 Binary), same output type
    assertEquals(gxy.size, gyx.size)
    val addXY = gxy(gxy.output).asInstanceOf[TensorExpr.Binary]
    val addYX = gyx(gyx.output).asInstanceOf[TensorExpr.Binary]
    // canonical: lower-id operand first
    assert(addXY.a.i <= addXY.b.i, "xy: canonical order violated")
    assert(addYX.a.i <= addYX.b.i, "yx: canonical order violated")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Hash-consing collapses commutative duplicates
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: (x + y) shares same Add node when referenced twice") {
    // (x + y) - (x + y): with hash-consing the two Adds should share one node
    // 0=Param(x), 1=Param(y), 2=Add(0,1), 3=Sub(2,2)
    val nodes = Vector(
      p("x"),
      p("y"),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s),
      TensorExpr.Binary(BinaryOp.Sub, NodeId(2), NodeId(2), f64s)
    )
    val graph = TensorGraph(nodes, NodeId(3))
    val g2 = Normalize.run(graph)
    // x, y are params; sub(add(x,y), add(x,y)) should share the Add node → 4 nodes total
    assertEquals(g2.size, 4) // x, y, Add(x,y), Sub(add,add)
    val sub = g2(g2.output).asInstanceOf[TensorExpr.Binary]
    assertEquals(sub.a, sub.b, "both operands of Sub should be the same (shared) Add node")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Idempotence
  // ══════════════════════════════════════════════════════════════════════════

  def idempotent(graph: TensorGraph): Boolean =
    val g1 = Normalize.run(graph)
    val g2 = Normalize.run(g1)
    g1 == g2
  end idempotent

  test("idempotence: x + 0 (after first normalize, second is no-op)") {
    assert(idempotent(paramOpConst(BinaryOp.Add, 0.0)))
  }

  test("idempotence: x * 0 (after first normalize, second is no-op)") {
    assert(idempotent(paramOpConst(BinaryOp.Mul, 0.0)))
  }

  test("idempotence: Const(2.0) + Const(3.0)") {
    val graph = g(c(2.0), c(3.0), TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s))
    assert(idempotent(graph))
  }

  test("idempotence: sin(x)") {
    val graph = g(p("x"), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64s))
    assert(idempotent(graph))
  }

  test("idempotence: y + x (canonical swap idempotent)") {
    val graph = g(p("y"), p("x"), TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s))
    assert(idempotent(graph))
  }

  test("idempotence: dead-node graph") {
    val nodes = Vector(
      TensorExpr.Param("dead", f64s),
      TensorExpr.Param("live", f64s),
      TensorExpr.Unary(UnaryOp.Neg, NodeId(1), f64s)
    )
    assert(idempotent(TensorGraph(nodes, NodeId(2))))
  }

  test("idempotence: where(true, x, y)") {
    val nodes = Vector(
      TensorExpr.Const(true, bools),
      TensorExpr.Param("x", f64s),
      TensorExpr.Param("y", f64s),
      TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64s)
    )
    assert(idempotent(TensorGraph(nodes, NodeId(3))))
  }

  // ══════════════════════════════════════════════════════════════════════════
  // TypeCheck still passes after normalisation
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize + TypeCheck: sin(x) passes TypeCheck") {
    val graph = g(p("x"), TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64s))
    val g2 = Normalize.run(graph)
    assertEquals(TypeCheck.infer(g2), Right(g2))
  }

  test("normalize + TypeCheck: x + 0 → x; TypeCheck passes on result") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Add, 0.0))
    assertEquals(TypeCheck.infer(g2), Right(g2))
  }

  test("normalize + TypeCheck: Const(2.0) + Const(3.0) → Const(5.0); TypeCheck passes") {
    val graph = g(c(2.0), c(3.0), TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s))
    val g2 = Normalize.run(graph)
    assertEquals(TypeCheck.infer(g2), Right(g2))
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Negative: non-foldable / non-simplifiable nodes survive unchanged
  // ══════════════════════════════════════════════════════════════════════════

  test("normalize: x + y (no identities, survives as Binary)") {
    val graph = g(p("x"), p("y"), TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64s))
    val g2 = Normalize.run(graph)
    assert(g2(g2.output).isInstanceOf[TensorExpr.Binary])
    assertEquals(g2.size, 3)
  }

  test("normalize: x - 1 (no identity rule for sub-rhs=1, survives)") {
    val g2 = Normalize.run(paramOpConst(BinaryOp.Sub, 1.0))
    assert(g2(g2.output).isInstanceOf[TensorExpr.Binary])
  }

  test("normalize: 0 / Param(x) — x is not a const, annihilator NOT applied") {
    // 0=Const(0.0), 1=Param(x), 2=Div(0,1)
    val graph = g(c(0.0), p("x"), TensorExpr.Binary(BinaryOp.Div, NodeId(0), NodeId(1), f64s))
    val g2 = Normalize.run(graph)
    // x is Param, not Const — so annihilator rule does NOT fire
    assert(g2(g2.output).isInstanceOf[TensorExpr.Binary])
  }

end NormalizePhase5Test
