package vecxt.fusion

import munit.FunSuite

class TypeCheckPhase3Test extends FunSuite:

  // ─── helpers ────────────────────────────────────────────────────────────────

  val f64Scalar = TType(DType.F64, Shape.scalar)
  val boolScalar = TType(DType.Bool, Shape.scalar)
  val f64_3x4 = TType(DType.F64, Shape(Dim.Known(3), Dim.Known(4)))
  val f64_4 = TType(DType.F64, Shape(Dim.Known(4)))
  val bool_3x4 = TType(DType.Bool, Shape(Dim.Known(3), Dim.Known(4)))

  def nodeAt[T <: TensorExpr](g: TensorGraph, i: Int)(using scala.reflect.ClassTag[T]): T =
    g.nodes(i) match
      case t: T  => t
      case other =>
        fail(s"expected ${scala.reflect.classTag[T].runtimeClass.getSimpleName} at index $i, got $other")

  // ══════════════════════════════════════════════════════════════════════════
  // Shape.broadcast
  // ══════════════════════════════════════════════════════════════════════════

  test("Shape.broadcast: equal shapes → same shape") {
    val s = Shape(Dim.Known(3), Dim.Known(4))
    assertEquals(Shape.broadcast(s, s), Right(s))
  }

  test("Shape.broadcast: scalar + [3,4] → [3,4]") {
    val result = Shape.broadcast(Shape.scalar, Shape(Dim.Known(3), Dim.Known(4)))
    assertEquals(result, Right(Shape(Dim.Known(3), Dim.Known(4))))
  }

  test("Shape.broadcast: [3,4] + scalar → [3,4]") {
    val result = Shape.broadcast(Shape(Dim.Known(3), Dim.Known(4)), Shape.scalar)
    assertEquals(result, Right(Shape(Dim.Known(3), Dim.Known(4))))
  }

  test("Shape.broadcast: [1,4] + [3,4] → [3,4]") {
    val a = Shape(Dim.Known(1), Dim.Known(4))
    val b = Shape(Dim.Known(3), Dim.Known(4))
    assertEquals(Shape.broadcast(a, b), Right(b))
  }

  test("Shape.broadcast: [4] + [3,4] → [3,4] (right-aligned padding)") {
    val a = Shape(Dim.Known(4))
    val b = Shape(Dim.Known(3), Dim.Known(4))
    assertEquals(Shape.broadcast(a, b), Right(b))
  }

  test("Shape.broadcast: [3,4] + [3,2] → ShapeError (mismatch at axis 1)") {
    val result = Shape.broadcast(Shape(Dim.Known(3), Dim.Known(4)), Shape(Dim.Known(3), Dim.Known(2)))
    assert(result.isLeft, s"expected Left, got $result")
    result.left.foreach(e => assert(e.message.contains("mismatch"), s"unexpected error: ${e.message}"))
  }

  test("Shape.broadcast: Sym(s) + Sym(s) → Sym(s) (same name unified)") {
    val s = Dim.Sym("batch")
    val result = Shape.broadcast(Shape(s, Dim.Known(4)), Shape(s, Dim.Known(4)))
    assertEquals(result, Right(Shape(Dim.Sym("batch"), Dim.Known(4))))
  }

  test("Shape.broadcast: Sym(s) + Known(1) → Sym(s)") {
    val result = Shape.broadcast(Shape(Dim.Sym("batch")), Shape(Dim.Known(1)))
    assertEquals(result, Right(Shape(Dim.Sym("batch"))))
  }

  test("Shape.broadcast: Unknown + anything → Unknown") {
    val result = Shape.broadcast(Shape(Dim.Unknown), Shape(Dim.Known(5)))
    assertEquals(result, Right(Shape(Dim.Unknown)))
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Builder: explicit BCast via .broadcastTo
  // ══════════════════════════════════════════════════════════════════════════

  test("builder broadcastTo: scalar .broadcastTo([3,4]) inserts BCast node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val s = b.Expr.const[Double](1.0)
    val sBig = s.broadcastTo(Shape(Dim.Known(3), Dim.Known(4)))
    val result = x + sBig
    val g = b.build(result)
    // Nodes: x=0, s=1, BCast(s,[3,4])=2, Binary(Add,0,2)=3
    assertEquals(g.size, 4)
    val bcast = nodeAt[TensorExpr.BCast](g, 2)
    assertEquals(bcast.a, NodeId(1))
    assertEquals(bcast.to, Shape(Dim.Known(3), Dim.Known(4)))
    assertEquals(bcast.tpe.dtype, DType.F64)
    val add = nodeAt[TensorExpr.Binary](g, 3)
    assertEquals(add.op, BinaryOp.Add)
    assertEquals(add.a, NodeId(0))
    assertEquals(add.b, NodeId(2))
    assertEquals(add.tpe.shape, Shape(Dim.Known(3), Dim.Known(4)))
  }

  test("builder broadcastTo: same shape returns same Expr (no BCast node)") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val y = x.broadcastTo(Shape(Dim.Known(3), Dim.Known(4))) // no-op
    val g = b.build(y)
    assertEquals(g.size, 1) // only the Param node
    assert(!g.nodes.exists(_.isInstanceOf[TensorExpr.BCast]))
  }

  test("builder broadcastTo: [1,4] → [3,4] inserts BCast node") {
    val b = new GraphBuilder()
    import b.*
    val bias = b.Expr.param[Double]("bias", Shape(Dim.Known(1), Dim.Known(4)))
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val biasBig = bias.broadcastTo(Shape(Dim.Known(3), Dim.Known(4)))
    val result = x + biasBig
    val g = b.build(result)
    assert(g.nodes.exists(_.isInstanceOf[TensorExpr.BCast]), "expected a BCast node")
    val bcast = g.nodes.collectFirst { case b: TensorExpr.BCast => b }.get
    assertEquals(bcast.to, Shape(Dim.Known(3), Dim.Known(4)))
  }

  test("builder broadcastTo: incompatible target shape throws IllegalArgumentException") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    intercept[IllegalArgumentException] {
      x.broadcastTo(Shape(Dim.Known(5), Dim.Known(4)))
    }
  }

  test("builder binary: different shapes (no explicit broadcast) throws IllegalArgumentException") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val y = b.Expr.param[Double]("y", Shape(Dim.Known(5), Dim.Known(4)))
    intercept[IllegalArgumentException] {
      x + y
    }
  }

  test("builder binary: equal shapes, no BCast inserted") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val y = b.Expr.param[Double]("y", Shape(Dim.Known(3), Dim.Known(4)))
    val result = x + y
    val g = b.build(result)
    // Nodes: x=0, y=1, Binary(Add,0,1)=2
    assertEquals(g.size, 3)
    assert(!g.nodes.exists(_.isInstanceOf[TensorExpr.BCast]), "no BCast expected when shapes match")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // TypeCheck.infer — well-typed graphs
  // ══════════════════════════════════════════════════════════════════════════

  test("TypeCheck: trivial Param graph is well-typed") {
    val g = TensorGraph(Vector(TensorExpr.Param("x", f64Scalar)), NodeId(0))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Const node is well-typed") {
    val g = TensorGraph(Vector(TensorExpr.Const(3.14, f64Scalar)), NodeId(0))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Unary(Sin) on f64 scalar is well-typed") {
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar),
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), f64Scalar)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Binary(Add) on equal-shape f64 tensors is well-typed") {
    val nodes = Vector(
      TensorExpr.Param("x", f64_3x4),
      TensorExpr.Param("y", f64_3x4),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64_3x4)
    )
    val g = TensorGraph(nodes, NodeId(2))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Cast(F32, f64) preserves shape, changes dtype") {
    val f32Scalar = TType(DType.F32, Shape.scalar)
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar),
      TensorExpr.Cast(DType.F32, NodeId(0), f32Scalar)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Cast Bool → F64 is allowed") {
    val f64Sc = TType(DType.F64, Shape.scalar)
    val nodes = Vector(
      TensorExpr.Param("flag", boolScalar),
      TensorExpr.Cast(DType.F64, NodeId(0), f64Sc)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Cast F64 → Bool is allowed") {
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar),
      TensorExpr.Cast(DType.Bool, NodeId(0), boolScalar)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Cast round-trip F64 → Bool → F64 is allowed") {
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar), // 0
      TensorExpr.Cast(DType.Bool, NodeId(0), boolScalar), // 1
      TensorExpr.Cast(DType.F64, NodeId(1), f64Scalar) // 2
    )
    val g = TensorGraph(nodes, NodeId(2))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  // ─── golden: sum(x: f64[3,4], axes=[0]) → f64[4] ─────────────────────────

  test("TypeCheck golden: Reduce(Sum) on f64[3,4] along axis 0 → f64[4]") {
    val nodes = Vector(
      TensorExpr.Param("x", f64_3x4),
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), f64_4)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
    assertEquals(g(NodeId(1)).tpe, f64_4)
  }

  test("TypeCheck: Reduce(ArgMax) on f64[3,4] along axis 0 → i64[4]") {
    val i64_4 = TType(DType.I64, Shape(Dim.Known(4)))
    val nodes = Vector(
      TensorExpr.Param("x", f64_3x4),
      TensorExpr.Reduce(ReduceOp.ArgMax, NodeId(0), Vector(0), i64_4)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Reduce(All) on bool[3,4] along both axes → bool[]") {
    val boolSc = TType(DType.Bool, Shape.scalar)
    val nodes = Vector(
      TensorExpr.Param("mask", bool_3x4),
      TensorExpr.Reduce(ReduceOp.All, NodeId(0), Vector(0, 1), boolSc)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: BCast scalar to [3,4] is well-typed") {
    val nodes = Vector(
      TensorExpr.Param("s", f64Scalar),
      TensorExpr.BCast(NodeId(0), Shape(Dim.Known(3), Dim.Known(4)), f64_3x4)
    )
    val g = TensorGraph(nodes, NodeId(1))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: Where(bool[3,4], f64[3,4], f64[3,4]) is well-typed") {
    val nodes = Vector(
      TensorExpr.Param("cond", bool_3x4),
      TensorExpr.Param("x", f64_3x4),
      TensorExpr.Param("y", f64_3x4),
      TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64_3x4)
    )
    val g = TensorGraph(nodes, NodeId(3))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("TypeCheck: comparison Binary produces Bool output") {
    val nodes = Vector(
      TensorExpr.Param("x", f64_3x4),
      TensorExpr.Param("y", f64_3x4),
      TensorExpr.Binary(BinaryOp.Lt, NodeId(0), NodeId(1), bool_3x4)
    )
    val g = TensorGraph(nodes, NodeId(2))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  // ══════════════════════════════════════════════════════════════════════════
  // TypeCheck.infer — negative / error cases
  // ══════════════════════════════════════════════════════════════════════════

  test("TypeCheck negative: Binary shape mismatch → ShapeMismatch at offending node") {
    val f64_5 = TType(DType.F64, Shape(Dim.Known(5)))
    val nodes = Vector(
      TensorExpr.Param("x", f64_4), // [4]
      TensorExpr.Param("y", f64_5), // [5]  — different shape, no BCast
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64_4)
    )
    val g = TensorGraph(nodes, NodeId(2))
    TypeCheck.infer(g) match
      case Left(e: TypeError.ShapeMismatch) =>
        assertEquals(e.at, NodeId(2))
        assert(e.message.nonEmpty)
      case other => fail(s"expected ShapeMismatch, got $other")
    end match
  }

  test("TypeCheck negative: Not(f64) → DTypeMismatch") {
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar),
      TensorExpr.Unary(UnaryOp.Not, NodeId(0), boolScalar)
    )
    val g = TensorGraph(nodes, NodeId(1))
    TypeCheck.infer(g) match
      case Left(e: TypeError.DTypeMismatch) =>
        assertEquals(e.at, NodeId(1))
      case other => fail(s"expected DTypeMismatch, got $other")
    end match
  }

  test("TypeCheck negative: And(f64, f64) → DTypeMismatch (must be Bool)") {
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar),
      TensorExpr.Param("y", f64Scalar),
      TensorExpr.Binary(BinaryOp.And, NodeId(0), NodeId(1), boolScalar)
    )
    val g = TensorGraph(nodes, NodeId(2))
    TypeCheck.infer(g) match
      case Left(e: TypeError.DTypeMismatch) =>
        assertEquals(e.at, NodeId(2))
      case other => fail(s"expected DTypeMismatch, got $other")
    end match
  }

  test("TypeCheck negative: Reduce axis out of rank → InvalidAxes") {
    val nodes = Vector(
      TensorExpr.Param("x", f64_4), // rank 1
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(2), f64Scalar) // axis 2 out of range
    )
    val g = TensorGraph(nodes, NodeId(1))
    TypeCheck.infer(g) match
      case Left(e: TypeError.InvalidAxes) =>
        assertEquals(e.at, NodeId(1))
      case other => fail(s"expected InvalidAxes, got $other")
    end match
  }

  test("TypeCheck negative: Where condition not Bool → DTypeMismatch") {
    val nodes = Vector(
      TensorExpr.Param("cond", f64Scalar), // should be Bool
      TensorExpr.Param("x", f64Scalar),
      TensorExpr.Param("y", f64Scalar),
      TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64Scalar)
    )
    val g = TensorGraph(nodes, NodeId(3))
    TypeCheck.infer(g) match
      case Left(e: TypeError.DTypeMismatch) =>
        assertEquals(e.at, NodeId(3))
      case other => fail(s"expected DTypeMismatch, got $other")
    end match
  }

  test("TypeCheck negative: Where branches with different shapes → ShapeMismatch") {
    val f64_5 = TType(DType.F64, Shape(Dim.Known(5)))
    val nodes = Vector(
      TensorExpr.Param("cond", boolScalar),
      TensorExpr.Param("x", f64_4),
      TensorExpr.Param("y", f64_5),
      TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), f64_4)
    )
    val g = TensorGraph(nodes, NodeId(3))
    TypeCheck.infer(g) match
      case Left(e: TypeError.ShapeMismatch) =>
        assertEquals(e.at, NodeId(3))
      case other => fail(s"expected ShapeMismatch, got $other")
    end match
  }

  test("TypeCheck negative: stored type disagrees with inferred → DTypeMismatch") {
    // Binary(Add, f64, f64) stored as f32 — wrong stored type
    val f32Scalar = TType(DType.F32, Shape.scalar)
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar),
      TensorExpr.Param("y", f64Scalar),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f32Scalar) // wrong stored type
    )
    val g = TensorGraph(nodes, NodeId(2))
    TypeCheck.infer(g) match
      case Left(e: TypeError.DTypeMismatch) =>
        assertEquals(e.at, NodeId(2))
      case other => fail(s"expected DTypeMismatch, got $other")
    end match
  }

  test("TypeCheck negative: BCast to incompatible shape → ShapeMismatch") {
    // [3,4] cannot be broadcast to [5,4]
    val f64_5x4 = TType(DType.F64, Shape(Dim.Known(5), Dim.Known(4)))
    val nodes = Vector(
      TensorExpr.Param("x", f64_3x4),
      TensorExpr.BCast(NodeId(0), Shape(Dim.Known(5), Dim.Known(4)), f64_5x4)
    )
    val g = TensorGraph(nodes, NodeId(1))
    TypeCheck.infer(g) match
      case Left(_: TypeError.ShapeMismatch) => () // expected
      case other                            => fail(s"expected ShapeMismatch, got $other")
    end match
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Builder: reduce and cast methods
  // ══════════════════════════════════════════════════════════════════════════

  test("builder: reduceSum on [3,4] along axis 0 → [4], TypeCheck passes") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val s = x.reduceSum(0)
    val g = b.build(s)
    assertEquals(TypeCheck.infer(g), Right(g))
    assertEquals(g(g.output).tpe, TType(DType.F64, Shape(Dim.Known(4))))
  }

  test("builder: argMax on [3,4] along axis 1 → [3] with I64 dtype") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val am: b.Expr[Long] = x.argMax(1)
    val g = b.build(am)
    assertEquals(TypeCheck.infer(g), Right(g))
    assertEquals(g(g.output).tpe, TType(DType.I64, Shape(Dim.Known(3))))
  }

  test("builder: castTo[Float] preserves shape, changes dtype") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(5)))
    val y: b.Expr[Float] = x.castTo[Float]
    val g = b.build(y)
    assertEquals(TypeCheck.infer(g), Right(g))
    assertEquals(g(g.output).tpe.dtype, DType.F32)
    assertEquals(g(g.output).tpe.shape, Shape(Dim.Known(5)))
  }

  test("builder: where selects correctly between two tensors") {
    val b = new GraphBuilder()
    import b.*
    val cond = b.Expr.param[Boolean]("cond", Shape(Dim.Known(4)))
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(4)))
    val y = b.Expr.param[Double]("y", Shape(Dim.Known(4)))
    val out = b.where(cond, x, y)
    val g = b.build(out)
    assertEquals(TypeCheck.infer(g), Right(g))
    assertEquals(g(g.output).tpe, TType(DType.F64, Shape(Dim.Known(4))))
  }

  test("builder: full graph TypeCheck after explicit tensor+scalar broadcast") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(3), Dim.Known(4)))
    val s = b.Expr.const[Double](2.0)
    val sBig = s.broadcastTo(Shape(Dim.Known(3), Dim.Known(4)))
    val result = (x + sBig).sin
    val g = b.build(result)
    assertEquals(TypeCheck.infer(g), Right(g))
  }

end TypeCheckPhase3Test
