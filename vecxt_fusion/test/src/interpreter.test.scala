package vecxt.fusion

import munit.FunSuite

class InterpreterPhase7Test extends FunSuite:

  // ── tolerance ────────────────────────────────────────────────────────────

  private val eps = 1e-12

  private def assertClose(actual: Double, expected: Double)(using munit.Location): Unit =
    assert(
      math.abs(actual - expected) < eps,
      s"expected $expected, got $actual (diff = ${math.abs(actual - expected)})"
    )

  private def assertF64Close(result: IVal, expected: Array[Double])(using munit.Location): Unit =
    result match
      case IVal.F64(nd) =>
        val data = nd.data
        assertEquals(data.length, expected.length, "data length mismatch")
        var i = 0
        while i < expected.length do
          assertClose(data(i), expected(i))
          i += 1
        end while
      case other => fail(s"expected IVal.F64, got $other")

  private def assertBoolEq(result: IVal, expected: Array[Boolean])(using munit.Location): Unit =
    result match
      case IVal.Bool(nd) =>
        val data = nd.data
        assertEquals(data.length, expected.length)
        assertEquals(data.toSeq, expected.toSeq)
      case other => fail(s"expected IVal.Bool, got $other")

  private def assertI64Eq(result: IVal, expected: Array[Long])(using munit.Location): Unit =
    result match
      case IVal.I64(nd) =>
        val data = nd.data
        assertEquals(data.length, expected.length)
        assertEquals(data.toSeq, expected.toSeq)
      case other => fail(s"expected IVal.I64, got $other")

  private def assertShape(result: IVal, expected: Array[Int])(using munit.Location): Unit =
    assertEquals(result.shape.toSeq, expected.toSeq, "shape mismatch")

  // ── scalar arithmetic via Const ──────────────────────────────────────────

  test("scalar const: f64 value round-trips") {
    val b = new GraphBuilder()
    val c = b.Expr.const[Double](3.14)
    val g = b.build(c)
    val r = Interpreter.eval(g)
    assertF64Close(r, Array(3.14))
    assertShape(r, Array())
  }

  test("scalar const: two consts added") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.const[Double](2.0)
    val y = b.Expr.const[Double](3.0)
    val g = b.build(x + y)
    assertF64Close(Interpreter.eval(g), Array(5.0))
  }

  test("scalar const: negate") {
    val b = new GraphBuilder()
    val x = b.Expr.const[Double](7.0)
    val g = b.build(-x)
    assertF64Close(Interpreter.eval(g), Array(-7.0))
  }

  test("scalar param: add two params") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val g = b.build(x + y)
    val r = Interpreter.eval(
      g,
      params = Map("x" -> IVal.f64cm(Array(4.0), Array()), "y" -> IVal.f64cm(Array(6.0), Array()))
    )
    assertF64Close(r, Array(10.0))
  }

  // ── unary ops ────────────────────────────────────────────────────────────

  test("unary: exp of scalar param") {
    val b = new GraphBuilder()
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.exp)
    val r = Interpreter.eval(g, params = Map("x" -> IVal.f64cm(Array(1.0), Array())))
    assertF64Close(r, Array(math.E))
  }

  test("unary: sin of vector param") {
    val b = new GraphBuilder()
    val s4 = Shape(Dim.Known(4))
    val x = b.Expr.param[Double]("x", s4)
    val g = b.build(x.sin)
    val data = Array(0.0, math.Pi / 2, math.Pi, 3 * math.Pi / 2)
    val r = Interpreter.eval(g, params = Map("x" -> IVal.f64cm(data, Array(4))))
    assertShape(r, Array(4))
    assertF64Close(r, data.map(math.sin))
  }

  test("unary: sqrt") {
    val b = new GraphBuilder()
    val s3 = Shape(Dim.Known(3))
    val x = b.Expr.param[Double]("x", s3)
    val g = b.build(x.sqrt)
    val r = Interpreter.eval(g, params = Map("x" -> IVal.f64cm(Array(1.0, 4.0, 9.0), Array(3))))
    assertF64Close(r, Array(1.0, 2.0, 3.0))
  }

  test("unary: not on Bool") {
    val b = new GraphBuilder()
    val s3 = Shape(Dim.Known(3))
    val x = b.Expr.param[Boolean]("x", s3)
    val g = b.build(!x)
    val r = Interpreter.eval(g, params = Map("x" -> IVal.boolcm(Array(true, false, true), Array(3))))
    assertBoolEq(r, Array(false, true, false))
  }

  // ── binary ops ───────────────────────────────────────────────────────────

  test("binary: vector elementwise mul") {
    val b = new GraphBuilder()
    val s4 = Shape(Dim.Known(4))
    import b.*
    val x = b.Expr.param[Double]("x", s4)
    val y = b.Expr.param[Double]("y", s4)
    val g = b.build(x * y)
    val r = Interpreter.eval(
      g,
      params = Map(
        "x" -> IVal.f64cm(Array(1.0, 2.0, 3.0, 4.0), Array(4)),
        "y" -> IVal.f64cm(Array(10.0, 20.0, 30.0, 40.0), Array(4))
      )
    )
    assertF64Close(r, Array(10.0, 40.0, 90.0, 160.0))
  }

  test("binary: comparison produces Bool") {
    val b = new GraphBuilder()
    val s4 = Shape(Dim.Known(4))
    val x = b.Expr.param[Double]("x", s4)
    val y = b.Expr.param[Double]("y", s4)
    val g = b.build(x < y)
    val r = Interpreter.eval(
      g,
      params = Map(
        "x" -> IVal.f64cm(Array(1.0, 5.0, 3.0, 8.0), Array(4)),
        "y" -> IVal.f64cm(Array(2.0, 4.0, 3.0, 7.0), Array(4))
      )
    )
    assertBoolEq(r, Array(true, false, false, false))
  }

  test("binary: pow (elementwise x^y)") {
    val s3 = Shape(Dim.Known(3))
    val paramX = TensorExpr.Param("x", TType(DType.F64, s3))
    val paramY = TensorExpr.Param("y", TType(DType.F64, s3))
    val powN = TensorExpr.Binary(BinaryOp.Pow, NodeId(0), NodeId(1), TType(DType.F64, s3))
    val g = TensorGraph(Vector(paramX, paramY, powN), NodeId(2))
    val r = Interpreter.eval(
      g,
      params = Map(
        "x" -> IVal.f64cm(Array(2.0, 3.0, 4.0), Array(3)),
        "y" -> IVal.f64cm(Array(3.0, 2.0, 0.5), Array(3))
      )
    )
    assertF64Close(r, Array(8.0, 9.0, 2.0))
  }

  // ── cast ─────────────────────────────────────────────────────────────────

  test("cast: F64 → I64") {
    val paramX = TensorExpr.Param("x", TType(DType.F64, Shape.scalar))
    val castN = TensorExpr.Cast(DType.I64, NodeId(0), TType(DType.I64, Shape.scalar))
    val g = TensorGraph(Vector(paramX, castN), NodeId(1))
    val r = Interpreter.eval(g, params = Map("x" -> IVal.f64cm(Array(3.7), Array())))
    assertI64Eq(r, Array(3L))
  }

  test("cast: Bool → F64") {
    val s3 = Shape(Dim.Known(3))
    val paramX = TensorExpr.Param("x", TType(DType.Bool, s3))
    val castN = TensorExpr.Cast(DType.F64, NodeId(0), TType(DType.F64, s3))
    val g = TensorGraph(Vector(paramX, castN), NodeId(1))
    val r = Interpreter.eval(g, params = Map("x" -> IVal.boolcm(Array(true, false, true), Array(3))))
    assertF64Close(r, Array(1.0, 0.0, 1.0))
  }

  // ── BCast ─────────────────────────────────────────────────────────────────

  test("bcast: scalar → 1-D vector") {
    val b = new GraphBuilder()
    val s4 = Shape(Dim.Known(4))
    val scShape = Shape.scalar
    val x = b.Expr.param[Double]("x", scShape)
    // Wire a BCast node manually on top of the Param
    val baseG = b.build(x)
    val bcastN = TensorExpr.BCast(NodeId(0), s4, TType(DType.F64, s4))
    val g = TensorGraph(baseG.nodes :+ bcastN, NodeId(baseG.size))
    val r = Interpreter.eval(g, params = Map("x" -> IVal.f64cm(Array(5.0), Array())))
    assertShape(r, Array(4))
    assertF64Close(r, Array(5.0, 5.0, 5.0, 5.0))
  }

  test("bcast: 1-D [1] → [2,3] 2-D") {
    val b = new GraphBuilder()
    val s1 = Shape(Dim.Known(1))
    val s23 = Shape(Dim.Known(2), Dim.Known(3))
    val x = b.Expr.param[Double]("x", s1)
    val baseG = b.build(x)
    val bcastN = TensorExpr.BCast(NodeId(0), s23, TType(DType.F64, s23))
    val g = TensorGraph(baseG.nodes :+ bcastN, NodeId(baseG.size))
    val r = Interpreter.eval(g, params = Map("x" -> IVal.f64cm(Array(7.0), Array(1))))
    assertShape(r, Array(2, 3))
    // col-major [2,3]: indices [0..5] → all 7.0
    assertF64Close(r, Array.fill(6)(7.0))
  }

  // ── reduce ───────────────────────────────────────────────────────────────

  test("reduce: sum over all axis of 1-D vector") {
    val b = new GraphBuilder()
    val s4 = Shape(Dim.Known(4))
    val sOut = Shape.scalar
    val x = b.Expr.param[Double]("x", s4)
    val baseG = b.build(x)
    val redN = TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), TType(DType.F64, sOut))
    val g = TensorGraph(baseG.nodes :+ redN, NodeId(baseG.size))
    val r = Interpreter.eval(g, params = Map("x" -> IVal.f64cm(Array(1.0, 2.0, 3.0, 4.0), Array(4))))
    assertShape(r, Array())
    assertF64Close(r, Array(10.0))
  }

  test("reduce: sum along axis 0 of 2-D [2,3] → [3]") {
    val b = new GraphBuilder()
    val s23 = Shape(Dim.Known(2), Dim.Known(3))
    val s3 = Shape(Dim.Known(3))
    val x = b.Expr.param[Double]("x", s23)
    val baseG = b.build(x)
    val redN = TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), TType(DType.F64, s3))
    val g = TensorGraph(baseG.nodes :+ redN, NodeId(baseG.size))
    // col-major [2,3]: data(0)=(0,0), data(1)=(1,0), data(2)=(0,1), data(3)=(1,1), data(4)=(0,2), data(5)=(1,2)
    // sum along axis 0 (row axis): out(j) = data(0,j) + data(1,j)
    // out(0) = 1+2=3, out(1) = 3+4=7, out(2) = 5+6=11
    val r = Interpreter.eval(
      g,
      params = Map("x" -> IVal.f64cm(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3)))
    )
    assertShape(r, Array(3))
    assertF64Close(r, Array(3.0, 7.0, 11.0))
  }

  test("reduce: max along axis 1 of 2-D [2,3] → [2]") {
    val b = new GraphBuilder()
    val s23 = Shape(Dim.Known(2), Dim.Known(3))
    val s2 = Shape(Dim.Known(2))
    val x = b.Expr.param[Double]("x", s23)
    val baseG = b.build(x)
    val redN = TensorExpr.Reduce(ReduceOp.Max, NodeId(0), Vector(1), TType(DType.F64, s2))
    val g = TensorGraph(baseG.nodes :+ redN, NodeId(baseG.size))
    // col-major [2,3]: row0=[1,3,5], row1=[2,4,6]
    // max along axis 1 (col axis): out(0)=max(1,3,5)=5, out(1)=max(2,4,6)=6
    val r = Interpreter.eval(
      g,
      params = Map("x" -> IVal.f64cm(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Array(2, 3)))
    )
    assertShape(r, Array(2))
    assertF64Close(r, Array(5.0, 6.0))
  }

  test("reduce: empty axes is identity") {
    val b = new GraphBuilder()
    val s3 = Shape(Dim.Known(3))
    val x = b.Expr.param[Double]("x", s3)
    val baseG = b.build(x)
    val redN = TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector.empty, TType(DType.F64, s3))
    val g = TensorGraph(baseG.nodes :+ redN, NodeId(baseG.size))
    val inp = IVal.f64cm(Array(1.0, 2.0, 3.0), Array(3))
    val r = Interpreter.eval(g, params = Map("x" -> inp))
    assertF64Close(r, Array(1.0, 2.0, 3.0))
  }

  test("reduce: all on Bool vector") {
    val b = new GraphBuilder()
    val s3 = Shape(Dim.Known(3))
    val x = b.Expr.param[Boolean]("x", s3)
    val baseG = b.build(x)
    val redN = TensorExpr.Reduce(ReduceOp.All, NodeId(0), Vector(0), TType(DType.Bool, Shape.scalar))
    val g = TensorGraph(baseG.nodes :+ redN, NodeId(baseG.size))
    val r1 = Interpreter.eval(g, params = Map("x" -> IVal.boolcm(Array(true, true, true), Array(3))))
    assertBoolEq(r1, Array(true))
    val r2 = Interpreter.eval(g, params = Map("x" -> IVal.boolcm(Array(true, false, true), Array(3))))
    assertBoolEq(r2, Array(false))
  }

  test("reduce: argmax along axis 0 of 1-D") {
    val b = new GraphBuilder()
    val s4 = Shape(Dim.Known(4))
    val sOut = Shape.scalar
    val x = b.Expr.param[Double]("x", s4)
    val baseG = b.build(x)
    val redN = TensorExpr.Reduce(ReduceOp.ArgMax, NodeId(0), Vector(0), TType(DType.I64, sOut))
    val g = TensorGraph(baseG.nodes :+ redN, NodeId(baseG.size))
    val r = Interpreter.eval(
      g,
      params = Map("x" -> IVal.f64cm(Array(1.0, 5.0, 3.0, 2.0), Array(4)))
    )
    assertI64Eq(r, Array(1L)) // index of max (5.0)
  }

  // ── where ─────────────────────────────────────────────────────────────────

  test("where: elementwise select on F64") {
    val s4 = Shape(Dim.Known(4))
    val paramC = TensorExpr.Param("c", TType(DType.Bool, s4))
    val paramX = TensorExpr.Param("x", TType(DType.F64, s4))
    val paramY = TensorExpr.Param("y", TType(DType.F64, s4))
    val whereNode = TensorExpr.Where(NodeId(0), NodeId(1), NodeId(2), TType(DType.F64, s4))
    val g = TensorGraph(Vector(paramC, paramX, paramY, whereNode), NodeId(3))
    val r = Interpreter.eval(
      g,
      params = Map(
        "c" -> IVal.boolcm(Array(true, false, true, false), Array(4)),
        "x" -> IVal.f64cm(Array(10.0, 20.0, 30.0, 40.0), Array(4)),
        "y" -> IVal.f64cm(Array(1.0, 2.0, 3.0, 4.0), Array(4))
      )
    )
    assertF64Close(r, Array(10.0, 2.0, 30.0, 4.0))
  }

  // ── round-trip via Normalize ──────────────────────────────────────────────

  test("round-trip Normalize: x + 0 evaluates identically") {
    // Use scalar shapes so const(0.0) and param("x") share the same shape
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x") // scalar
    val zero = b.Expr.const[Double](0.0) // scalar
    val sum = x + zero
    val g = b.build(sum)
    val gNorm = Normalize.run(g)
    val params = Map("x" -> IVal.f64cm(Array(5.0), Array()))
    val r1 = Interpreter.eval(g, params)
    val r2 = Interpreter.eval(gNorm, params)
    assertF64Close(r1, Array(5.0))
    assertF64Close(r2, Array(5.0))
  }

  // ── round-trip via FusionPlanner ──────────────────────────────────────────

  test("round-trip FusionPlanner: graph unchanged by plan") {
    val b = new GraphBuilder()
    val s4 = Shape(Dim.Known(4))
    import b.*
    val x = b.Expr.param[Double]("x", s4)
    val y = b.Expr.param[Double]("y", s4)
    val result = (x + y).exp
    val g = b.build(result)
    val plan = FusionPlanner.plan(g)
    val inp = Map(
      "x" -> IVal.f64cm(Array(1.0, 0.0, -1.0, 2.0), Array(4)),
      "y" -> IVal.f64cm(Array(0.0, 1.0, 1.0, -2.0), Array(4))
    )
    // The plan's graph should evaluate to the same result
    val r1 = Interpreter.eval(g, inp)
    val r2 = Interpreter.eval(plan.graph, inp)
    assertF64Close(r1, Array(math.exp(1.0), math.exp(1.0), math.exp(0.0), math.exp(0.0)))
    assertF64Close(r2, r1.asInstanceOf[IVal.F64].nd.data)
  }

  // ── Lift nodes ───────────────────────────────────────────────────────────

  test("lift: read scalar NDArray via handle ID") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    import vecxt.ndarray.*
    val nd = NDArray.scalar[Double](42.0)
    val b = new GraphBuilder()
    val lifted = b.Expr.lift[Double](nd) // handle ID 0
    val g = b.build(lifted)
    val ival = IVal.fromNDArray(nd)
    val r = Interpreter.eval(g, lifts = Map(0 -> ival))
    assertF64Close(r, Array(42.0))
  }

  test("lift: 1-D NDArray via handle ID + elementwise op") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    import vecxt.ndarray.*
    val data = Array(1.0, 2.0, 3.0)
    val nd = NDArray.fromArray(data)
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.lift[Double](nd) // handle ID 0
    val g = b.build(x.exp)
    val ival = IVal.fromNDArray(nd)
    val r = Interpreter.eval(g, lifts = Map(0 -> ival))
    assertF64Close(r, data.map(math.exp))
  }

end InterpreterPhase7Test
