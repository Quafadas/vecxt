package vecxt.fusion

import munit.FunSuite

/** Phase-8 JVM executor tests.
  *
  * Every test either (a) directly verifies the output of `FusedRunner.eval` against expected values, or (b) compares
  * `FusedRunner.eval` to `Interpreter.eval` (the Phase-7 correctness oracle). Tolerance for F64 is 1e-12.
  */
class ExecutorPhase8Test extends FunSuite:

  private val eps = 1e-12

  // ── helpers ───────────────────────────────────────────────────────────────

  private def assertClose(actual: Double, expected: Double)(using munit.Location): Unit =
    assert(
      math.abs(actual - expected) <= eps,
      s"expected $expected, got $actual (diff = ${math.abs(actual - expected)})"
    )

  private def assertArrayClose(actual: Array[Double], expected: Array[Double])(using munit.Location): Unit =
    assertEquals(actual.length, expected.length, "array length mismatch")
    var i = 0
    while i < expected.length do
      assertClose(actual(i), expected(i))
      i += 1
    end while
  end assertArrayClose

  /** Run graph through FusedRunner and the reference Interpreter, assert they agree element-wise. */
  private def assertFusedMatchesInterp(
      graph:  TensorGraph,
      params: Map[String, Array[Double]]
  )(using munit.Location): Unit =
    val interpParams: Map[String, IVal] = params.map { (name, arr) =>
      val shape    = graph.nodes.collectFirst { case TensorExpr.Param(`name`, tpe) => tpe.shape }.getOrElse(Shape.scalar)
      val intShape = Schedule.knownShape(shape)
      val strides  = IVal.cmStrides(intShape)
      name -> IVal.F64(vecxt.ndarray.NDArray.wrap(arr, intShape, strides))
    }
    val interpResult = Interpreter.eval(graph, interpParams)
    val interpData = interpResult match
      case IVal.F64(nd) => nd.data
      case other        => fail(s"interpreter returned non-F64: $other")

    FusedRunner.eval(graph, params) match
      case Left(err)   => fail(s"FusedRunner failed: ${err.message}")
      case Right(data) => assertArrayClose(data, interpData)
  end assertFusedMatchesInterp

  // ── Elementwise parity ────────────────────────────────────────────────────

  test("parity: x + y elementwise") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(4)))
    val y = b.Expr.param[Double]("y", Shape(Dim.Known(4)))
    val g = b.build(x + y)

    val xd = Array(1.0, 2.0, 3.0, 4.0)
    val yd = Array(10.0, 20.0, 30.0, 40.0)

    FusedRunner.eval(g, Map("x" -> xd, "y" -> yd)) match
      case Left(err)   => fail(err.message)
      case Right(data) => assertArrayClose(data, Array(11.0, 22.0, 33.0, 44.0))

    /**
      * Ideally
      *
      * val x = NArray(1.0, 2.0, 3.0, 4.0)
      * val y = NArray(5.0, 6.0, 7.0, 8.0)
      *
      * val expected = x + y
      * assertArrayClose(data, expected.data)
      */
  }

  test("parity: sin(x) matches math.sin element-wise") {
    val xd = Array(0.0, math.Pi / 6, math.Pi / 4, math.Pi / 3, math.Pi / 2)

    val b = new GraphBuilder()
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))
    val g = b.build(x.sin)

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) => assertArrayClose(data, xd.map(math.sin))
  }

  test("parity: sin(x) + cos(x) — single fused group") {
    val xd = Array(0.0, 1.0, 2.0, 3.0)

    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))
    val g = b.build(x.sin + x.cos)

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) => assertArrayClose(data, xd.map(v => math.sin(v) + math.cos(v)))
  }

  test("parity: x * 2.0 + 1.0 — fused with constants") {
    val xd    = Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
    val xShape = Shape(Dim.Known(xd.length))

    val b = new GraphBuilder()
    import b.*
    val x   = b.Expr.param[Double]("x", xShape)
    val two = b.Expr.const[Double](2.0).broadcastTo(xShape)
    val one = b.Expr.const[Double](1.0).broadcastTo(xShape)
    val g   = b.build(x * two + one)

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) => assertArrayClose(data, xd.map(_ * 2.0 + 1.0))
  }

  test("parity: two-group — y=sin(x) materialised, z=exp(y)+y") {
    val xd = Array(0.0, 1.0, 2.0, 3.0)

    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))
    val y = x.sin         // refCount=2 => planner materialises y between groups
    val z = y.exp + y
    val g = b.build(z)

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) =>
        val expected = xd.map { v => val s = math.sin(v); math.exp(s) + s }
        assertArrayClose(data, expected)
  }

  test("parity: matches Interpreter — exp(x) * sin(x)") {
    val xd = Array(0.5, 1.0, 1.5, 2.0)
    val b  = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))
    val g = b.build(x.exp * x.sin)

    assertFusedMatchesInterp(g, Map("x" -> xd))
  }

  // ── FullReduce parity ─────────────────────────────────────────────────────

  test("parity: reduceSum(0) full reduce") {
    val xd = Array(1.0, 2.0, 3.0, 4.0, 5.0)

    val b = new GraphBuilder()
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))
    val g = b.build(x.reduceSum(0))

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) =>
        assertEquals(data.length, 1)
        assertClose(data(0), 15.0)
  }

  test("parity: sum(sin(x) + 1) — elementwise fused into reduce group") {
    val n  = 8
    val xd = Array.tabulate(n)(i => i.toDouble * 0.5)

    val b      = new GraphBuilder()
    import b.*
    val xShape = Shape(Dim.Known(n))
    val x      = b.Expr.param[Double]("x", xShape)
    val one    = b.Expr.const[Double](1.0).broadcastTo(xShape)
    val g      = b.build((x.sin + one).reduceSum(0))

    val expected = xd.map(v => math.sin(v) + 1.0).sum

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) =>
        assertEquals(data.length, 1)
        assertClose(data(0), expected)
  }

  test("parity: reduceProduct(0)") {
    val xd = Array(1.0, 2.0, 3.0, 4.0)

    val b = new GraphBuilder()
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))
    val g = b.build(x.reduceProduct(0))

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) =>
        assertEquals(data.length, 1)
        assertClose(data(0), 24.0)
  }

  test("parity: reduceMin and reduceMax") {
    val xd = Array(3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0, 6.0)

    val b = new GraphBuilder()
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))

    val gMin = b.build(x.reduceMin(0))
    val gMax = b.build(x.reduceMax(0))

    FusedRunner.eval(gMin, Map("x" -> xd)) match
      case Left(err)   => fail(s"min: ${err.message}")
      case Right(data) => assertClose(data(0), 1.0)

    FusedRunner.eval(gMax, Map("x" -> xd)) match
      case Left(err)   => fail(s"max: ${err.message}")
      case Right(data) => assertClose(data(0), 9.0)
  }

  test("parity: argMax(0) returns correct flat index") {
    val xd = Array(1.0, 5.0, 3.0, 2.0)

    val b = new GraphBuilder()
    val x = b.Expr.param[Double]("x", Shape(Dim.Known(xd.length)))
    val g = b.build(x.argMax(0))

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) =>
        assertEquals(data.length, 1)
        assertEquals(data(0), 1.0) // index 1 has value 5.0
  }

  // ── Normalisation round-trips ─────────────────────────────────────────────

  test("parity: x + 0 normalised away — result equals x") {
    val xd     = Array(1.0, 2.0, 3.0, 4.0)
    val xShape = Shape(Dim.Known(xd.length))

    val b = new GraphBuilder()
    import b.*
    val x    = b.Expr.param[Double]("x", xShape)
    val zero = b.Expr.const[Double](0.0).broadcastTo(xShape)
    val g    = b.build(x + zero)

    FusedRunner.eval(g, Map("x" -> xd)) match
      case Left(err)   => fail(err.message)
      case Right(data) => assertArrayClose(data, xd)
  }

  test("parity: constant folding — const-only graph") {
    val b = new GraphBuilder()
    import b.*
    val c1 = b.Expr.const[Double](3.0)
    val c2 = b.Expr.const[Double](4.0)
    val g  = b.build(c1 + c2)

    FusedRunner.eval(g, Map.empty) match
      case Left(err)   => fail(err.message)
      case Right(data) =>
        assertEquals(data.length, 1)
        assertClose(data(0), 7.0)
  }

  test("parity: matches Interpreter — sum(exp(x) * 2)") {
    val xd = Array(0.0, 0.5, 1.0, 1.5, 2.0)
    val b      = new GraphBuilder()
    import b.*
    val xShape = Shape(Dim.Known(xd.length))
    val x      = b.Expr.param[Double]("x", xShape)
    val two    = b.Expr.const[Double](2.0).broadcastTo(xShape)
    val g      = b.build((x.exp * two).reduceSum(0))

    assertFusedMatchesInterp(g, Map("x" -> xd))
  }

end ExecutorPhase8Test
