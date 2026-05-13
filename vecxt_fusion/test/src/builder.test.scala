package vecxt.fusion

import munit.FunSuite
import scala.compiletime.testing.typeChecks

class BuilderPhase2Test extends FunSuite:

  // ─── helpers ────────────────────────────────────────────────────────────────

  /** Retrieve a node by index from a graph and cast to the expected type. */
  def nodeAt[T <: TensorExpr](g: TensorGraph, i: Int)(using scala.reflect.ClassTag[T]): T =
    g.nodes(i) match
      case t: T => t
      case other => fail(s"expected ${scala.reflect.classTag[T].runtimeClass.getSimpleName} at index $i, got $other")

  // ─── hash-consing: x + x shares the Param node ───────────────────────────

  test("hash-consing: x + x produces one Param and one Binary") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val sum = x + x
    val g = b.build(sum)
    // nodes: Param("x") at 0, Binary(Add, 0, 0) at 1
    assertEquals(g.size, 2)
    val p = nodeAt[TensorExpr.Param](g, 0)
    assertEquals(p.name, "x")
    val bin = nodeAt[TensorExpr.Binary](g, 1)
    assertEquals(bin.op, BinaryOp.Add)
    assertEquals(bin.a, NodeId(0))
    assertEquals(bin.b, NodeId(0))
  }

  // ─── hash-consing: (x+y)*(x+y) shares the Add node ──────────────────────

  test("hash-consing: (x+y)*(x+y) shares the Add node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val add = x + y
    val prod = add * add
    val g = b.build(prod)
    // nodes: x=0, y=1, add=2, mul=3 — NOT x=0, y=1, add1=2, add2=3, mul=4
    assertEquals(g.size, 4)
    val mul = nodeAt[TensorExpr.Binary](g, 3)
    assertEquals(mul.op, BinaryOp.Mul)
    assertEquals(mul.a, NodeId(2))
    assertEquals(mul.b, NodeId(2))
  }

  // ─── hash-consing: identical Param nodes are shared ──────────────────────

  test("hash-consing: creating the same Param twice returns the same NodeId") {
    val b = new GraphBuilder()
    val x1 = b.Expr.param[Double]("x")
    val x2 = b.Expr.param[Double]("x")
    assertEquals(x1, x2)
    val g = b.build(x1)
    assertEquals(g.size, 1)
  }

  // ─── hash-consing: different names are distinct nodes ────────────────────

  test("hash-consing: Param nodes with different names are distinct") {
    val b = new GraphBuilder()
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    assertNotEquals(x, y)
  }

  // ─── hash-consing: identical Const nodes are shared ──────────────────────

  test("hash-consing: Const(1.0) created twice is one node") {
    val b = new GraphBuilder()
    val c1 = b.Expr.const[Double](1.0)
    val c2 = b.Expr.const[Double](1.0)
    assertEquals(c1, c2)
    val g = b.build(c1)
    assertEquals(g.size, 1)
  }

  // ─── arithmetic operations ────────────────────────────────────────────────

  test("arithmetic: x - y inserts Sub node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val diff = x - y
    val g = b.build(diff)
    assertEquals(g.size, 3)
    val node = nodeAt[TensorExpr.Binary](g, 2)
    assertEquals(node.op, BinaryOp.Sub)
  }

  test("arithmetic: x * y inserts Mul node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val prod = x * y
    val g = b.build(prod)
    val node = nodeAt[TensorExpr.Binary](g, 2)
    assertEquals(node.op, BinaryOp.Mul)
  }

  test("arithmetic: x / y inserts Div node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val quot = x / y
    val g = b.build(quot)
    val node = nodeAt[TensorExpr.Binary](g, 2)
    assertEquals(node.op, BinaryOp.Div)
  }

  test("arithmetic: unary_- inserts Neg node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val neg = -x
    val g = b.build(neg)
    assertEquals(g.size, 2)
    val node = nodeAt[TensorExpr.Unary](g, 1)
    assertEquals(node.op, UnaryOp.Neg)
    assertEquals(node.a, NodeId(0))
  }

  // ─── comparison operations ────────────────────────────────────────────────

  test("comparison: x < y produces Expr[Boolean] with Bool dtype") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val cmp: b.Expr[Boolean] = x < y
    val g = b.build(cmp)
    val node = nodeAt[TensorExpr.Binary](g, 2)
    assertEquals(node.op, BinaryOp.Lt)
    assertEquals(node.tpe.dtype, DType.Bool)
  }

  test("comparison: <= produces Lte node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val g = b.build(x <= y)
    assertEquals(nodeAt[TensorExpr.Binary](g, 2).op, BinaryOp.Lte)
  }

  test("comparison: > produces Gt node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val g = b.build(x > y)
    assertEquals(nodeAt[TensorExpr.Binary](g, 2).op, BinaryOp.Gt)
  }

  test("comparison: >= produces Gte node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val g = b.build(x >= y)
    assertEquals(nodeAt[TensorExpr.Binary](g, 2).op, BinaryOp.Gte)
  }

  test("comparison: === produces Eq node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val g = b.build(x === y)
    assertEquals(nodeAt[TensorExpr.Binary](g, 2).op, BinaryOp.Eq)
  }

  test("comparison: =!= produces Neq node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val g = b.build(x =!= y)
    assertEquals(nodeAt[TensorExpr.Binary](g, 2).op, BinaryOp.Neq)
  }

  // ─── unary math operations ────────────────────────────────────────────────

  test("unary: sin inserts Sin node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.sin)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Sin)
  }

  test("unary: cos inserts Cos node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.cos)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Cos)
  }

  test("unary: exp inserts Exp node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.exp)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Exp)
  }

  test("unary: log inserts Log node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.log)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Log)
  }

  test("unary: sqrt inserts Sqrt node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.sqrt)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Sqrt)
  }

  test("unary: abs inserts Abs node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.abs)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Abs)
  }

  test("unary: reciprocal inserts Reciprocal node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.reciprocal)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Reciprocal)
  }

  test("unary: tan inserts Tan node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val g = b.build(x.tan)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Tan)
  }

  // ─── boolean operations ───────────────────────────────────────────────────

  test("boolean: && produces And node") {
    val b = new GraphBuilder()
    import b.*
    val p = b.Expr.param[Boolean]("p")
    val q = b.Expr.param[Boolean]("q")
    val g = b.build(p && q)
    assertEquals(nodeAt[TensorExpr.Binary](g, 2).op, BinaryOp.And)
  }

  test("boolean: || produces Or node") {
    val b = new GraphBuilder()
    import b.*
    val p = b.Expr.param[Boolean]("p")
    val q = b.Expr.param[Boolean]("q")
    val g = b.build(p || q)
    assertEquals(nodeAt[TensorExpr.Binary](g, 2).op, BinaryOp.Or)
  }

  test("boolean: unary_! produces Not node") {
    val b = new GraphBuilder()
    import b.*
    val p = b.Expr.param[Boolean]("p")
    val g = b.build(!p)
    assertEquals(nodeAt[TensorExpr.Unary](g, 1).op, UnaryOp.Not)
  }

  // ─── output node identity ─────────────────────────────────────────────────

  test("build: output NodeId matches result expression") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val result = x + y
    val g = b.build(result)
    assertEquals(g.output, NodeId(2))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Add)
  }

  // ─── type shape (rank) preserved ─────────────────────────────────────────

  test("shape: scalar param + scalar param produces scalar output type") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val y = b.Expr.param[Double]("y")
    val g = b.build(x + y)
    assertEquals(g(g.output).tpe.shape.rank, 0)
    assert(g(g.output).tpe.shape.isScalar)
  }

  test("shape: tensor param preserves shape through unary op") {
    val b = new GraphBuilder()
    import b.*
    val shape = Shape(Dim.Known(3), Dim.Known(4))
    val x = b.Expr.param[Double]("x", shape)
    val g = b.build(x.sin)
    assertEquals(g(g.output).tpe.shape, shape)
  }

  test("shape: comparison output has Bool dtype and same shape as input") {
    val b = new GraphBuilder()
    import b.*
    val shape = Shape(Dim.Known(5))
    val x = b.Expr.param[Double]("x", shape)
    val y = b.Expr.param[Double]("y", shape)
    val g = b.build(x < y)
    val out = g(g.output)
    assertEquals(out.tpe.dtype, DType.Bool)
    assertEquals(out.tpe.shape, shape)
  }

  // ─── complex expression: sin(x) + cos(x) ─────────────────────────────────

  test("complex: sin(x) + cos(x) shares one Param node") {
    val b = new GraphBuilder()
    import b.*
    val x = b.Expr.param[Double]("x")
    val result = x.sin + x.cos
    val g = b.build(result)
    // nodes: x=0, sin=1, cos=2, add=3
    assertEquals(g.size, 4)
    val sinNode = nodeAt[TensorExpr.Unary](g, 1)
    val cosNode = nodeAt[TensorExpr.Unary](g, 2)
    assertEquals(sinNode.a, NodeId(0))
    assertEquals(cosNode.a, NodeId(0))
  }

  // ─── type-level tests ─────────────────────────────────────────────────────

  test("type-level: Expr[Double] + Expr[Int] does not compile") {
    assert(
      !typeChecks("val b = new vecxt.fusion.GraphBuilder(); import b.*; val x = b.Expr.param[Double](\"x\"); val y = b.Expr.param[Int](\"y\"); x + y"),
      "expected Expr[Double] + Expr[Int] to be a compile error"
    )
  }

  test("type-level: Expr[Double] < Expr[Double] compiles and returns Expr[Boolean]") {
    assert(
      typeChecks("val b = new vecxt.fusion.GraphBuilder(); import b.*; val x = b.Expr.param[Double](\"x\"); val y = b.Expr.param[Double](\"y\"); val r: b.Expr[Boolean] = x < y"),
      "expected Expr[Double] < Expr[Double]: Expr[Boolean] to compile"
    )
  }

  test("type-level: Expr[Boolean] has no numeric + operator") {
    assert(
      !typeChecks("val b = new vecxt.fusion.GraphBuilder(); import b.*; val p = b.Expr.param[Boolean](\"p\"); val q = b.Expr.param[Boolean](\"q\"); p + q"),
      "expected Expr[Boolean] + Expr[Boolean] to be a compile error"
    )
  }

  test("type-level: Expr[Boolean] supports && operator") {
    assert(
      typeChecks("val b = new vecxt.fusion.GraphBuilder(); import b.*; val p = b.Expr.param[Boolean](\"p\"); val q = b.Expr.param[Boolean](\"q\"); val r: b.Expr[Boolean] = p && q"),
      "expected Expr[Boolean] && Expr[Boolean]: Expr[Boolean] to compile"
    )
  }

  test("type-level: Float params support numeric operations") {
    assert(
      typeChecks("val b = new vecxt.fusion.GraphBuilder(); import b.*; val x = b.Expr.param[Float](\"x\"); val y = b.Expr.param[Float](\"y\"); val r: b.Expr[Float] = x + y"),
      "expected Float numeric ops to compile"
    )
  }

end BuilderPhase2Test
