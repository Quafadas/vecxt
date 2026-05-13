package vecxt.fusion

import munit.FunSuite

class IrPhase1Test extends FunSuite:

  // ─── helpers ────────────────────────────────────────────────────────────────

  val f64Scalar = TType(DType.F64, Shape.scalar)
  val boolScalar = TType(DType.Bool, Shape.scalar)

  /** Build the canonical small test graph: add = x + y, out = sin(add). */
  def addSinGraph: TensorGraph =
    val nodes = Vector(
      TensorExpr.Param("x", f64Scalar), // 0
      TensorExpr.Param("y", f64Scalar), // 1
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64Scalar), // 2
      TensorExpr.Unary(UnaryOp.Sin, NodeId(2), f64Scalar) // 3
    )
    TensorGraph(nodes, NodeId(3))
  end addSinGraph

  // ─── graph structure ────────────────────────────────────────────────────────

  test("graph: node count and output id") {
    val g = addSinGraph
    assertEquals(g.size, 4)
    assertEquals(g.output, NodeId(3))
  }

  test("graph: output node is Unary(Sin, ...)") {
    val g = addSinGraph
    assert(g(NodeId(3)).isInstanceOf[TensorExpr.Unary], "expected Unary output node")
    val u = g(NodeId(3)).asInstanceOf[TensorExpr.Unary]
    assertEquals(u.op, UnaryOp.Sin)
    assertEquals(u.a, NodeId(2))
  }

  test("graph: Binary add node at index 2") {
    val g = addSinGraph
    assert(g(NodeId(2)).isInstanceOf[TensorExpr.Binary], "expected Binary node at index 2")
    val b = g(NodeId(2)).asInstanceOf[TensorExpr.Binary]
    assertEquals(b.op, BinaryOp.Add)
    assertEquals(b.a, NodeId(0))
    assertEquals(b.b, NodeId(1))
  }

  // ─── toString round-trip ────────────────────────────────────────────────────

  test("graph.toString: contains expected tokens for add+sin graph") {
    val s = addSinGraph.toString
    assert(s.startsWith("graph {"), s"missing 'graph {': $s")
    assert(s.contains("""Param("x")"""), s"""missing Param("x"): $s""")
    assert(s.contains("""Param("y")"""), s"""missing Param("y"): $s""")
    assert(s.contains("Binary(Add"), s"missing Binary(Add: $s")
    assert(s.contains("Unary(Sin"), s"missing Unary(Sin: $s")
    assert(s.contains("output: 3"), s"missing output: 3: $s")
  }

  test("graph.toString: Const value round-trips") {
    val nodes = Vector(
      TensorExpr.Param("a", f64Scalar),
      TensorExpr.Const(1.5, f64Scalar),
      TensorExpr.Binary(BinaryOp.Add, NodeId(0), NodeId(1), f64Scalar)
    )
    val g = TensorGraph(nodes, NodeId(2))
    val s = g.toString
    assert(s.contains("Const(1.5)"), s"missing Const(1.5): $s")
    assert(s.contains("output: 2"), s"missing output: 2: $s")
  }

  // ─── Shape ──────────────────────────────────────────────────────────────────

  test("Shape: structural equality") {
    val s1 = Shape(Dim.Known(3), Dim.Known(4))
    val s2 = Shape(Dim.Known(3), Dim.Known(4))
    assertEquals(s1, s2)
  }

  test("Shape: hashCode consistent with equals") {
    val s1 = Shape(Dim.Known(3), Dim.Known(4))
    val s2 = Shape(Dim.Known(3), Dim.Known(4))
    assertEquals(s1.hashCode, s2.hashCode)
  }

  test("Shape: unequal shapes are not equal") {
    val s1 = Shape(Dim.Known(3), Dim.Known(4))
    val s2 = Shape(Dim.Known(4), Dim.Known(3))
    assertNotEquals(s1, s2)
  }

  test("Shape: scalar has rank 0 and isScalar = true") {
    assertEquals(Shape.scalar.rank, 0)
    assert(Shape.scalar.isScalar)
  }

  test("Shape: mixed dims rank") {
    val s = Shape(Dim.Known(2), Dim.Sym("batch"), Dim.Unknown)
    assertEquals(s.rank, 3)
    assert(!s.isScalar)
  }

  // ─── property: rank ≥ 0 for every node ─────────────────────────────────────

  test("property: every node tpe.shape.rank >= 0") {
    val tensor3x4 = TType(DType.F64, Shape(Dim.Known(3), Dim.Known(4)))
    val boolTensor3x4 = TType(DType.Bool, Shape(Dim.Known(3), Dim.Known(4)))
    val tensor4 = TType(DType.F64, Shape(Dim.Known(4)))
    val nodes = Vector[TensorExpr](
      TensorExpr.Param("x", tensor3x4), // 0
      TensorExpr.Unary(UnaryOp.Sin, NodeId(0), tensor3x4), // 1
      TensorExpr.Binary(BinaryOp.Mul, NodeId(0), NodeId(1), tensor3x4), // 2
      TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), tensor4), // 3
      TensorExpr.Const(0.0, f64Scalar), // 4
      TensorExpr.Cast(DType.F32, NodeId(0), TType(DType.F32, tensor3x4.shape)), // 5
      TensorExpr.BCast(NodeId(4), tensor3x4.shape, tensor3x4), // 6
      TensorExpr.Param("mask", boolTensor3x4), // 7
      TensorExpr.Where(NodeId(7), NodeId(0), NodeId(1), tensor3x4) // 8
    )
    nodes.foreach { node =>
      assert(node.tpe.shape.rank >= 0, s"negative rank in node: $node")
    }
  }

  // ─── property: Reduce axes within rank ──────────────────────────────────────

  test("property: Reduce axes are within input rank") {
    val tensor3x4 = TType(DType.F64, Shape(Dim.Known(3), Dim.Known(4)))
    val tensor4 = TType(DType.F64, Shape(Dim.Known(4)))
    val inputRank = tensor3x4.shape.rank
    val reduce = TensorExpr.Reduce(ReduceOp.Sum, NodeId(0), Vector(0), tensor4)
    reduce.axes.foreach { ax =>
      assert(ax >= 0 && ax < inputRank, s"axis $ax out of range for rank $inputRank")
    }
  }

  // ─── NodeId ─────────────────────────────────────────────────────────────────

  test("NodeId: equality and unwrapping") {
    val id = NodeId(42)
    assertEquals(id.i, 42)
    assertEquals(id, NodeId(42))
    assertNotEquals(id, NodeId(0))
  }

  // ─── TType ──────────────────────────────────────────────────────────────────

  test("TType: dtype and shape accessible") {
    val tpe = TType(DType.F32, Shape(Dim.Known(10)))
    assertEquals(tpe.dtype, DType.F32)
    assertEquals(tpe.shape.rank, 1)
  }

  test("TType: toString contains dtype and shape") {
    val tpe = TType(DType.F64, Shape(Dim.Known(3), Dim.Known(4)))
    val s = tpe.toString
    assert(s.contains("f64"), s"missing dtype in toString: $s")
    assert(s.contains("3"), s"missing dim 3 in toString: $s")
    assert(s.contains("4"), s"missing dim 4 in toString: $s")
  }

  // ─── all ops ────────────────────────────────────────────────────────────────

  test("UnaryOp: all cases instantiable") {
    val ops = Vector(
      UnaryOp.Neg,
      UnaryOp.Sin,
      UnaryOp.Cos,
      UnaryOp.Tan,
      UnaryOp.Exp,
      UnaryOp.Log,
      UnaryOp.Sqrt,
      UnaryOp.Abs,
      UnaryOp.Not,
      UnaryOp.Reciprocal
    )
    assertEquals(ops.size, 10)
  }

  test("BinaryOp: all cases instantiable") {
    val ops = Vector(
      BinaryOp.Add,
      BinaryOp.Sub,
      BinaryOp.Mul,
      BinaryOp.Div,
      BinaryOp.Pow,
      BinaryOp.Min,
      BinaryOp.Max,
      BinaryOp.Eq,
      BinaryOp.Neq,
      BinaryOp.Lt,
      BinaryOp.Lte,
      BinaryOp.Gt,
      BinaryOp.Gte,
      BinaryOp.And,
      BinaryOp.Or
    )
    assertEquals(ops.size, 15)
  }

  test("ReduceOp: all cases instantiable") {
    val ops = Vector(
      ReduceOp.Sum,
      ReduceOp.Product,
      ReduceOp.Min,
      ReduceOp.Max,
      ReduceOp.All,
      ReduceOp.Any,
      ReduceOp.ArgMax,
      ReduceOp.ArgMin
    )
    assertEquals(ops.size, 8)
  }

end IrPhase1Test
