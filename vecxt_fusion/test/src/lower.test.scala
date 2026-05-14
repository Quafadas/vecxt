package vecxt.fusion

import munit.FunSuite

class LowerPhase4Test extends FunSuite:

  // ─── helpers ────────────────────────────────────────────────────────────────

  val f64Scalar = TType(DType.F64, Shape.scalar)
  val f64_3x4   = TType(DType.F64, Shape(Dim.Known(3), Dim.Known(4)))

  /** Scalar env: x, y, z are f64 scalars. */
  val scalarEnv: TypeEnv = Map("x" -> f64Scalar, "y" -> f64Scalar, "z" -> f64Scalar)

  /** Tensor env: x and y are f64[3,4]. */
  val tensorEnv: TypeEnv = Map("x" -> f64_3x4, "y" -> f64_3x4)

  import MathExpr.*

  def assertRight[A](result: Either[?, A]): A = result match
    case Right(a) => a
    case Left(e)  => fail(s"Expected Right but got Left($e)")

  // ══════════════════════════════════════════════════════════════════════════
  // Atoms
  // ══════════════════════════════════════════════════════════════════════════

  test("lower: Number(3.14) → Const(3.14, f64[])") {
    val g = assertRight(Lower.lower(Number(3.14), Map.empty))
    assertEquals(g.size, 1)
    val c = g(g.output).asInstanceOf[TensorExpr.Const]
    assertEquals(c.value, 3.14)
    assertEquals(c.tpe, f64Scalar)
  }

  test("lower: Symbol(x) → Param(x, env type)") {
    val g = assertRight(Lower.lower(Symbol("x"), scalarEnv))
    assertEquals(g.size, 1)
    val p = g(g.output).asInstanceOf[TensorExpr.Param]
    assertEquals(p.name, "x")
    assertEquals(p.tpe, f64Scalar)
  }

  test("lower: Symbol(x) not in env → UnknownSymbol error") {
    Lower.lower(Symbol("missing"), Map.empty) match
      case Left(e: LowerError.UnknownSymbol) => assertEquals(e.name, "missing")
      case other                             => fail(s"expected UnknownSymbol, got $other")
  }

  test("lower: Constant(pi) resolved from env → Param(pi)") {
    val env = scalarEnv + ("pi" -> f64Scalar)
    val g   = assertRight(Lower.lower(Constant("pi"), env))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Param].name, "pi")
  }

  test("lower: Constant(pi) not in env → UnknownSymbol error") {
    Lower.lower(Constant("pi"), Map.empty) match
      case Left(_: LowerError.UnknownSymbol) => ()
      case other                             => fail(s"expected UnknownSymbol, got $other")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Arithmetic binary operators
  // ══════════════════════════════════════════════════════════════════════════

  test("lower: x + y → Binary(Add, Param(x), Param(y))") {
    val g   = assertRight(Lower.lower(Add(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(g.size, 3) // x, y, Binary
    val add = g(g.output).asInstanceOf[TensorExpr.Binary]
    assertEquals(add.op, BinaryOp.Add)
    assertEquals(g(add.a).asInstanceOf[TensorExpr.Param].name, "x")
    assertEquals(g(add.b).asInstanceOf[TensorExpr.Param].name, "y")
    assertEquals(add.tpe, f64Scalar)
  }

  test("lower: x - y → Binary(Sub, ...)") {
    val g = assertRight(Lower.lower(Sub(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Sub)
  }

  test("lower: x * y → Binary(Mul, ...)") {
    val g = assertRight(Lower.lower(Mul(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Mul)
  }

  test("lower: x / y → Binary(Div, ...)") {
    val g = assertRight(Lower.lower(Div(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Div)
  }

  test("lower: Pow(x, y) → Binary(Pow, ...)") {
    val g = assertRight(Lower.lower(Pow(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Pow)
  }

  test("lower: Neg(x) → Unary(Neg, Param(x))") {
    val g   = assertRight(Lower.lower(Neg(Symbol("x")), scalarEnv))
    assertEquals(g.size, 2)
    val neg = g(g.output).asInstanceOf[TensorExpr.Unary]
    assertEquals(neg.op, UnaryOp.Neg)
    assertEquals(g(neg.a).asInstanceOf[TensorExpr.Param].name, "x")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Structural equivalences (transparent wrappers)
  // ══════════════════════════════════════════════════════════════════════════

  test("lower: Fraction(x, y) → Binary(Div, ...)") {
    val g = assertRight(Lower.lower(Fraction(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Div)
  }

  test("lower: Group(x + y) → Binary(Add, ...) (transparent)") {
    val g = assertRight(Lower.lower(Group(Add(Symbol("x"), Symbol("y"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Add)
  }

  test("lower: BracketGroup(x + y) → Binary(Add, ...) (transparent)") {
    val g = assertRight(
      Lower.lower(BracketGroup("(", ")", Add(Symbol("x"), Symbol("y"))), scalarEnv)
    )
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Add)
  }

  test("lower: Color(red, x + y) → Binary(Add, ...) (transparent)") {
    val g = assertRight(Lower.lower(Color("red", Add(Symbol("x"), Symbol("y"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Add)
  }

  test("lower: Style(bold, x + y) → Binary(Add, ...) (transparent)") {
    val g = assertRight(Lower.lower(Style("bold", Add(Symbol("x"), Symbol("y"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Binary].op, BinaryOp.Add)
  }

  test("lower: Enclose(notation, x) → Param(x) (transparent)") {
    val g = assertRight(Lower.lower(Enclose("box", Symbol("x")), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Param].name, "x")
  }

  test("lower: Root(None, x) → Unary(Sqrt, ...)") {
    val g   = assertRight(Lower.lower(Root(None, Symbol("x")), scalarEnv))
    val sq  = g(g.output).asInstanceOf[TensorExpr.Unary]
    assertEquals(sq.op, UnaryOp.Sqrt)
    assertEquals(g(sq.a).asInstanceOf[TensorExpr.Param].name, "x")
  }

  test("lower: Root(Some(deg), x) → UnsupportedNode error") {
    Lower.lower(Root(Some(Number(3.0)), Symbol("x")), scalarEnv) match
      case Left(_: LowerError.UnsupportedNode) => ()
      case other                               => fail(s"expected UnsupportedNode, got $other")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Superscript
  // ══════════════════════════════════════════════════════════════════════════

  test("lower: Superscript(x, Number(2)) → Binary(Pow, Param(x), Const(2))") {
    val g   = assertRight(Lower.lower(Superscript(Symbol("x"), Number(2.0)), scalarEnv))
    val pow = g(g.output).asInstanceOf[TensorExpr.Binary]
    assertEquals(pow.op, BinaryOp.Pow)
    assertEquals(g(pow.a).asInstanceOf[TensorExpr.Param].name, "x")
    assertEquals(g(pow.b).asInstanceOf[TensorExpr.Const].value, 2.0)
  }

  test("lower: Superscript(x, Symbol(y)) → NonSemanticSuperscript error") {
    Lower.lower(Superscript(Symbol("x"), Symbol("y")), scalarEnv) match
      case Left(_: LowerError.NonSemanticSuperscript) => ()
      case other => fail(s"expected NonSemanticSuperscript, got $other")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Function calls
  // ══════════════════════════════════════════════════════════════════════════

  test("lower: FunctionCall(sin, [x]) → Unary(Sin, Param(x))") {
    val g   = assertRight(Lower.lower(FunctionCall("sin", List(Symbol("x"))), scalarEnv))
    val sin = g(g.output).asInstanceOf[TensorExpr.Unary]
    assertEquals(sin.op, UnaryOp.Sin)
    assertEquals(g(sin.a).asInstanceOf[TensorExpr.Param].name, "x")
  }

  test("lower: FunctionCall(cos, [x]) → Unary(Cos, ...)") {
    val g = assertRight(Lower.lower(FunctionCall("cos", List(Symbol("x"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Unary].op, UnaryOp.Cos)
  }

  test("lower: FunctionCall(exp, [x]) → Unary(Exp, ...)") {
    val g = assertRight(Lower.lower(FunctionCall("exp", List(Symbol("x"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Unary].op, UnaryOp.Exp)
  }

  test("lower: FunctionCall(log, [x]) → Unary(Log, ...)") {
    val g = assertRight(Lower.lower(FunctionCall("log", List(Symbol("x"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Unary].op, UnaryOp.Log)
  }

  test("lower: FunctionCall(ln, [x]) → Unary(Log, ...) (ln alias)") {
    val g = assertRight(Lower.lower(FunctionCall("ln", List(Symbol("x"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Unary].op, UnaryOp.Log)
  }

  test("lower: FunctionCall(sqrt, [x]) → Unary(Sqrt, ...)") {
    val g = assertRight(Lower.lower(FunctionCall("sqrt", List(Symbol("x"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Unary].op, UnaryOp.Sqrt)
  }

  test("lower: FunctionCall(abs, [x]) → Unary(Abs, ...)") {
    val g = assertRight(Lower.lower(FunctionCall("abs", List(Symbol("x"))), scalarEnv))
    assertEquals(g(g.output).asInstanceOf[TensorExpr.Unary].op, UnaryOp.Abs)
  }

  test("lower: FunctionCall(tanh, [x]) → UnknownFunction error") {
    Lower.lower(FunctionCall("tanh", List(Symbol("x"))), scalarEnv) match
      case Left(e: LowerError.UnknownFunction) => assertEquals(e.name, "tanh")
      case other                               => fail(s"expected UnknownFunction, got $other")
  }

  test("lower: FunctionCall(sin, [x, y]) wrong arity → UnknownFunction error") {
    Lower.lower(FunctionCall("sin", List(Symbol("x"), Symbol("y"))), scalarEnv) match
      case Left(_: LowerError.UnknownFunction) => ()
      case other                               => fail(s"expected UnknownFunction, got $other")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Unsupported binders
  // ══════════════════════════════════════════════════════════════════════════

  test("lower: Sum(...) → UnsupportedBinder error") {
    Lower.lower(Sum(Symbol("i"), Number(0.0), Number(10.0), Symbol("x")), scalarEnv) match
      case Left(_: LowerError.UnsupportedBinder) => ()
      case other                                 => fail(s"expected UnsupportedBinder, got $other")
  }

  test("lower: Integral(...) → UnsupportedBinder error") {
    Lower.lower(Integral(Symbol("x"), Number(0.0), Number(1.0), Symbol("x")), scalarEnv) match
      case Left(_: LowerError.UnsupportedBinder) => ()
      case other                                 => fail(s"expected UnsupportedBinder, got $other")
  }

  test("lower: Subscript(...) → UnsupportedBinder error") {
    Lower.lower(Subscript(Symbol("x"), Number(0.0)), scalarEnv) match
      case Left(_: LowerError.UnsupportedBinder) => ()
      case other                                  => fail(s"expected UnsupportedBinder, got $other")
  }

  // ══════════════════════════════════════════════════════════════════════════
  // Hash-consing
  // ══════════════════════════════════════════════════════════════════════════

  test("lower: x + x shares Param node (hash-consing)") {
    val g   = assertRight(Lower.lower(Add(Symbol("x"), Symbol("x")), scalarEnv))
    // Param(x) + Param(x) → 2 nodes: one Param, one Binary
    assertEquals(g.size, 2)
    val add = g(g.output).asInstanceOf[TensorExpr.Binary]
    assertEquals(add.a, add.b)
  }

  test("lower: (x + y) + (x + y) shares the Add subexpression") {
    val inner = Add(Symbol("x"), Symbol("y"))
    val g     = assertRight(Lower.lower(Add(inner, inner), scalarEnv))
    // Param(x), Param(y), Binary(Add,0,1), Binary(Add,2,2)
    assertEquals(g.size, 4)
    val outer = g(g.output).asInstanceOf[TensorExpr.Binary]
    assertEquals(outer.a, outer.b) // same shared Add subgraph
  }

  // ══════════════════════════════════════════════════════════════════════════
  // TypeCheck round-trip (lowered graph passes Phase-3 checker)
  // ══════════════════════════════════════════════════════════════════════════

  test("lower + TypeCheck: x + y (scalar) passes TypeCheck") {
    val g = assertRight(Lower.lower(Add(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("lower + TypeCheck: sin(x) (scalar) passes TypeCheck") {
    val g = assertRight(Lower.lower(FunctionCall("sin", List(Symbol("x"))), scalarEnv))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("lower + TypeCheck: Fraction(x, y) (scalar) passes TypeCheck") {
    val g = assertRight(Lower.lower(Fraction(Symbol("x"), Symbol("y")), scalarEnv))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("lower + TypeCheck: tensor x + y passes TypeCheck") {
    val g = assertRight(Lower.lower(Add(Symbol("x"), Symbol("y")), tensorEnv))
    assertEquals(TypeCheck.infer(g), Right(g))
  }

  test("lower + TypeCheck: deeply nested Color/Group/BracketGroup passes TypeCheck") {
    val expr = Color("blue", Group(BracketGroup("(", ")", Neg(Symbol("x")))))
    val g    = assertRight(Lower.lower(expr, scalarEnv))
    val neg  = g(g.output).asInstanceOf[TensorExpr.Unary]
    assertEquals(neg.op, UnaryOp.Neg)
    assertEquals(g(neg.a).asInstanceOf[TensorExpr.Param].name, "x")
    assertEquals(TypeCheck.infer(g), Right(g))
  }

end LowerPhase4Test
